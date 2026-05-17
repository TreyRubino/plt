(**
@file   gc.ml
@brief  Mark-and-sweep garbage collector for the raw word slab and the
        parallel string table. The mark phase uses an explicit worklist to
        avoid OCaml call-stack overflow on deep object graphs. It traces
        all VPtr and StrIdx references reachable from the operand stack,
        active frame locals, and frame self pointers, plus all constant
        strings that are permanently live. The sweep phase scans the slab
        linearly, coalescing adjacent dead blocks into the free list, and
        then sweeps the string table.
@author Trey Rubino
@date   03/28/2026
*)

open Runtime

(** @brief Evaluates an OCaml value to identify heap references. If the value 
           is a VPtr, its corresponding slab offset is added to the mark-phase 
           worklist for further tracing.
    @param worklist The queue of slab offsets pending reachability analysis.
    @param v The COOL value to inspect for pointers. *)
let seed_value (worklist : int Queue.t) (v : value) : unit =
  match v with
  | VPtr p -> Queue.push p worklist
  | _      -> ()

(** @brief Iterates through the field slots of a specific slab object. It identifies 
           child pointers (VPtr) to add to the worklist and immediately marks 
           any string table indices (StrIdx) found within the object's data.
    @param worklist The queue of slab offsets pending reachability analysis.
    @param st The global VM state.
    @param p The word offset of the object being scanned. *)
let scan_fields (worklist : int Queue.t) (st : vm_state) (p : int) : unit =
  let size = Nativeint.to_int st.heap.slab.{p + 1} in
  for i = 2 to size - 1 do
    let w = st.heap.slab.{p + i} in
    if w <> 0n then
      if Heap.is_ptr w then
        Queue.push (Nativeint.to_int (Nativeint.shift_right_logical w 3)) worklist
      else if Heap.is_str_idx w then
        Strings.mark st.strings (Heap.decode_str_idx w)
  done

(** @brief Initiates the mark phase by identifying all root references from 
           the operand stack, frame locals, self pointers, and the permanent 
           string literal pool. It then drains the worklist to mark the entire 
           transitive closure of reachable objects.
    @param st The global VM state. *)
let mark_roots (st : vm_state) : unit =
  let worklist = Queue.create () in
  List.iter (seed_value worklist) st.stack;

  (* active frames: self pointer and all locals *)
  List.iter (fun f ->
    Queue.push f.self_ptr worklist;
    Array.iter (seed_value worklist) f.locals
  ) st.frames;

  (* constant table strings are permanently live for the program lifetime *)
  Array.iter (function
    | Ir.LString s ->
      (match Hashtbl.find_opt st.strings.tbl s with
      | Some i -> Strings.mark st.strings i
      | None   -> ())
    | _ -> ()
  ) st.ir.consts;

  (* drain worklist *)
  while not (Queue.is_empty worklist) do
    let p = Queue.pop worklist in
    if not (Heap.is_marked st.heap p) then (
      Heap.mark_obj st.heap p;
      scan_fields worklist st p
    )
  done

(** @brief Performs a linear scan of the entire slab to reclaim memory. It 
           identifies unmarked objects, converts them into free nodes, and 
           performs immediate coalescing by merging adjacent dead blocks 
           into single, larger entries in the free list.
    @param st The global VM state. *)
let sweep_heap (st : vm_state) : unit =
  let h = st.heap in
  h.free <- [];
  let i = ref 0 in
  while !i < h.next do
    let header = h.slab.{!i} in
    let size   = Nativeint.to_int h.slab.{!i + 1} in

    if Heap.hdr_is_free header then (
      (* free node: coalesce into free list *)
      (match h.free with
      | (prev_off, prev_sz) :: rest when prev_off + prev_sz = !i ->
        let merged = prev_sz + size in
        h.slab.{prev_off + 1} <- Nativeint.of_int merged;
        h.free <- (prev_off, merged) :: rest
      | _ ->
        h.free <- (!i, size) :: h.free)

    ) else if not (Heap.hdr_is_marked header) then (
      (* dead live object: deduct its words and add to free list *)
      h.n_live_words <- h.n_live_words - size;
      h.slab.{!i} <- Heap.free_node_hdr;
      (match h.free with
      | (prev_off, prev_sz) :: rest when prev_off + prev_sz = !i ->
        let merged = prev_sz + size in
        h.slab.{prev_off + 1} <- Nativeint.of_int merged;
        h.free <- (prev_off, merged) :: rest
      | _ ->
        h.free <- (!i, size) :: h.free)

    ) else (
      (* live: clear mark bit for next cycle *)
      h.slab.{!i} <- Heap.hdr_clear_mark header
    );

    i := !i + size
  done
  
(** @brief Entry point for a garbage collection cycle. It 
           coordinates the marking of the heap and string table, followed 
           by the sweeping of the slab and the parallel string array.
    @param st The global VM state to be collected. *)
let collect (st : vm_state) : unit =
  mark_roots st;
  sweep_heap st;
  Strings.sweep st.strings
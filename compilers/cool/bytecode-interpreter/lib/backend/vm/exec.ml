(**
@file   exec.ml
@brief  Core execution engine that interprets bytecode instructions over
        the raw word slab. Integers and booleans are unboxed; no allocation
        occurs for arithmetic, comparison, or boolean operations. Field
        access, dispatch, and new-object instructions go through the slab
        allocator and heap accessors. The GC is triggered implicitly by
        Alloc.allocate_object whenever the bump pointer reaches threshold of
        75% heap allocation.
@author Trey Rubino
@date   11/30/2025
*)

open Runtime
open Bytecode
open Builtin
open Stack
open Alloc
open Ir
open Error

(** @brief Scans the line map of the currently executing method to determine 
           the source code line number corresponding to the current program counter.
    @param f The active call frame being executed.
    @return A string representation of the line number, or "unknown" if not found. *)
let get_line_number (f : frame) : string =
  let pc = f.pc - 1 in
  let m  = f.method_info in
  let best_line = ref 0 in
  Array.iter (fun (start_pc, line) ->
    if pc >= start_pc then best_line := line
  ) m.line_map;
  if !best_line = 0 then "unknown" else string_of_int !best_line

let to_int32 (x : int) : int =
  Int32.to_int (Int32.of_int x)

let expect_int (v : value) (f : frame) : int =
  match v with
  | VInt i -> i
  | VVoid  -> Error.vm (get_line_number f) "expected Int (void)"
  | _      -> Error.vm (get_line_number f) "expected Int"

let expect_bool (v : value) (f : frame) : bool =
  match v with
  | VBool b -> b
  | VVoid   -> Error.vm (get_line_number f) "expected Bool (void)"
  | _       -> Error.vm (get_line_number f) "expected Bool"

let expect_ptr (v : value) (f : frame) (ctx : string) : int =
  match v with
  | VPtr p -> p
  | VVoid  -> Error.vm (get_line_number f) "dispatch on void" ctx
  | _      -> Error.vm (get_line_number f) "%s: expected object" ctx

let const_of_lit (st : vm_state) : Ir.literal -> value = function
  | LInt i    -> VInt i
  | LBool b   -> VBool b
  | LString s -> VPtr (Alloc.allocate_string st s)
  | LVoid     -> VVoid

(** @brief The central interpreter loop of the Virtual Machine. It fetches 
           the next bytecode instruction, performs the associated operation 
           (arithmetic, object access, or stack manipulation), and manages 
           the flow of execution across call frames.
    @param st The global VM state, including the heap, stacks, and IR.
    @return The final COOL value produced by the program upon completion. *)
let run (st : vm_state) : value =
  let rec loop () =
    let frame =
      match st.frames with
      | f :: _ -> f
      | []     -> Error.vm "0" "run called with no active frame"
    in
    let code = frame.method_info.code in
    if frame.pc < 0 || frame.pc >= Array.length code then
      Error.vm "0" "pc out of bounds %d" frame.pc;
    let instr = code.(frame.pc) in
    frame.pc <- frame.pc + 1;
    match instr.op with

    | OP_POP ->
      ignore (Stack.pop_val st);
      loop ()

    | OP_CONST ->
      (match instr.arg with
      | IntArg idx ->
        Stack.push_val st (const_of_lit st st.ir.consts.(idx));
        loop ()
      | _ ->
        Error.vm (get_line_number frame) "CONST missing IntArg")

    | OP_TRUE ->
      Stack.push_val st (VBool true);
      loop ()

    | OP_FALSE ->
      Stack.push_val st (VBool false);
      loop ()

    | OP_VOID ->
      Stack.push_val st VVoid;
      loop ()

    | OP_GET_LOCAL ->
      (match instr.arg with
      | IntArg slot ->
        Stack.push_val st (Stack.get_local st slot);
        loop ()
      | _ ->
        Error.vm (get_line_number frame) "GET_LOCAL missing IntArg")

    | OP_SET_LOCAL ->
      (match instr.arg with
      | IntArg slot ->
        Stack.set_local st slot (Stack.pop_val st);
        loop ()
      | _ ->
        Error.vm (get_line_number frame) "SET_LOCAL missing IntArg")

    | OP_GET_SELF ->
      Stack.push_val st (VPtr frame.self_ptr);
      loop ()

    | OP_GET_ATTR ->
      (match instr.arg with
      | IntArg off ->
        let p = expect_ptr (Stack.pop_val st) frame "GET_ATTR" in
        Stack.push_val st (Heap.get_field st.heap p (off - 1));
        loop ()
      | _ ->
        Error.vm (get_line_number frame) "GET_ATTR missing IntArg")

    | OP_SET_ATTR ->
      (match instr.arg with
      | IntArg off ->
        let v = Stack.pop_val st in
        let p = expect_ptr (Stack.pop_val st) frame "SET_ATTR" in
        Heap.set_field st.heap p (off - 1) v;
        loop ()
      | _ ->
        Error.vm (get_line_number frame) "SET_ATTR missing IntArg")

    | OP_NEW ->
      (match instr.arg with
      | IntArg cid ->
        let p = Alloc.allocate_object st cid in
        Stack.push_val st (VPtr p);
        loop ()
      | _ ->
        Error.vm (get_line_number frame) "NEW missing IntArg")

    | OP_NEW_SELF_TYPE ->
      let cid       = Heap.class_id st.heap frame.self_ptr in
      let cls       = st.ir.classes.(cid) in
      let init_name = "__init_" ^ cls.name in
      let rec find_init i =
        if i >= Array.length st.ir.methods then
          Error.vm "0" "missing constructor for %s" cls.name
        else if st.ir.methods.(i).name = init_name then i
        else find_init (i + 1)
      in
      let init_mid = find_init 0 in
      let new_p    = Alloc.allocate_object st cid in
      Stack.push_frame st new_p init_mid [];
      loop ()

    | OP_JUMP ->
      (match instr.arg with
      | IntArg offset | OffsetArg offset ->
        frame.pc <- frame.pc + offset;
        loop ()
      | _ -> Error.vm (get_line_number frame) "JUMP missing offset")

    | OP_JUMP_IF_FALSE ->
      (match instr.arg with
      | IntArg offset | OffsetArg offset ->
        let b = expect_bool (Stack.pop_val st) frame in
        if not b then frame.pc <- frame.pc + offset;
        loop ()
      | _ -> Error.vm (get_line_number frame) "JUMP_IF_FALSE missing offset")

    | OP_CASE_ABORT ->
      Error.vm (get_line_number frame) "case on void"

    | OP_ADD ->
      let rhs = expect_int (Stack.pop_val st) frame in
      let lhs = expect_int (Stack.pop_val st) frame in
      Stack.push_val st (VInt (to_int32 (lhs + rhs)));
      loop ()

    | OP_SUB ->
      let rhs = expect_int (Stack.pop_val st) frame in
      let lhs = expect_int (Stack.pop_val st) frame in
      Stack.push_val st (VInt (to_int32 (lhs - rhs)));
      loop ()

    | OP_MUL ->
      let rhs = expect_int (Stack.pop_val st) frame in
      let lhs = expect_int (Stack.pop_val st) frame in
      Stack.push_val st (VInt (to_int32 (lhs * rhs)));
      loop ()

    | OP_DIV ->
      let rhs = expect_int (Stack.pop_val st) frame in
      let lhs = expect_int (Stack.pop_val st) frame in
      if rhs = 0 then Error.vm (get_line_number frame) "division by zero";
      Stack.push_val st (VInt (to_int32 (lhs / rhs)));
      loop ()

    | OP_NEG ->
      let i = expect_int (Stack.pop_val st) frame in
      Stack.push_val st (VInt (to_int32 (-i)));
      loop ()

    | OP_NOT ->
      let b = expect_bool (Stack.pop_val st) frame in
      Stack.push_val st (VBool (not b));
      loop ()

    | OP_EQUAL ->
      let rhs = Stack.pop_val st in
      let lhs = Stack.pop_val st in
      let eq =
        match lhs, rhs with
        | VVoid,   VVoid   -> true
        | VVoid,   _       -> false
        | _,       VVoid   -> false
        | VInt a,  VInt b  -> a = b
        | VBool a, VBool b -> a = b
        | VPtr a,  VPtr b  ->
          let cid = Heap.class_id st.heap a in
          let cls = st.ir.classes.(cid) in
          if cls.name = "String" then
            (* string equality is structural *)
            let sa = st.strings.data.(Heap.get_str_field st.heap a) in
            let sb = st.strings.data.(Heap.get_str_field st.heap b) in
            sa = sb
          else
            (* object equality is pointer equality *)
            a = b
        | _, _ -> false
      in
      Stack.push_val st (VBool eq);
      loop ()

    | OP_LESS ->
      let rhs = expect_int (Stack.pop_val st) frame in
      let lhs = expect_int (Stack.pop_val st) frame in
      Stack.push_val st (VBool (lhs < rhs));
      loop ()

    | OP_LESS_EQUAL ->
      let rhs = expect_int (Stack.pop_val st) frame in
      let lhs = expect_int (Stack.pop_val st) frame in
      Stack.push_val st (VBool (lhs <= rhs));
      loop ()

    | OP_ISVOID ->
      let v = Stack.pop_val st in
      Stack.push_val st (VBool (v = VVoid));
      loop ()

    | OP_IS_SUBTYPE ->
      (match instr.arg with
      | IntArg target_cid ->
        let p = expect_ptr (Stack.pop_val st) frame "IS_SUBTYPE" in
        let actual_cid = Heap.class_id st.heap p in
        let rec is_sub cid =
          if cid = target_cid then true
          else if cid < 0 then false
          else is_sub st.ir.classes.(cid).parent_id
        in
        Stack.push_val st (VBool (is_sub actual_cid));
        loop ()
      | _ -> Error.vm (get_line_number frame) "IS_SUBTYPE missing IntArg")

    | OP_CALL ->
      (match instr.arg with
      | IntArg mid ->
        let m    = st.ir.methods.(mid) in
        let recv = expect_ptr (Stack.pop_val st) frame "CALL" in
        let rec pop_args acc n =
          if n = 0 then acc
          else pop_args (Stack.pop_val st :: acc) (n - 1)
        in
        let args = pop_args [] m.n_formals in
        Stack.push_frame st recv mid args;
        loop ()
      | _ ->
        Error.vm (get_line_number frame) "CALL missing IntArg")

    | OP_DISPATCH ->
      (match instr.arg with
      | IntArg slot ->
        let recv = expect_ptr (Stack.pop_val st) frame "DISPATCH" in
        let cid  = Heap.class_id st.heap recv in
        let cls  = st.ir.classes.(cid) in
        let mid  = cls.dispatch.(slot) in
        let m    = st.ir.methods.(mid) in
        let rec pop_args acc n =
          if n = 0 then acc
          else pop_args (Stack.pop_val st :: acc) (n - 1)
        in
        let args = pop_args [] m.n_formals in
        Stack.push_frame st recv mid args;
        loop ()
      | _ -> Error.vm (get_line_number frame) "DISPATCH missing IntArg")

    | OP_STATIC_DISPATCH ->
      (match instr.arg with
      | IntArg mid ->
        let recv = expect_ptr (Stack.pop_val st) frame "STATIC_DISPATCH" in
        let m    = st.ir.methods.(mid) in
        let rec pop_args acc n =
          if n = 0 then acc
          else pop_args (Stack.pop_val st :: acc) (n - 1)
        in
        let args = pop_args [] m.n_formals in
        Stack.push_frame st recv mid args;
        loop ()
      | _ -> Error.vm (get_line_number frame) "STATIC_DISPATCH missing IntArg")

    | OP_RETURN ->
      let frame =
        match st.frames with
        | f :: _ -> f
        | []     -> Error.vm (get_line_number frame) "RETURN with no frame"
      in
      (match Builtin.maybe_handle_builtin st frame with
      | Some v ->
        ignore (Stack.pop_frame st);
        Stack.push_val st v;
        if st.frames = [] then v else loop ()
      | None ->
        let ret = Stack.pop_val st in
        match st.frames with
        | _current :: caller :: rest ->
          ignore (Stack.pop_frame st);
          st.frames <- caller :: rest;
          Stack.push_val st ret;
          loop ()
        | [_] ->
          ignore (Stack.pop_frame st);
          Stack.push_val st ret;
          ret
        | [] ->
          Error.vm (get_line_number frame) "RETURN with no frame")
  in
  loop ()
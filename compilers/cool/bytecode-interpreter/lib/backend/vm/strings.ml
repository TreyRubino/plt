(**
@file   strings.ml
@brief  Parallel string table for COOL runtime strings. String bytes live
        in an OCaml string array managed here. The slab carries only an
        integer slot index per String object. intern deduplicates content
        via a hash table. The collector marks live slots; sweep reclaims
        unreachable ones back to the free list.
@author Trey Rubino
@date   03/28/2026
*)

open Runtime
open Error

(** @brief Interns a string into the parallel table, returning its unique slot index. 
           If the string content already exists, it returns the cached index; 
           otherwise, it allocates a new slot from the free list and updates 
           the interning hash table for future deduplication.
    @param st The global strings state containing the data array and hash table.
    @param s The OCaml string content to be interned.
    @return The unique integer index (ticket) representing this string. *)
let intern (st : strings) (s : string) : int =
  match Hashtbl.find_opt st.tbl s with
  | Some i -> i
  | None ->
    (match st.free with
    | [] ->
      Error.vm "0" "string table exhausted"
    | i :: rest ->
      st.data.(i)  <- s;
      st.live.(i)  <- false;
      st.free      <- rest;
      st.n_live    <- st.n_live + 1;
      Hashtbl.add st.tbl s i;
      i)

let mark (st : strings) (i : int) : unit =
  st.live.(i) <- true

(** @brief Performs a linear scan of the string table to reclaim unmarked slots. 
           It clears the string data, removes the entry from the interning hash table, 
           and returns the index to the free list. 
    @param st The global strings state to be swept. *)
let sweep (st : strings) : unit =
  for i = 0 to st.capacity - 1 do
    if st.live.(i) then
      st.live.(i) <- false
    else if Hashtbl.find_opt st.tbl st.data.(i) = Some i then (
      Hashtbl.remove st.tbl st.data.(i);
      st.data.(i)  <- "";
      st.free      <- i :: st.free;
      st.n_live    <- st.n_live - 1
    )
  done
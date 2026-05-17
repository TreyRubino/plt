(**
@file   heap.ml
@brief  Raw word slab allocator for COOL runtime objects. Manages a
        Bigarray.Array1 of nativeint words that lives outside OCaml's GC.
        Allocation uses a bump pointer with a first fit free list for
        reclaimed blocks. Provides field encode/decode between the OCaml
        value type and the slab's tagged word representation, and exposes
        the header manipulation primitives used by the collector.
@author Trey Rubino
@date   03/28/2026
*)

open Bigarray
open Runtime
open Error

let tag_int     = 1n
let tag_bool    = 2n
let tag_ptr     = 3n
let tag_str_idx = 4n
let tag_mask    = 7n

let free_node_hdr = 2n    (* header for free blocks, bit 1 (Free) set *)

(** @brief Encodes a COOL value into a 64-bit tagged word by shifting the value left by 3 bits 
           and injecting the type tag into the least significant bits.
    @param v The OCaml variant value (VInt, VBool, VPtr) to encode.
    @return A tagged nativeint representation for slab storage. *)
let encode (v : value) : nativeint =
  match v with
  | VVoid   -> 0n
  | VInt n  -> Nativeint.logor (Nativeint.shift_left (Nativeint.of_int n) 3) tag_int
  | VBool b -> Nativeint.logor (Nativeint.shift_left (Nativeint.of_int (if b then 1 else 0)) 3) tag_bool
  | VPtr p  -> Nativeint.logor (Nativeint.shift_left (Nativeint.of_int p) 3) tag_ptr

(** @brief Decodes a tagged nativeint from the slab by masking the lower 3 bits to identify 
           the type tag and shifting the remaining bits right to recover the original value.
    @param w The 64-bit tagged word retrieved from the Bigarray slab.
    @return The corresponding OCaml value variant. *)
let decode (w : nativeint) : value =
  if w = 0n then VVoid
  else
    let tag = Nativeint.to_int (Nativeint.logand w tag_mask) in (* using logand on the tag mask deletes all but the last 3 bits *)
    match tag with
    | 1 -> VInt  (Nativeint.to_int (Nativeint.shift_right w 3))
    | 2 -> VBool (Nativeint.to_int (Nativeint.logand w 8n) <> 0)
    | 3 -> VPtr  (Nativeint.to_int (Nativeint.shift_right_logical w 3))
    | _ -> VVoid

(** @brief Encodes a raw integer index for the parallel string table into a tagged word 
           using the specific StrIdx tag 4.
    @param i The integer index representing a slot in the string table.
    @return A nativeint word containing the shifted index and tag 4. *)
let encode_str_idx (i : int) : nativeint =
  Nativeint.logor (Nativeint.shift_left (Nativeint.of_int i) 3) tag_str_idx

(** @brief Extracts a string table index from a tagged word by performing a logical 
           right shift of 3 bits, discarding the StrIdx tag.
    @param w The tagged word stored in a String object's field.
    @return The integer index into the parallel string array. *)
let decode_str_idx (w : nativeint) : int =
  Nativeint.to_int (Nativeint.shift_right_logical w 3)

(** @brief Validates if a slab word is a String Index by checking if the 
           bottom 3 bits match the StrIdx tag 4.
    @param w The word to inspect.
    @return True if the word is a string table reference. *)
let is_str_idx (w : nativeint) : bool =
  Nativeint.logand w tag_mask = tag_str_idx

(** @brief Validates if a slab word is a Heap Pointer by checking if the 
           bottom 3 bits match the VPtr tag 3.
    @param w The word to inspect.
    @return True if the word represents a memory offset in the slab. *)
let is_ptr (w : nativeint) : bool =
  Nativeint.logand w tag_mask = tag_ptr

(** @brief Constructs an object header by shifting the Class ID left by 2 bits, 
           leaving bit 0 (Mark) and bit 1 (Free) initialized to zero.
    @param cid The unique integer ID assigned to the object's class.
    @return A nativeint header word for a live, unmarked object. *)
let make_header (cid : int) : nativeint =
  Nativeint.shift_left (Nativeint.of_int cid) 2

(** @brief Retrieves the Class ID from an object header by performing a 
           logical right shift of 2 bits to discard the GC status flags.
    @param h The raw header word from the first slot of an object.
    @return The integer Class ID. *)
let hdr_class_id (h : nativeint) : int =
  Nativeint.to_int (Nativeint.shift_right_logical h 2)

(** @brief Tests the least significant bit of the header to determine 
           if the object has been visited during the GC mark phase.
    @param h The object's header word.
    @return True if the mark bit is 1. *)
let hdr_is_marked (h : nativeint) : bool =
  Nativeint.logand h 1n <> 0n

(** @brief Applies a bitwise OR to set the least significant bit of the header, 
           indicating to the collector that this object is reachable.
    @param h The current header word.
    @return The updated header with the mark bit set. *)
let hdr_set_mark (h : nativeint) : nativeint =
  Nativeint.logor h 1n

(** @brief Applies a bitwise AND with a negated mask to clear the mark bit, 
           resetting the object's GC status for the next collection cycle.
    @param h The current header word.
    @return The updated header with the mark bit cleared. *)
let hdr_clear_mark (h : nativeint) : nativeint =
  Nativeint.logand h (Nativeint.lognot 1n)

(** @brief Inspects bit 1 of the header to determine if the slab block is 
           part of the free list rather than a live COOL object.
    @param h The word at the potential header offset.
    @return True if the free-node bit is set. *)
let hdr_is_free (h : nativeint) : bool =
  Nativeint.logand h 2n <> 0n

(** @brief Direct accessor that reads the first word of an object at offset [p] 
           and extracts the Class ID metadata.
    @param h The heap record containing the Bigarray slab.
    @param p The starting word offset of the object.
    @return The Class ID stored in the header. *)
let class_id (h : heap) (p : int) : int =
  hdr_class_id h.slab.{p}

(** @brief Direct accessor that reads the second word of an object at offset [p] 
           to determine the total memory footprint of the block.
    @param h The heap record.
    @param p The starting word offset of the object.
    @return The total size (Header + Size + Fields) in words. *)
let total_size (h : heap) (p : int) : int =
  Nativeint.to_int h.slab.{p + 1}

(** @brief Calculates the field offset and decodes the resulting 
           tagged word into an OCaml value for the VM interpreter.
    @param h The heap record.
    @param p The starting word offset of the object.
    @param n The zero-indexed field position.
    @return The decoded COOL value. *)
let get_field (h : heap) (p : int) (n : int) : value =
  decode h.slab.{p + 2 + n}

(** @brief Encodes an OCaml value and writes it to the slab at the calculated 
           field offset.
    @param h The heap record.
    @param p The starting word offset of the object.
    @param n The zero-indexed field position.
    @param v The value to encode and store. *)
let set_field (h : heap) (p : int) (n : int) (v : value) : unit =
  h.slab.{p + 2 + n} <- encode v

(** @brief Accessor for String objects that extracts the string 
           table index from the first field slot.
    @param h The heap record.
    @param p The word offset of the String object.
    @return The raw integer index into the parallel string array. *)
let get_str_field (h : heap) (p : int) : int =
  decode_str_idx h.slab.{p + 2}

(** @brief Setter for String objects that encodes and writes a 
           string table index into the first field slot.
    @param h The heap record.
    @param p The word offset of the String object.
    @param idx The integer index to store. *)
let set_str_field (h : heap) (p : int) (idx : int) : unit =
  h.slab.{p + 2} <- encode_str_idx idx

(** @brief Updates the header word of an object at offset [p] to set its 
           mark bit, effectively shielding it from reclamation in the current GC cycle.
    @param h The heap record.
    @param p The word offset of the object to mark. *)
let mark_obj (h : heap) (p : int) : unit =
  h.slab.{p} <- hdr_set_mark h.slab.{p}

(** @brief Checks if the object at offset [p] has its mark bit set in its header.
    @param h The heap record.
    @param p The word offset of the object.
    @return True if the object is currently marked as reachable. *)
let is_marked (h : heap) (p : int) : bool =
  hdr_is_marked h.slab.{p}

(** @brief Initializer that writes the header and size words, zeros 
           out all fields to VVoid, and updates the live word counter for GC tracking.
    @param h The heap record.
    @param off The slab offset where the block starts.
    @param cid The Class ID to write into the header.
    @param size The total number of words allocated for this block. *)
let write_object (h : heap) (off : int) (cid : int) (size : int) : unit =
  h.n_live_words    <- h.n_live_words + size;
  h.slab.{off}      <- make_header cid;
  h.slab.{off + 1}  <- Nativeint.of_int size;
  for i = 2 to size - 1 do
    h.slab.{off + i} <- 0n
  done

(** @brief Compares the current count of words occupied by live objects against 
            the configured threshold (currently 75% heap usage) to determine if a collection cycle is due.
    @param h The heap record.
    @return True if n_live_words exceeds or equals the threshold. *)
let needs_gc (h : heap) : bool =
  h.n_live_words >= h.threshold

(** @brief Attempts to find a suitable block in the free list using first-fit; 
           otherwise, it allocates at the bump pointer and checks for heap exhaustion.
    @param h The heap record.
    @param cid The Class ID of the object being allocated.
    @param n_fields The number of attributes/fields defined for the class.
    @return The slab offset of the newly allocated and initialized object. *)
let alloc (h : heap) (cid : int) (n_fields : int) : int =
  let size = 2 + n_fields in

  (* first fit search through the free list *)
  let rec search acc = function
    | [] -> None
    | (off, sz) :: rest when sz >= size ->
      h.free <- List.rev_append acc rest;
      Some (off, sz)
    | blk :: rest ->
      search (blk :: acc) rest
  in
  (match search [] h.free with
  | Some (off, sz) ->
    let rem = sz - size in
    if rem >= 2 then (
      (* split the slab: mark the remainder as a free node.
         the remainder is not counted in n_live_words, only live
         objects are. write_object handles the accounting. *)
      let r = off + size in
      h.slab.{r}     <- free_node_hdr;
      h.slab.{r + 1} <- Nativeint.of_int rem;
      h.free <- (r, rem) :: h.free
    );
    write_object h off cid size;
    off
  | None ->
    (* bump allocate *)
    if h.next + size > h.capacity then
      Error.vm "0" "out of memory: heap exhausted after garbage collection";
    let off = h.next in
    h.next <- h.next + size;
    write_object h off cid size;
    off)
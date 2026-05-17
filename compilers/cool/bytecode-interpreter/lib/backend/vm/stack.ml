(**
@file   stack.ml
@brief  Implements the VM call stack and value stack, supporting frame
        creation, argument installation, local access, and stack-value
        operations.
@author Trey Rubino
@date   11/30/2025
*)

open Runtime

(** @brief Creates and pushes a new activation record (frame) onto the call stack. 
           This involves looking up method metadata, allocating a local variable 
           array sized for both formals and locals, and installing arguments.
    @param st The current global VM state.
    @param self_ptr The slab word offset of the 'self' object for this method call.
    @param method_id The unique identifier for the method being invoked.
    @param args The list of COOL values to be mapped to the method's formal parameters. *)
let push_frame (st : vm_state) (self_ptr : int) (method_id : int) (args : value list) : unit =
  let m      = st.ir.methods.(method_id) in
  let locals = Array.make (m.n_formals + m.n_locals) VVoid in
  List.iteri (fun i v ->
    if i < m.n_formals then locals.(i) <- v
    else Error.vm "0" "too many arguments to method"
  ) args;
  let frame = {
    method_info = m;
    locals;
    pc       = 0;
    self_ptr;
  } in
  st.frames <- frame :: st.frames

(** @brief Removes the top-most activation record from the call stack and returns it. 
           Used during method return to unwind the execution context.
    @param st The current global VM state.
    @return The popped frame containing the previous execution state. *)
let pop_frame (st : vm_state) : frame =
  match st.frames with
  | fr :: rest ->
    st.frames <- rest;
    fr
  | [] ->
    Error.vm "0" "call stack underflow"

(** @brief Retrieves the current active frame from the top of the call stack 
           without removing it.
    @param st The current global VM state.
    @return The top-most frame representing the currently executing method. *)
let peek_frame (st : vm_state) : frame =
  match st.frames with
  | fr :: _ -> fr
  | []      -> Error.vm "0" "no active frame"

(** @brief Retrieves a COOL value from a specific slot in the current frame's 
           local variable array.
    @param st The current global VM state.
    @param slot The zero-indexed position in the locals array.
    @return The value stored at the requested slot. *)
let get_local (st : vm_state) (slot : int) : value =
  match st.frames with
  | f :: _ -> f.locals.(slot)
  | []     -> Error.vm "0" "no active frame"

(** @brief Updates the current frame's local variable array at a specific slot 
           with a new COOL value.
    @param st The current global VM state.
    @param slot The zero-indexed position in the locals array.
    @param v The COOL value to store in the local slot. *)
let set_local (st : vm_state) (slot : int) (v : value) : unit =
  match st.frames with
  | f :: _ -> f.locals.(slot) <- v
  | []     -> Error.vm "0" "no active frame"

(** @brief Pushes a COOL value onto the global operand stack (value stack) 
    @param st The current global VM state.
    @param v The value to be added to the top of the stack. *)
let push_val (st : vm_state) (v : value) : unit =
  st.stack <- v :: st.stack

(** @brief Removes and returns the top-most value from the global operand stack.
    @param st The current global VM state.
    @return The value previously at the top of the stack. *)
let pop_val (st : vm_state) : value =
  match st.stack with
  | v :: rest ->
    st.stack <- rest;
    v
  | [] -> Error.vm "0" "value stack underflow"

(** @brief Retrieves the top-most value from the global operand stack 
           without removing it.
    @param st The current global VM state.
    @return The value currently at the top of the stack. *)
let peek_val (st : vm_state) : value =
  match st.stack with
  | v :: _ -> v
  | []     -> Error.vm "0" "value stack empty"
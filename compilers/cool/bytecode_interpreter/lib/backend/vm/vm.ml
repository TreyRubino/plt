(**
@file   vm.ml
@brief  Entry point for constructing the VM, instantiating the main object,
        invoking its initializer, installing the entry method, and starting
        program execution.
@author Trey Rubino
@date   11/30/2025
*)

open Runtime
open Exec
open Alloc
open Stack
open Ir

(** @brief Orchestrates the complete Virtual Machine lifecycle, from memory 
           initialization to final bytecode execution. It instantiates the 
           runtime state, allocates the 'Main' object on the heap, and 
           sets up the initial call frame for the entry point.
    @param ir The complete intermediate representation (IR) of the program, 
              including the class hierarchy and method table.
    @return The final COOL value resulting from the program's execution. *)
let execute (ir : Ir.ir) : value =
  let st            = Runtime.create_vm ir in
  let entry_m       = ir.methods.(ir.entry_method) in
  let main_class_id = entry_m.class_id in
  let main_ptr      = Alloc.allocate_object st main_class_id in
  Stack.push_frame st main_ptr ir.entry_method [];
  Exec.run st
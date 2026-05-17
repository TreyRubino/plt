(**
@file   emit.ml
@brief  Utilities for managing a mutable instruction buffer, including 
        bytecode emission, jump back-patching, and source-to-PC line mapping.
@author Trey Rubino
@date   11/30/2025
*)

open Bytecode

type t = {
  mutable code : instruction list;
  mutable line_map : (int * int) list; (* (PC, Line) *)
  mutable last_line : int;
}

let create () = { 
  code = []; 
  line_map = []; 
  last_line = -1; 
} 

(** @brief Correlates the current Program Counter (PC) with a source code line 
           number. This mapping is essential for the VM to provide accurate 
           stack traces and error messages during runtime.
    @param buf The current instruction buffer.
    @param loc_str The string representation of the source location. *)
let record_line buf loc_str =
  let line = try int_of_string loc_str with _ -> 0 in
  let current_pc = List.length buf.code in
  if line <> buf.last_line && line <> 0 then (
    buf.line_map <- (current_pc, line) :: buf.line_map;
    buf.last_line <- line
  )

let emit_op buf op loc =
  record_line buf loc;
  buf.code <- buf.code @ [{ op; arg = NoArg }]

let emit_op_i buf op n loc = 
  record_line buf loc;
  buf.code <- buf.code @ [{ op; arg = IntArg n }]

let mark buf = List.length buf.code 

(** @brief Modifies an existing instruction at a specific index. This is 
           primarily used for "back-patching" jump instructions once the 
           target offset is determined later in the codegen process.
    @param buf The instruction buffer.
    @param idx The PC index of the instruction to modify.
    @param op The new opcode (usually the same).
    @param arg The new operand (usually the calculated jump offset). *)
let patch buf idx op arg = 
  let rec upd i = function
    | [] -> []
    | x :: xs -> 
      if i = idx then { op; arg } :: xs
      else x :: upd (i + 1) xs
  in
  buf.code <- upd 0 buf.code
 
let to_program buf = 
  Array.of_list buf.code

let get_line_map buf =
  Array.of_list (List.rev buf.line_map)
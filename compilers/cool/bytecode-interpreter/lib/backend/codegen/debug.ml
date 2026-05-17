(**
@file   debug.ml
@brief  Tools for generating readable dumps of class layouts, dispatch
        tables, constants, and bytecode. Only active during '-d' argument
        specification.
@author Trey Rubino
@date   11/30/2025
*)

open Printf
open Bytecode

let string_of_opcode = function
  | Bytecode.OP_POP -> "POP"
  | OP_CONST -> "CONST"
  | OP_TRUE -> "TRUE"
  | OP_FALSE -> "FALSE"
  | OP_VOID -> "VOID"
  | OP_GET_LOCAL -> "GET_LOCAL"
  | OP_SET_LOCAL -> "SET_LOCAL"
  | OP_GET_SELF -> "GET_SELF"
  | OP_GET_ATTR -> "GET_ATTR"
  | OP_SET_ATTR -> "SET_ATTR"
  | OP_NEW -> "NEW"
  | OP_NEW_SELF_TYPE -> "NEW_SELF_TYPE"
  | OP_CALL -> "CALL"
  | OP_JUMP -> "JUMP"
  | OP_JUMP_IF_FALSE -> "JUMP_IF_FALSE"
  | OP_CASE_ABORT -> "OP_CASE_ABORT"
  | OP_ADD -> "ADD"
  | OP_SUB -> "SUB"
  | OP_MUL -> "MUL"
  | OP_DIV -> "DIV"
  | OP_NEG -> "NEG"
  | OP_NOT -> "NOT"
  | OP_EQUAL -> "EQUAL"
  | OP_LESS -> "LESS"
  | OP_LESS_EQUAL -> "LESS_EQUAL"
  | OP_ISVOID -> "ISVOID"
  | OP_IS_SUBTYPE -> "IS_SUBTYPE"
  | OP_DISPATCH -> "DISPATCH"
  | OP_STATIC_DISPATCH -> "STATIC_DISPATCH"
  | OP_RETURN -> "RETURN"

let string_of_operand = function
  | Bytecode.NoArg -> ""
  | IntArg n -> sprintf " %d" n
  | OffsetArg o -> sprintf " %d" o

(** @brief Generates a text file dumping the entire state of 
           the IR. This includes the constant pool, the class hierarchy 
           with attribute offsets, the method dispatch tables, and a 
           disassembled view of every bytecode instruction.
    @param filename The path where the IR dump should be written.
    @param ir The complete Intermediate Representation to be inspected. *)
let dump_ir (filename : string) (ir : Ir.ir) : unit =
  let oc = open_out filename in

  let pf fmt = fprintf oc fmt in

  let class_name (ir : Ir.ir) (cid : int) =
    ir.classes.(cid).name
  in

  pf "--- IR.consts ---\n%!";
  Array.iteri (fun i c ->
    match c with
    | Ir.LInt n    -> pf "%d: int %d\n%!" i n
    | Ir.LBool b   -> pf "%d: bool %b\n%!" i b
    | Ir.LString s -> pf "%d: string \"%s\"\n%!" i s
    | Ir.LVoid     -> pf "%d: void\n%!" i
  ) ir.consts;

  pf "\n--- IR.classes ---\n%!";
  Array.iter (fun (cls : Ir.class_info) ->
    pf "class %s (id=%d parent=%d size=%d)\n%!" cls.name cls.id cls.parent_id (Array.length cls.dispatch);
    Array.iter (fun (a : Ir.attr_info) ->
      pf "  attr %s @%d\n%!" a.name a.offset
    ) cls.attributes
  ) ir.classes;

  pf "\n--- IR.methods ---\n%!";

  Array.iteri (fun i (m : Ir.method_info) ->
    let cls = class_name ir m.class_id in
    pf "method[%d] %s.%s (class=%d formals=%d locals=%d)\n%!"
      i cls m.name m.class_id m.n_formals m.n_locals;

    pf "  code size=%d\n%!" (Array.length m.code);

    Array.iteri (fun pc instr ->
      let arg_ctor =
        match instr.arg with
        | NoArg -> "NoArg()"
        | IntArg n -> Printf.sprintf "IntArg(%d)" n
        | OffsetArg o -> Printf.sprintf "OffsetArg(%d)" o
      in
      pf "    %04d: %s %s\n%!"
        pc
        (string_of_opcode instr.op)
        arg_ctor
    ) m.code;
  ) ir.methods;

  (* entry point *)
  pf "\nentry_method=%d\n%!" ir.entry_method;
  pf "\n--- end IR dump ---\n%!";

  close_out oc
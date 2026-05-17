(**
@file   bytecode.ml
@brief  Declares the full instruction set architecture (ISA) for the COOL VM,
        including opcodes, operands, instruction records, and program
        representations. Used by the code generator and VM.
@author Trey Rubino
@date   11/15/2025
*)

(** @brief Defines the full set of opcodes supported by the COOL Virtual Machine. 
           These operations cover stack manipulation, arithmetic, control flow, 
           object creation, and both dynamic and static method dispatch. *)
type opcode =
  | OP_POP
  | OP_CONST
  | OP_TRUE
  | OP_FALSE
  | OP_VOID
  | OP_GET_LOCAL
  | OP_SET_LOCAL
  | OP_GET_SELF
  | OP_GET_ATTR
  | OP_SET_ATTR
  | OP_NEW
  | OP_NEW_SELF_TYPE
  | OP_CALL
  | OP_JUMP
  | OP_JUMP_IF_FALSE
  | OP_CASE_ABORT
  | OP_ADD
  | OP_SUB
  | OP_MUL
  | OP_DIV
  | OP_NEG
  | OP_NOT
  | OP_EQUAL
  | OP_LESS
  | OP_LESS_EQUAL
  | OP_ISVOID
  | OP_IS_SUBTYPE
  | OP_DISPATCH
  | OP_STATIC_DISPATCH
  | OP_RETURN

(** @brief Utility function to convert an opcode variant into its 
           corresponding string representation for logging or debugging.
    @param op The opcode variant to stringify.
    @return A string matching the variant name. *)
let string_of_op = function
  | OP_POP -> "OP_POP"
  | OP_CONST -> "OP_CONST"
  | OP_TRUE -> "OP_TRUE"
  | OP_FALSE -> "OP_FALSE"
  | OP_VOID -> "OP_VOID"
  | OP_GET_LOCAL -> "OP_GET_LOCAL"
  | OP_SET_LOCAL -> "OP_SET_LOCAL"
  | OP_GET_SELF -> "OP_GET_SELF"
  | OP_GET_ATTR -> "OP_GET_ATTR"
  | OP_SET_ATTR -> "OP_SET_ATTR"
  | OP_NEW -> "OP_NEW"
  | OP_NEW_SELF_TYPE -> "OP_NEW_SELF_TYPE"
  | OP_CALL -> "OP_CALL"
  | OP_JUMP -> "OP_JUMP"
  | OP_JUMP_IF_FALSE -> "OP_JUMP_IF_FALSE"
  | OP_CASE_ABORT -> "OP_CASE_ABORT"
  | OP_ADD -> "OP_ADD"
  | OP_SUB -> "OP_SUB"
  | OP_MUL -> "OP_MUL"
  | OP_DIV -> "OP_DIV"
  | OP_NEG -> "OP_NEG"
  | OP_NOT -> "OP_NOT"
  | OP_EQUAL -> "OP_EQUAL"
  | OP_LESS -> "OP_LESS"
  | OP_LESS_EQUAL -> "OP_LESS_EQUAL"
  | OP_ISVOID -> "OP_ISVOID"
  | OP_IS_SUBTYPE -> "OP_IS_SUBTYPE"
  | OP_DISPATCH -> "OP_DISPATCH"
  | OP_STATIC_DISPATCH -> "OP_STATIC_DISPATCH"
  | OP_RETURN -> "OP_RETURN"

(** @brief Represents the argument associated with an instruction. 
           Instructions can have no arguments, a direct integer argument 
           (like a class ID or local slot), or a relative PC offset for jumps. *)
type operand = 
  | NoArg
  | IntArg of int
  | OffsetArg of int

(** @brief A single executable unit within the VM, pairing an operation 
           with its necessary metadata (operand). *)
type instruction = {
  op : opcode;
  arg: operand;
}

(** @brief A linearized sequence of instructions representing a compiled 
           method body, optimized for fast indexing during the VM's 
           fetch-decode-execute cycle. *)
type program = instruction array
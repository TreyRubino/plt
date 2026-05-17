(**
@file   ir.ml
@brief  COOL Language Intermediate Representation
@author Trey Rubino
@date   11/15/2025
*)

(** @brief Represents a primitive literal value in the IR. These are stored in 
           the global constant table and referenced by bytecode instructions.
    @param LInt An unboxed 32-bit integer.
    @param LBool An unboxed boolean value.
    @param LString A string literal, to be interned in the parallel string table.
    @param LVoid The null/void sentinel. *)
type literal = 
  | LInt of int
  | LBool of bool
  | LString of string
  | LVoid 

(** @brief A fixed-size array acting as the global pool for all constant 
           literals defined in the program. *)
type const_table = literal array

(** @brief Metadata for a class attribute, mapping its name to its 
           calculated word-offset within the object's slab memory.
    @param offset The field index (starting after the header and size words). *)
type attr_info = {
  name   : string;
  offset : int; 
}

(** @brief Defines the structural and behavioral layout of a COOL class. 
           It includes the inheritance links, the physical attribute layout, 
           and the virtual dispatch table (vtable) for dynamic method calls.
    @param dispatch An array of global Method IDs, where the array index 
                    corresponds to the dispatch slot. *)
type class_info = {
  name : string;
  id   : int; 
  parent_id  : int;
  attributes : attr_info array;
  dispatch   : int array;
}

(** @brief Contains the complete execution profile for a method, including 
           the compiled bytecode, stack requirements (locals/formals), 
           and the line map used for runtime error reporting.
    @param code The array of VM instructions.
    @param line_map A mapping of Program Counter (PC) to source line number. *)
type method_info = {
  name : string;
  class_id  : int;
  n_locals  : int;
  n_formals : int;
  code : Bytecode.program;
  line_map : (int * int) array;
}

(** @brief The top-level IR container representing a fully compiled COOL 
           program. It packages the constant pool, class definitions, 
           and method implementations into a single record for the VM.
    @param entry_method The global ID of the method where execution starts. *)
type ir = {
  consts  : const_table; 
  classes : class_info array;
  methods : method_info array;
  entry_method : int;
}


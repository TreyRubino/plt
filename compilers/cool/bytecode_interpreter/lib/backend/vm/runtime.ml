(**
@file   runtime.ml
@brief  Defines the core runtime types used by the VM: the unboxed value
        type, the raw word-slab managed heap, the parallel string table,
        the call frame, and the virtual machine state. Integers and booleans
        are unboxed values that never touch the heap. Only COOL objects
        created with new and string results go to the slab.
@author Trey Rubino
@date   03/28/2026
*)

open Bigarray
open Ir

type value =
  | VInt  of int
  | VBool of bool
  | VPtr  of int     (* word offset into the raw heap slab *)
  | VVoid

type heap = {
  slab               : (nativeint, nativeint_elt, c_layout) Array1.t;
  mutable next       : int;              (* bump pointer: next free word offset *)
  mutable free       : (int * int) list; (* (offset, total_words) free blocks   *)
  mutable n_live_words : int;            (* words occupied by live objects only  *)
  capacity           : int;              (* total words in slab                  *)
  threshold          : int;              (* GC trigger when n_live_words >= this *)
}

(* parallel string table to reduce heap manage complexity of string bits *)
type strings = {
  data           : string array;
  mutable live   : bool array;       (* GC mark bitmap, reset each cycle *)
  mutable free   : int list;         (* free slot indices *)
  mutable n_live : int;
  capacity       : int;
  tbl            : (string, int) Hashtbl.t;  (* content -> slot index *)
}

type frame = {
  mutable pc  : int;
  method_info : Ir.method_info;
  locals      : value array;
  self_ptr    : int;               (* word offset of self in slab *)
}

type vm_state = {
  ir             : Ir.ir;
  mutable stack  : value list;
  mutable frames : frame list;
  heap           : heap;
  strings        : strings;
}

let heap_capacity  = 1048576                    (* 1M words, 8MB on 64-bit *)
let heap_threshold = heap_capacity * 3 / 4      (* GC at 75% live usage    *)
let str_capacity   = 65536

(** @brief Allocates and initializes the raw word slab using C-layout memory. 
           All words are initially zeroed to ensure VVoid consistency.
    @return An initialized heap record with a 1M word capacity. *)
let create_heap () : heap =
  let slab = Array1.create nativeint c_layout heap_capacity in
  Array1.fill slab 0n;
  {
    slab;
    next         = 0;
    free         = [];
    n_live_words = 0;
    capacity     = heap_capacity;
    threshold    = heap_threshold;
  }

(** @brief Allocates the structures required for the parallel string table, 
           initializing a free-list of indices and an empty interning hash table.
    @return An initialized strings record with a 64k slot capacity. *)
let create_strings () : strings =
  {
    data     = Array.make str_capacity "";
    live     = Array.make str_capacity false;
    free     = List.init str_capacity (fun i -> i);
    n_live   = 0;
    capacity = str_capacity;
    tbl      = Hashtbl.create 512;
  }

(** @brief Constructs the initial VM state by linking the provided IR 
           definitions with freshly allocated heap and string management systems.
    @param ir The intermediate representation produced by the compiler.
    @return A clean vm_state ready for program execution. *)
let create_vm (ir : Ir.ir) : vm_state =
  {
    ir;
    stack   = [];
    frames  = [];
    heap    = create_heap ();
    strings = create_strings ();
  }

let string_of_value = function
  | VVoid   -> "void"
  | VInt i  -> Printf.sprintf "Int(%d)" i
  | VBool b -> Printf.sprintf "Bool(%b)" b
  | VPtr p  -> Printf.sprintf "Ptr(%d)" p
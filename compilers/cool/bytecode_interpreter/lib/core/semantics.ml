(**
@file   semantics.ml
@brief  Semantic environment produced by the type checker and consumed
        by codegen
@author Trey Rubino
@date   11/15/2025
*)

open Ast

(** @brief Represents the concrete implementation of a class attribute. 
           It stores the attribute's name, its declared type, and an 
           optional initialization expression.
    @param aname The string name of the attribute.
    @param atype The declared COOL type of the attribute.
    @param init An optional AST expression for the attribute's default value. *)
type attr_impl = {
  aname : string;
  atype : string;
  init  : expr option;
}

(** @brief Distinguishes between methods defined in COOL source code and 
           Internal methods provided by the runtime (like IO or String 
           primitives).
    @param Internal A built-in routine with a return type and a qualified name.
    @param User A method containing a standard COOL AST expression. *)
type method_body =
  | Internal of { rtype : string; qname : string }
  | User of expr

(** @brief Defines the full implementation of a method, including the 
           class where it was original defined (for dispatch resolution), 
           its formal parameters, and its body.
    @param definer The name of the class that provides this implementation.
    @param formals The list of identifiers and types for method arguments. *)
type method_impl = {
  definer : string;
  formals : (id * cool_type) list;
  body    : method_body;
}

type class_attrs   = attr_impl list

(** @brief A mapping of method names to their corresponding implementations 
           for a specific class. *)
type class_methods = (string, method_impl) Hashtbl.t

(** @brief The central repository of semantic information for a COOL program. 
           It contains the class layout map (attributes), the implementation 
           map (methods), and the inheritance hierarchy (parent map).
    @param class_map Maps class names to their ordered list of attributes.
    @param impl_map Maps class names to their specific method implementation tables.
    @param parent_map Maps class names to their direct parent classes. *)
type semantic_env = {
  class_map  : (string, class_attrs) Hashtbl.t;
  impl_map   : (string, class_methods) Hashtbl.t;
  parent_map : (string, string) Hashtbl.t;
}

(** @brief Initializes an empty semantic environment with fresh hash tables, 
           ready to be populated by the semantic analyzer.
    @return A clean semantic_env record. *)
let empty_env () : semantic_env =
  {
    class_map  = Hashtbl.create 255;
    impl_map   = Hashtbl.create 255;
    parent_map = Hashtbl.create 255;
  }

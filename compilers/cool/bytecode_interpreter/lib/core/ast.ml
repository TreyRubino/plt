(**
@file   ast.ml
@brief  COOL Language recursivable mutual Abstract Syntax Tree
@author Trey Rubino
@date   11/15/2025
*)

(** @brief Represents the static type of an expression as determined by the 
           semantic analyzer. It distinguishes between standard class types 
           and the polymorphic 'SELF_TYPE'.
    @param Class A standard named class in the hierarchy.
    @param SELF_TYPE The dynamic type of the 'self' object, specialized to a class. *)
type static_type =
	| Class of string
	| SELF_TYPE of string

(** @brief The top-level representation of a COOL program, consisting of a 
           list of class definitions. This is a mutually recursive structure 
           reflecting the nesting of classes, features, and expressions. *)
type cool_program = cool_class list
and loc = string
and id = loc * string
and cool_type = id

(** @brief Represents a single class definition in the AST. It includes the 
           class name, an optional parent class for inheritance, and a 
           list of features (attributes and methods). *)
and cool_class = id * (id option) * feature list

(** @brief Defines a feature within a class. Features are either attributes 
           with optional initializers or methods (behavior) 
           with a list of formal parameters and a body expression. *)
and feature =
	| Attribute of id * cool_type * (expr option)
	| Method  of id * (formal list) * cool_type * expr

(** @brief Defines a formal parameter for a method, pairing a unique 
           identifier with its declared COOL type. *)
and formal = id * cool_type

(** @brief The fundamental unit of execution in the AST. Every expression 
           tracks its source code location and its inferred static type 
           alongside the specific operation it performs. *)
and expr = {
	loc : loc;
	expr_kind : expr_kind;
	mutable static_type : static_type option;
}

(** @brief Enumerates the various types of operations and control structures 
           available in COOL. This includes assignments, multiple dispatch 
           strategies, loops, conditionals, and primitive literals. *)
and expr_kind =
	| Assign of id * expr
	| DynamicDispatch of expr * id * expr list
	| StaticDispatch of expr * id * id * expr list
	| SelfDispatch of id * expr list
  | Let of (id * id * expr option) list * expr 
	| Case of expr * (id * id * expr) list
	| If of expr * expr * expr
	| While of expr * expr
	| New of id
	| Isvoid of expr
	| Plus of expr * expr
	| Minus of expr * expr
	| Times of expr * expr
	| Divide of expr * expr
	| Tilde of expr
	| Lt of expr * expr
	| Le of expr * expr
	| Equals of expr * expr
	| Not of expr
	| Identifier of id
	| Integer of string
	| String of string
	| True
  | False
  | Block of expr list

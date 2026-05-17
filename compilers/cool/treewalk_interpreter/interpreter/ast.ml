(* @author Trey Rubino *)
type static_type =
	| Class of string
	| SELF_TYPE of string

type cool_program = cool_class list
and loc = string
and id = loc * string
and cool_type = id
and cool_class = id * (id option) * feature list
and feature =
	| Attribute of id * cool_type * (expr option)
	| Method  of id * (formal list) * cool_type * expr
and formal = id * cool_type
and expr = {
	loc : loc;
	expr_kind : expr_kind;
	mutable static_type : static_type option;
}
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

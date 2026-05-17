(*
@author Trey Rubino 
@date 09/14/2025
*)

open Reader

(* 
when the interpreter evalutes an expression, it will produce one of 
the following runtime values
*)
type obj = {
  cls : string;
  fields : (string, value ref) Hashtbl.t;
  oid : int;
}
and value = 
  | VVoid
  | VInt of int
  | VBool of bool
  | VString of string
  | VObj of obj

(* 
when a value is declared and not explicitly initialized, 
we need to give it a default
*)
let default_of_type (t : string) : value = 
  match t with
  | "Int" -> VInt 0 
  | "Bool" -> VBool false
  | "String" -> VString ""
  | _ -> VVoid (* Object, IO, user classes *)

(*
given a class, walk the parent map up to object
*)
let ancestry (parent_map : (string, string) Hashtbl.t) (cls : string) : string list = 
  let rec go acc c = 
    if c = "Object" then List.rev ("Object" :: acc)
    else
      let p = try Hashtbl.find parent_map c with Not_found -> "Object" in
      go (c :: acc) p
    in 
    go [] cls

(*
get attributes in linearized order (Object->...->cls)
*)
let attributes_linearized (env : runtime_env) (cls : string) : attr_info list =
  let get_attrs c = 
    try Hashtbl.find env.class_map c 
    with Not_found -> []
  in
  ancestry env.parent_map cls
  |> List.map get_attrs
  |> List.concat

(*
run default initializers in ancestor -> descendant order
*)
let next_oid = ref 0
let fresh_oid () =
  let id = !next_oid in
  incr next_oid;
  id

let new_object_defaults (env : runtime_env) (cls : string) : obj = 
  let fields = Hashtbl.create 31 in
  attributes_linearized env cls
  |> List.iter (fun { aname; atype; _} -> 
      Hashtbl.replace fields aname (ref (default_of_type atype)));
  { cls; fields; oid = fresh_oid () }

(*
given the runtime env, loop up the class name in implementation map 
then look up the specific method name given the class
*)
let lookup_method (env : runtime_env) (cls : string) (mname : string) : method_impl option = 
  try 
    let methods = Hashtbl.find env.impl_map cls in
    try Some (Hashtbl.find methods mname)
    with Not_found -> None 
  with Not_found -> None

(*
centralized runtime error message and exit handler 
*)
let runtime_error (loc : string) (msg : string) : 'a = 
  Printf.printf "ERROR: %s: Exception: %s\n" loc msg;
  exit 1

(*
lexing (static) scoping for variables
variables scope is determined by its physical location
in the source code rather than the order in which functions
are called at runtime
*)

(* activation record for each block/let/method *)
type scope = (string, value ref) Hashtbl.t
let new_scope () : scope = Hashtbl.create 31 

let push_scope (s : scope) (stack : scope list) : scope list = s :: stack
let pop_scope (stack : scope list) : scope list = 
  match stack with 
  | _ :: tl -> tl
  | [] -> failwith "pop_scope: empty stack"

let bind_local (stack : scope list) (name : string) (v : value) : unit = 
  match stack with
  | s :: _ -> Hashtbl.replace s name (ref v)
  | [] -> failwith "bind_local: no active scope"

let rec lookup_local_cell (stack : scope list) (name : string) : (value ref) option =
  match stack with
  | s :: tl -> (try Some (Hashtbl.find s name) with Not_found -> lookup_local_cell tl name)
  | [] -> None

let lookup_field_cell (self : obj) (name : string) : (value ref) option = 
  try Some (Hashtbl.find self.fields name) with Not_found -> None
  
let lookup_value ~(self:obj) (stack : scope list) (name : string) : value option =
  if name = "self" then Some (VObj self)
  else
    match lookup_local_cell stack name with
    | Some cell -> Some !cell
    | None ->
      (match lookup_field_cell self name with
      | Some cell -> Some !cell
      | None -> None)

let lookup_lvalue_cell ~(self:obj) (stack : scope list) (name : string) : (value ref) option = 
  if name = "self" then None
  else
    match lookup_local_cell stack name with
    | Some cell -> Some cell
    | None -> lookup_field_cell self name
(*
@author Trey Rubino
@date 09/06/2025
*)

open Ast

type attr_info = {
  aname : string;
  atype : string;
  init  : expr option;
}

type method_body = 
  | Internal of { rtype : string; qname : string }
  | User of expr

type method_impl = {
  definer : string;
  formals : string list;
  body    : method_body;
}

type class_attrs = attr_info list
type class_methods = (string, method_impl) Hashtbl.t

type runtime_env = {
  class_map  : (string, class_attrs) Hashtbl.t;
  impl_map   : (string, class_methods) Hashtbl.t;
  parent_map : (string, string) Hashtbl.t;
}

let rec range k = if k <= 0 then [] else k :: (range (k - 1))
let read ic = input_line ic

let read_list ic worker =
  let k = int_of_string (read ic) in
  List.map (fun _ -> worker ic) (range k)

let read_id ic =
  let loc = read ic in
  let name = read ic in
  (loc, name)

let rec read_expr_from ic eloc : expr =
  let _static_type = read ic in
  let tag = read ic in
  let ekind =
    match tag with
    | "assign" ->
      let lhs = read_id ic in
      let rhs = read_expr ic in
      Assign (lhs, rhs)
    | "dynamic_dispatch" ->
      let recv = read_expr ic in
      let mid  = read_id ic in
      let args = read_list ic read_expr in
      DynamicDispatch (recv, mid, args)
    | "static_dispatch" ->
      let recv = read_expr ic in
      let tyid = read_id ic in
      let mid  = read_id ic in
      let args = read_list ic read_expr in
      StaticDispatch (recv, tyid, mid, args)
    | "self_dispatch" ->
      let mid  = read_id ic in
      let args = read_list ic read_expr in
      SelfDispatch (mid, args)
    | "if" ->
      let p = read_expr ic in
      let t = read_expr ic in
      let e = read_expr ic in
      If (p, t, e)
    | "while" ->
      let p = read_expr ic in
      let b = read_expr ic in
      While (p, b)
    | "let" ->
      let bindings =
        read_list ic (fun ic ->
          match read ic with
          | "let_binding_no_init" ->
            let v = read_id ic in
            let t = read_id ic in
            (v, t, None)
          | "let_binding_init" ->
            let v = read_id ic in
            let t = read_id ic in
            let init = read_expr ic in
            (v, t, Some init)
          | x -> failwith ("bad let_binding tag: " ^ x)
        )
      in
      let body = read_expr ic in
      Let (bindings, body)
    | "case" ->
      let scrut = read_expr ic in
      let branches =
        read_list ic (fun ic ->
          let v = read_id ic in
          let t = read_id ic in
          let br = read_expr ic in
          (v, t, br)
        )
      in
      Case (scrut, branches)
    | "new" ->
      New (read_id ic)
    | "isvoid" ->
      Isvoid (read_expr ic)
    | "plus"   -> let a = read_expr ic in let b = read_expr ic in Plus (a, b)
    | "minus"  -> let a = read_expr ic in let b = read_expr ic in Minus (a, b)
    | "times"  -> let a = read_expr ic in let b = read_expr ic in Times (a, b)
    | "divide" -> let a = read_expr ic in let b = read_expr ic in Divide (a, b)
    | "lt"     -> let a = read_expr ic in let b = read_expr ic in Lt (a, b)
    | "le"     -> let a = read_expr ic in let b = read_expr ic in Le (a, b)
    | "eq"     -> let a = read_expr ic in let b = read_expr ic in Equals (a, b)
    | "not"    -> Not (read_expr ic)
    | "negate" -> Tilde (read_expr ic)
    | "identifier" -> Identifier (read_id ic)
    | "integer" -> Integer (read ic)
    | "string"  -> String (read ic)
    | "true"    -> True
    | "false"   -> False
    | "block" ->
      let es = read_list ic read_expr in
      Block es
    | x -> failwith ("expression kind unhandled: " ^ x)
  in
  { loc = eloc; expr_kind = ekind; static_type = None }

and read_expr ic : expr =
  let eloc = read ic in
  read_expr_from ic eloc

let read_class_map ic : (string, class_attrs) Hashtbl.t =
  let tag = read ic in
  if tag <> "class_map" then failwith ("expected class_map, got " ^ tag);
  let class_entries =
    read_list ic (fun ic ->
      let cname = read ic in
      let attrs =
        read_list ic (fun ic ->
          match read ic with
          | "no_initializer" ->
            let aname = read ic in
            let atype = read ic in
            { aname; atype; init = None }
          | "initializer" ->
            let aname = read ic in
            let atype = read ic in
            let init = read_expr ic in
            { aname; atype; init = Some init }
          | x -> failwith ("bad attribute tag: " ^ x)
        )
      in
      (cname, attrs)
    )
  in
  let tbl = Hashtbl.create (List.length class_entries) in
  List.iter (fun (cname, attrs) -> Hashtbl.replace tbl cname attrs) class_entries;
  tbl

let read_implementation_map ic : (string, class_methods) Hashtbl.t =
  let tag = read ic in
  if tag <> "implementation_map" then failwith ("expected implementation_map, got " ^ tag);
  let class_entries =
    read_list ic (fun ic ->
      let cname = read ic in
      let methods_kv =
        read_list ic (fun ic ->
          let mname   = read ic in
          let formals = read_list ic (fun ic -> read ic) in
          let definer = read ic in
          let head    = read ic in
          let body =
            if head = "0" then
              let rtype = read ic in
              let internal_tag = read ic in
              if internal_tag <> "internal" then failwith "expected 'internal'";
              let qname = read ic in
              Internal { rtype; qname }
            else
              let expr = read_expr_from ic head in
              User expr
          in
          (mname, { definer; formals; body })
        )
      in
      (cname, methods_kv)
    )
  in
  let tbl = Hashtbl.create (List.length class_entries) in
  List.iter (fun (cname, methods_kv) ->
    let mtbl = Hashtbl.create (List.length methods_kv) in
    List.iter (fun (mname, impl) -> Hashtbl.replace mtbl mname impl) methods_kv;
    Hashtbl.replace tbl cname mtbl
  ) class_entries;
  tbl

let read_parent_map ic : (string, string) Hashtbl.t =
  let tag = read ic in
  if tag <> "parent_map" then failwith ("expected parent_map, got " ^ tag);
  let pairs =
    read_list ic (fun ic ->
      let child  = read ic in
      let parent = read ic in
      (child, parent)
    )
  in
  let tbl = Hashtbl.create (List.length pairs) in
  List.iter (fun (c, p) -> Hashtbl.replace tbl c p) pairs;
  tbl

let load_runtime_env (ic : in_channel) : runtime_env =
  let class_map  = read_class_map ic in
  let impl_map   = read_implementation_map ic in
  let parent_map = read_parent_map ic in
  { class_map; impl_map; parent_map }

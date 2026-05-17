(* @author Trey Rubino *)

open Ast

let rec range k = if k <= 0 then [] else k :: (range (k - 1))
let read ic = input_line ic

let read_list ic worker =
  let k = int_of_string (read ic) in
  List.map (fun _ -> worker ic) (range k)

let rec read_program ic = read_list ic read_cool_class

and read_id ic =
  let loc = read ic in
  let name = read ic in
  (loc, name)

and read_cool_class ic =
  let cname = read_id ic in
  let inherits =
    match read ic with
    | "no_inherits" -> None
    | "inherits" -> Some (read_id ic)
    | x -> failwith ("bad inherits tag: " ^ x)
  in
  let features = read_list ic read_feature in
  (cname, inherits, features)

and read_feature ic =
  match read ic with
  | "attribute_no_init" ->
    let aname = read_id ic in
    let atype = read_id ic in
    Attribute(aname, atype, None)
  | "attribute_init" ->
    let aname = read_id ic in
    let atype = read_id ic in
    let init = read_expr ic in
    Attribute(aname, atype, Some init)
  | "method" ->
    let mname = read_id ic in
    let formals = read_list ic read_formal in
    let mtype = read_id ic in
    let mbody = read_expr ic in
    Method(mname, formals, mtype, mbody)
  | x -> failwith ("bad feature tag: " ^ x)

and read_formal ic =
  let fname = read_id ic in
  let ftype = read_id ic in
  (fname, ftype)

and read_expr_from ic eloc =
  let ekind =
    match read ic with
    | "assign" ->
      let lh = read_id ic in
      let rh = read_expr ic in
      Assign(lh, rh)
    | "dynamic_dispatch" ->
      let recv = read_expr ic in
      let m = read_id ic in
      let args = read_list ic read_expr in
      DynamicDispatch(recv, m, args)
    | "static_dispatch" ->
      let recv = read_expr ic in
      let ty = read_id ic in
      let m = read_id ic in
      let args = read_list ic read_expr in
      StaticDispatch(recv, ty, m, args)
    | "self_dispatch" ->
      let m = read_id ic in
      let args = read_list ic read_expr in
      SelfDispatch(m, args)
    | "if" ->
      let p = read_expr ic in
      let t = read_expr ic in
      let e = read_expr ic in
      If(p, t, e)
    | "while" ->
      let p = read_expr ic in
      let b = read_expr ic in
      While(p, b)
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
      let k = int_of_string (read ic) in
      let rec loop n acc =
        if n = 0 then List.rev acc
        else
          let id1 = read_id ic in
          let id2 = read_id ic in
          let e = read_expr ic in
          loop (n-1) ((id1, id2, e) :: acc)
      in
      let branches = loop k [] in
      Case(scrut, branches)
    | "new" ->
      New(read_id ic)
    | "isvoid" ->
      Isvoid(read_expr ic)
    | "plus" ->
      let lhs = read_expr ic in
      let rhs = read_expr ic in
      Plus(lhs, rhs)
    | "minus"  -> 
      let lhs = read_expr ic in
      let rhs = read_expr ic in
      Minus(lhs, rhs)
    | "times"  -> 
      let lhs = read_expr ic in
      let rhs = read_expr ic in
      Times(lhs, rhs)
    | "divide" ->
      let lhs = read_expr ic in
      let rhs = read_expr ic in
      Divide(lhs, rhs)
    | "lt" -> 
      let lhs = read_expr ic in
      let rhs = read_expr ic in
      Lt(lhs, rhs)
    | "le" -> 
      let lhs = read_expr ic in
      let rhs = read_expr ic in
      Le(lhs, rhs)
    | "eq" -> 
      let lhs = read_expr ic in
      let rhs = read_expr ic in
      Equals(lhs, rhs)
    | "not" -> Not(read_expr ic)
    | "negate" -> Tilde(read_expr ic)
    | "identifier" ->
      Identifier(read_id ic)
    | "integer" ->
      Integer(read ic)
    | "string" ->
      String(read ic)
    | "true" -> True
    | "false" -> False
    | "block" ->
      let k = int_of_string (read ic) in
      let rec gather n acc =
        if n = 0 then List.rev acc
        else gather (n-1) (read_expr ic :: acc)
      in
      Block (gather k [])
    | x -> failwith ("expression kind unhandled: " ^ x)
  in
  { loc = eloc; expr_kind = ekind; static_type = None }

and read_expr ic =
  let eloc = read ic in
  read_expr_from ic eloc

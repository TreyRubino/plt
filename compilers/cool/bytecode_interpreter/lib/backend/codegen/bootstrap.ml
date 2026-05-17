(**
@file   bootstrap.ml
@brief  Reads in .cl-type file from the reference compiler and builds in-memory
        semantic env for the lowering pipeline. Only executed with the '-b' cli
        argument.
@author Trey Rubino
@date   11/30/2025
*)

open Ast
open Semantics

type raw_method_impl = {
  r_definer : string;
  r_formals : string list;
  r_body    : method_body;
}

let parse_static_type s =
  if s = "SELF_TYPE" then Some (SELF_TYPE s)
  else Some (Class s)

(* taking from Weisely Wiemer's youtube series *)
let rec range k = if k <= 0 then [] else k :: (range (k - 1))

let read ic = input_line ic

(** @brief Higher-order function that reads a list of items from the input channel. 
           It first reads an integer 'k', then calls the worker function 'k' times.
    @param ic The input channel.
    @param worker A function that parses a single element of type 'a.
    @return A list of 'a elements parsed from the stream. *)
let read_list ic worker =
  let k = int_of_string (read ic) in
  List.map (fun _ -> worker ic) (range k)

let read_id ic =
  let loc = read ic in
  let name = read ic in
  (loc, name)

(** @brief Recursively reconstructs an AST expression from the serialized input stream. 
           It dispatches based on operation tags to build the tree structure.
    @param ic The input channel.
    @param eloc The source location of the expression.
    @return A fully formed expr record with its associated static type. *)
let rec read_expr_from ic eloc : expr =
  let st = read ic in
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
  { loc = eloc; expr_kind = ekind; static_type = parse_static_type st }

and read_expr ic =
  let eloc = read ic in
  read_expr_from ic eloc

(** @brief Parses the 'class_map' section of the semantic output, which defines 
           the physical layout of every class including inherited attributes.
    @param ic The input channel.
    @return A hash table mapping class names to their list of attributes. *)
let read_class_map ic =
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

(** @brief Parses the 'implementation_map' section, defining exactly which 
           method body is invoked for every class and method name pair.
    @param ic The input channel.
    @return A nested hash table: class_name -> (method_name -> raw_implementation). *)
let read_implementation_map ic =
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
          (mname, { r_definer = definer; r_formals = formals; r_body = body })
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

(** @brief Parses the 'parent_map' section, which explicitly defines the 
           inheritance hierarchy of the COOL program.
    @param ic The input channel.
    @return A hash table mapping child class names to their direct parents. *)
let read_parent_map ic =
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

let read_formal ic =
  let fname = read_id ic in
  let ftype = read_id ic in
  (fname, ftype)

(** @brief Parses a class feature, identifying it as either an attribute 
           (with or without initialization) or a method.
    @param ic The input channel.
    @return The parsed feature variant. *)
let read_feature ic =
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

(** @brief Reconstructs a full COOL class definition, including its name, 
           parent, and list of features (methods and attributes).
    @param ic The input channel.
    @return A tuple containing the class metadata and its feature list. *)
let read_cool_class ic =
  let cname = read_id ic in
  let inherits =
    match read ic with
    | "no_inherits" -> None
    | "inherits" -> Some (read_id ic)
    | x -> failwith ("bad inherits tag: " ^ x)
  in
  let features = read_list ic read_feature in
  (cname, inherits, features)

(** @brief Reads the entire program structure as a list of class definitions.
    @param ic The input channel.
    @return The full list of parsed classes. *)
let read_program ic = read_list ic read_cool_class

(** @brief The primary loader for the Codegen environment durning bootstrap mode. 
          It consumes the serialized output of the Semantic Analyzer to build a 
          'semantic_env'. This environment contains all the mapping and AST data 
          required to generate bytecode and layout the heap.
    @param ic The input channel containing the semantic data.
    @return A semantic_env populated with maps and the program AST. *)
let load_bootstrap_env (ic : in_channel) : semantic_env =
  let raw_class_map = read_class_map ic in
  let raw_impl_map = read_implementation_map ic in
  let raw_parent_map = read_parent_map ic in

  let ast = read_program ic in
  let env = Semantics.empty_env () in

  let rec find_typed_formals cname mname =
    let features =
      List.concat (
        List.map
          (fun ((_, c2), _, fs) -> if c2 = cname then fs else [])
          ast
      )
    in
    match List.find_opt
      (function Method ((_, n), _, _, _) when n = mname -> true | _ -> false)
      features
    with
    | Some (Method (_, formals_ast, _, _)) ->
      formals_ast
    | _ ->
      match Hashtbl.find_opt raw_parent_map cname with
      | Some p when p <> cname -> find_typed_formals p mname
      | _ -> []
  in

  Hashtbl.iter (fun cname attrs -> 
    Hashtbl.replace env.class_map cname attrs
  ) raw_class_map;

  Hashtbl.iter (fun child parent ->
    Hashtbl.replace env.parent_map child parent
  ) raw_parent_map;

  Hashtbl.iter (fun cname methods_kv -> 
    let fixed_methods = Hashtbl.create 31 in
    Hashtbl.iter (fun mname impl ->
      let typed_formals = find_typed_formals impl.r_definer mname in
      let fixed_impl = {
        definer = impl.r_definer;
        formals = typed_formals;
        body    = impl.r_body;
      } in
      Hashtbl.replace fixed_methods mname fixed_impl
    ) methods_kv;
    Hashtbl.replace env.impl_map cname fixed_methods
  ) raw_impl_map;
  
  env
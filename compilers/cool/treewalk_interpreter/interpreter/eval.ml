(*
@author Trey Rubino
@date 09/14/2025
*)

open Ast
open Reader
open Runtime

let string_of_value (v : value) : string =
  match v with
  | VVoid -> "VVoid"
  | VInt i -> "VInt(" ^ string_of_int i ^ ")"
  | VBool b -> "VBool(" ^ string_of_bool b ^ ")"
  | VString s -> "VString(" ^ s ^ ")"
  | VObj o -> "VObj(" ^ o.cls ^ ")"

let int_of_value (loc : string) (v : value) : int = 
  match v with
  | VInt i -> i
  | _ -> runtime_error loc ("arithmetic/comparison on non-Int, got " ^ string_of_value v)

let bool_of_value (loc : string) (v : value) : bool =
  match v with
  | VBool b -> b
  | _ -> runtime_error loc "boolean operation on non-Bool"

let class_of_value (v : value) : string = 
  match v with
  | VInt _ -> "Int"
  | VBool _ -> "Bool"
  | VString _ -> "String"
  | VObj o -> o.cls
  | VVoid -> "Object"

let index_of x lst =
  let rec aux i = function
    | [] -> max_int
    | y :: ys -> if x = y then i else aux (i + 1) ys
  in
  aux 0 lst

let unescape (s : string) : string =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else
      match s.[i] with
      | '\\' when i + 1 < len ->
        (match s.[i+1] with
        | 'n'  -> Buffer.add_char buf '\n'  ; loop (i+2)
        | 't'  -> Buffer.add_char buf '\t'  ; loop (i+2)
        | '"'  -> Buffer.add_char buf '\\'; Buffer.add_char buf '"'; loop (i+2)
        | c    ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c;
          loop (i+2))
      | c ->
        Buffer.add_char buf c; loop (i+1)
  in
  loop 0;
  Buffer.contents buf

let to_int32 (x : int) : int = 
  Int32.to_int (Int32.of_int x)

let dispatch_internal (loc : string) (recv : obj) (qname : string) (args : value list) : value = 
  match qname, args with
  | "Object.abort", _ ->
    Printf.printf "abort\n"; exit 1;
  | "Object.type_name", _ -> 
    VString recv.cls
  | "Object.copy", _ -> 
    let new_fields = Hashtbl.create (Hashtbl.length recv.fields) in
    Hashtbl.iter (fun k cell -> Hashtbl.replace new_fields k (ref !cell)) recv.fields;
    VObj { cls = recv.cls; fields = new_fields; oid = fresh_oid () }
  | "IO.out_string", [VString s] ->
      let unesc = unescape s in
      print_string unesc;
      flush stdout;
      VObj recv
  | "IO.out_int", [VInt i] -> 
    print_int i;
    flush stdout; 
    VObj recv
  | "IO.in_string", _ -> 
    let line = try read_line () with End_of_file -> "" in
    VString line
  | "IO.in_int", _ -> 
    let line = try read_line () with End_of_file -> "0" in
    (try VInt (int_of_string line) with Failure _ -> VInt 0)
  | _ -> runtime_error loc ("internal method not implemented: " ^ qname)

(* hash table to keep track of objects that are being constructed *)
let constructing : (string, obj) Hashtbl.t = Hashtbl.create 8

let rec eval (env : runtime_env) ~(self:obj) ~(scopes:scope list) (e : expr) : value = 
  match e.expr_kind with
  | Integer s -> VInt (int_of_string s)
  | String s  -> VString s
  | True      -> VBool true
  | False     -> VBool false
  | Identifier (loc, name) ->
    (match lookup_value ~self scopes name with
    | Some v -> v
    | None -> runtime_error loc ("unbound identifier " ^ name))
  | Assign ((loc, name), rhs) ->
    if name = "self" then
      runtime_error loc "cannot assign to self"
    else 
      let v = eval env ~self ~scopes rhs in
      (match lookup_lvalue_cell ~self scopes name with
      | Some cell -> 
        cell := v;
        v
      | None -> 
        runtime_error loc ("unbound identifier " ^ name))
  | Plus (lhs, rhs) ->
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    VInt (to_int32 (lhs_i + rhs_i))
  | Minus (lhs, rhs) -> 
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    VInt (to_int32 (lhs_i - rhs_i))
  | Times (lhs, rhs) -> 
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    VInt (to_int32 (lhs_i * rhs_i))
  | Divide (lhs, rhs) -> 
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    if rhs_i = 0 then runtime_error e.loc "division by zero"
    else VInt (to_int32 (lhs_i / rhs_i))
  | Lt (lhs, rhs) ->
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    VBool (lhs_i < rhs_i)
  | Le (lhs, rhs) ->
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    VBool (lhs_i <= rhs_i)
  | Equals (lhs, rhs) ->
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    (match (lhs_v, rhs_v) with
    | VInt i1, VInt i2 -> VBool (i1 = i2)
    | VBool b1, VBool b2 -> VBool (b1 = b2)
    | VString s1, VString s2 -> VBool (s1 = s2) (* structural eqaulity *)
    | VObj o1, VObj o2 -> VBool (o1 == o2) (* obj reference equality *)
    | VVoid, _ | _, VVoid -> VBool false
    | _ -> VBool (lhs_v == rhs_v)) (* fallback to reference equality *)
  | Isvoid expr ->
    let expr_v = eval env ~self ~scopes expr in
    (match expr_v with
    | VVoid -> VBool true
    | _ -> VBool false)
  | Tilde expr ->
    let expr_v = eval env ~self ~scopes expr in
    let expr_i = int_of_value e.loc expr_v in 
    VInt (to_int32 (-expr_i))
  | Not expr -> 
    let expr_v = eval env ~self ~scopes expr in
    let expr_b = bool_of_value e.loc expr_v in
    VBool (not expr_b)
  | Block exprs -> 
    let rec eval_list (exprs : expr list) : value = 
      match exprs with
      | [] -> VVoid
      | [last] -> eval env ~self ~scopes last
      | head :: tail -> 
        ignore (eval env ~self ~scopes head); 
        eval_list tail
    in
    eval_list exprs
  | If (pred, then_br, else_br) -> 
    let pred_v = eval env ~self ~scopes pred in
    let pred_b = bool_of_value e.loc pred_v in 
    if pred_b then eval env ~self ~scopes then_br
    else eval env ~self ~scopes else_br
  | While (pred, body) -> 
    let rec loop () = 
      let pred_v = eval env ~self ~scopes pred in
      let pred_b = bool_of_value pred.loc pred_v in
      if pred_b then (
        ignore (eval env ~self ~scopes body); 
        loop ()
      ) else VVoid
    in 
    loop ()
  | Let (bindings, body) ->
    let scope = new_scope () in
    let scopes' = push_scope scope scopes in
    List.iter (fun ((loc, name), (_, ty), init_opt) -> 
      let v = 
        match init_opt with
        | Some init -> eval env ~self ~scopes:scopes' init
        | None -> default_of_type ty
      in 
      bind_local scopes' name v 
    ) bindings; 
    let result = eval env ~self ~scopes:scopes' body in
    result
  | Case (scrut, branches) ->
    let scrut_v = eval env ~self ~scopes scrut in
    (match scrut_v with
    | VVoid ->
      runtime_error scrut.loc "case on void"
    | _ ->
      let dynamic_cls = class_of_value scrut_v in
      let ancestry_list = ancestry env.parent_map dynamic_cls in
      (* all branches whose declared type is an ancestor of the dynamic class *)
      let matching =
        List.filter (fun (_, (_, ty), _) ->
          List.exists ((=) ty) ancestry_list
        ) branches
      in
      (* pick the branch with the smallest ancestry "distance" *)
      let chosen =
        match matching with
        | [] -> None
        | _ ->
          Some (
            List.hd
              (List.sort
                (fun (_, (_, t1), _) (_, (_, t2), _) ->
                  compare (index_of t1 ancestry_list) (index_of t2 ancestry_list))
                matching)
          )
      in
      match chosen with
      | None ->
        runtime_error e.loc ("no matching case branch for " ^ dynamic_cls)
      | Some ((_, name), _, body) ->
        let scope = new_scope () in
        let scopes' = push_scope scope scopes in
        bind_local scopes' name scrut_v;
        eval env ~self ~scopes:scopes' body)
  | New (_loc, ty) ->
    let cls = if ty = "SELF_TYPE" then self.cls else ty in
    if Hashtbl.mem constructing cls then (
      VObj (Hashtbl.find constructing cls)
    ) else (
      let obj = new_object_defaults env cls in
      Hashtbl.add constructing cls obj;
      run_initializers env obj ~scopes;
      Hashtbl.remove constructing cls;
      VObj obj
    )
  | DynamicDispatch (recv, (_, mname), args) -> 
    let recv_v = eval env ~self ~scopes recv in
    (match recv_v with
    | VVoid -> runtime_error e.loc "dispatch on void"
    | VObj o -> 
      let args_v = List.map (eval env ~self ~scopes) args in
      (match lookup_method env o.cls mname with
      | Some impl -> call_method env ~recv:o ~scopes impl args_v
      | None -> runtime_error e.loc ("method not found: " ^ mname))
    | VString s ->
      let args_v = List.map (eval env ~self ~scopes) args in
      (match mname, args_v with
      | "length", [] -> VInt (String.length s)
      | "concat", [VString s2] -> VString (s ^ s2)
      | "substr", [VInt i; VInt l] ->
        if i < 0 || l < 0 || i + l > String.length s then
          runtime_error "0" "String.substr out of range"
        else VString (String.sub s i l)
      | "type_name", _ -> VString (class_of_value recv_v)
      | "copy", _ -> recv_v
      | "abort", _ -> Printf.printf "abort\n"; exit 1
      | _ -> runtime_error e.loc ("string method not implemented: " ^ mname))
    | VInt _ | VBool _ ->
      let _ = List.map (eval env ~self ~scopes) args in
      (match mname with
      | "type_name" -> VString (class_of_value recv_v)
      | "copy" -> recv_v
      | "abort" -> Printf.printf "abort\n"; exit 1
      | _ -> runtime_error e.loc ("method not implemented for " ^ class_of_value recv_v)))
  | StaticDispatch (recv, (_, ty), (_, mname), args) -> 
    let recv_v = eval env ~self ~scopes recv in
    (match recv_v with
    | VVoid -> runtime_error e.loc "dispatch on void"
    | VObj o -> 
      let args_v = List.map (eval env ~self ~scopes) args in
      (match lookup_method env ty mname with
      |Some impl -> call_method env ~recv:o ~scopes impl args_v
      | None -> runtime_error e.loc ("method not found: " ^ mname))
    | VString s ->
      let args_v = List.map (eval env ~self ~scopes) args in
      (match mname, args_v with
      | "length", [] -> VInt (String.length s)
      | "concat", [VString s2] -> VString (s ^ s2)
      | "substr", [VInt i; VInt l] ->
        if i < 0 || l < 0 || i + l > String.length s then
          runtime_error "0" "String.substr out of range"
        else VString (String.sub s i l)
      | "type_name", _ -> VString (class_of_value recv_v)
      | "copy", _ -> recv_v
      | "abort", _ -> Printf.printf "abort\n"; exit 1
      | _ -> runtime_error e.loc ("string method not implemented: " ^ mname))
    | VInt _ | VBool _ ->
      let _ = List.map (eval env ~self ~scopes) args in
      (match mname with
      | "type_name" -> VString (class_of_value recv_v)
      | "copy" -> recv_v
      | "abort" -> Printf.printf "abort\n"; exit 1
      | _ -> runtime_error e.loc ("method not implemented for " ^ class_of_value recv_v)))
  | SelfDispatch ((_, mname), args) -> 
    let args_v = List.map (eval env ~self ~scopes) args in
    (match lookup_method env self.cls mname with 
    | Some impl -> call_method env ~recv:self ~scopes impl args_v
    | None -> runtime_error e.loc ("method not found: " ^ mname))

and run_initializers (env : runtime_env) (obj : obj) ~(scopes:scope list) : unit = 
  attributes_linearized env obj.cls
  |> List.iter (fun { aname; init; _} -> 
    match init with
    | None -> ()
    | Some expr -> 
      let expr_v = eval env ~self:obj ~scopes expr in
      let cell = Hashtbl.find obj.fields aname in
      cell := expr_v
  )
  
and call_method (env : runtime_env) ~(recv:obj) ~(scopes:scope list) (impl : method_impl) (args : value list) : value = 
  match impl.body with
  | User body -> 
    let scope = new_scope () in
    let scopes' = push_scope scope [] in
    List.iter2 (fun formal arg -> 
      bind_local scopes' formal arg
    ) impl.formals args;
    eval env ~self:recv ~scopes:scopes' body
  | Internal { qname; _ } -> 
    dispatch_internal "0" recv qname args
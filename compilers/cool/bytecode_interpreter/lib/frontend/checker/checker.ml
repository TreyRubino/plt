(*
@author Trey Rubino
@date 11/14/2025
*)

open Ast
open Env
open Typecheck
open Validate
open Semantics

let check (ast : Ast.cool_program) = 
  (
    let base_classes = [ "Int"; "String"; "Bool"; "IO"; "Object" ] in
    dups_base_validation ~base_classes ast;

    let user_classes = List.map (fun ((_, cname), _, _) -> cname) ast in
    let all_classes = List.sort compare (base_classes @ user_classes) in

    let parent_pairs =
      let builtins = [
        ("Bool",   "Object");
        ("IO",     "Object");
        ("Int",    "Object");
        ("String", "Object");
      ] in
      let user_pairs =
        List.map (fun ((_, cname), inherits, _) ->
          match inherits with
          | None -> (cname, "Object")
          | Some ((_, pname)) -> (cname, pname)
        ) ast
      in
      builtins @ user_pairs
    in
    Hashtbl.clear parent_map;
    List.iter (fun (c, p) ->
      Hashtbl.replace parent_map c p
    ) parent_pairs;

    parent_validation ~all_classes ast;
    decl_types_validation ~all_classes ast;
    main_validation ast;

    seed_builtins ();
    seed_user_methods ast;
    override_validation ast;
    seed_user_attributes ast;
    names_scoping_validation ast;

    List.iter (fun ((_, cname), _inherits, features) ->
      type_check_class cname ((("", cname)), None, features)
    ) ast;

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
        match Hashtbl.find_opt parent_map cname with
        | Some p when p <> cname -> find_typed_formals p mname
        | _ -> []
    in

    (* class map *)
    List.iter (fun cname ->
      let attrs_tbl =
        match Hashtbl.find_opt attribute_env cname with
        | Some t -> t
        | None -> Hashtbl.create 1
      in
      
      let class_features = 
        List.concat (List.map (fun ((_, c), _, f) -> 
          if c = cname then f else []
        ) ast)
      in
      
      let attrs =
        List.filter_map (function
          | Attribute ((_, aname), _, init_opt) ->
              (match Hashtbl.find_opt attrs_tbl aname with
              | Some ty -> Some { aname; atype = ty; init = init_opt }
              | None -> None)
          | _ -> None
        ) class_features
      in
      Hashtbl.replace env.class_map cname attrs
    ) all_classes;

    (* impl_map *)
    List.iter (fun cname ->
      let tbl = Hashtbl.create 31 in
      let rec gather c =
        match Hashtbl.find_opt method_env c with
        | Some defs ->
          Hashtbl.iter (fun mname sig_ ->
            if not (Hashtbl.mem tbl mname) then
              let body =
                match List.find_opt (function
                  | Method ((_loc, n), _, _, _) when n = mname -> true
                  | _ -> false
                ) (List.concat (List.map (fun ((_, c2), _, f) -> if c2 = c then f else []) ast))
                with
                | Some (Method (_, _, _, b)) -> User b
                | _ -> Internal { rtype = sig_.ret; qname = c ^ "." ^ mname }
              in
              let typed_formals = find_typed_formals sig_.definer mname in
              let impl = {
                definer = sig_.definer;
                formals = typed_formals;
                body;
              } in
              Hashtbl.replace tbl mname impl
          ) defs;
          (match Hashtbl.find_opt parent_map c with
            | Some p when p <> c -> gather p
            | _ -> ())
        | None ->
          match Hashtbl.find_opt parent_map c with
          | Some p when p <> c -> gather p
          | _ -> ()
      in
      gather cname;
      Hashtbl.replace env.impl_map cname tbl
    ) all_classes;

    (* parent map *)
    Hashtbl.iter (fun c p ->
      Hashtbl.replace env.parent_map c p
    ) parent_map;
    env
  );;

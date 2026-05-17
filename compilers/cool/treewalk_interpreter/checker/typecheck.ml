(* @author Trey Rubino *)

open Ast
open Env

type object_env = (string, static_type) Hashtbl.t
let empty_object_env () = Hashtbl.create 255

let class_exists (t : string) : bool = 
  t = "Object" || Hashtbl.mem parent_map t

let string_of_type t =
  match t with
  | Class s -> s
  | SELF_TYPE s -> "SELF_TYPE(" ^ s ^ ")"

let rec lub (t1 : static_type) (t2 : static_type) (self_cls : string) : static_type =
  (* if both branches are SELF_TYPE, the join is SELF_TYPE *)
  match t1, t2 with
  | SELF_TYPE _, SELF_TYPE _ -> SELF_TYPE self_cls
  | _ ->
    let norm = function SELF_TYPE _ -> Class self_cls | x -> x in
    match norm t1, norm t2 with
    | Class a, Class b ->
      let rec climb_a ca =
        if is_subtype (Class b) (Class ca) then Class ca
        else
          let p = try Hashtbl.find parent_map ca with Not_found -> "Object" in
          if ca = "Object" then Class "Object" else climb_a p
      in
      climb_a a
    | _ -> Class "Object"

let rec type_check (current_class : string) (o : object_env) (expr : expr) : static_type =
	let check (exprs : expr list) (expected : string) =
		List.iter (fun e ->
			match type_check current_class o e with
			| Class t when t = expected -> ()
			| Class x -> (Printf.printf "ERROR: %s: Type-Check: comparison/arithmetic on type %s instead of type %s\n" expr.loc x expected; exit 1)
			| SELF_TYPE _ ->
				(* SELF_TYPE not allowed for Int/String/Bool ops *)
				if expected = "Int" || expected = "Bool" || expected = "String"
				then (Printf.printf "ERROR: %s: Type-Check: SELF_TYPE not allowed where %s expected\n" expr.loc expected; exit 1)
		) exprs;
		Class expected
	in
  (* static type *)
	let st =
		match expr.expr_kind with
		| Integer _ -> Class "Int"
		| String _ -> Class "String"
		| True | False -> Class "Bool"

		| Identifier ((vloc, vname)) ->
			if Hashtbl.mem o vname then Hashtbl.find o vname
			else (Printf.printf "ERROR: %s: Type-Check: undeclared variable %s\n" vloc vname; exit 1)

		| Assign ((vloc, vname), e) ->
      if vname = "self" then (
        Printf.printf "ERROR: %s: Type-Check: cannot assign to self\n" vloc;
        exit 1
      );
			if Hashtbl.mem o vname then
				let declared = Hashtbl.find o vname in
				let et = type_check current_class o e in
				if is_subtype et declared then et
        else (Printf.printf "ERROR: %s: Type-Check: assignment on type %s does not conform to type %s\n" expr.loc (string_of_type et) (string_of_type declared); exit 1)
			else (Printf.printf "ERROR: %s: Type-Check: undeclared variable %s\n" vloc vname; exit 1)

		| Plus (x, y)   -> check [x; y] "Int"
		| Minus (x, y)  -> check [x; y] "Int"
		| Times (x, y)  -> check [x; y] "Int"
		| Divide (x, y) -> check [x; y] "Int"
		| Tilde x       -> check [x]   "Int"
		| Lt (x, y)     -> ignore (check [x; y] "Int"); Class "Bool"
		| Le (x, y)     -> ignore (check [x; y] "Int"); Class "Bool"

		| Equals (x, y) ->
			let t1 = type_check current_class o x in
			let t2 = type_check current_class o y in
			if is_primitive t1 || is_primitive t2 then
				if t1 <> t2 then (
          Printf.printf "ERROR: %s: Type-Check: equality on primitive types requires identical types. Got %s and %s\n" expr.loc (string_of_type t1) (string_of_type t2); exit 1
        );
			Class "Bool"

		| Not e1 ->
			ignore (check [e1] "Bool"); Class "Bool"

		| Isvoid e1 ->
			ignore (type_check current_class o e1); Class "Bool"

		| If (p, tbr, ebr) ->
			let pt = type_check current_class o p in
			if pt <> Class "Bool" then (
        Printf.printf "ERROR: %s: Type-Check: conditional has type %s instead of Bool\n" expr.loc (string_of_type pt); exit 1
      );
			let tt = type_check current_class o tbr in
			let et = type_check current_class o ebr in
			lub tt et current_class

		| While (p, b) ->
			let pt = type_check current_class o p in
			if pt <> Class "Bool" then (
        Printf.printf "ERROR: %s: Type-Check: predicate has type %s instead of Bool\n" expr.loc (string_of_type pt); exit 1
      );
			ignore (type_check current_class o b);
			Class "Object"

    | Let (bindings, body) ->
      let lloc = expr.loc in 
      let cleanup = ref [] in
      List.iter (fun ((vloc, vname), (tloc, tname), init_opt) ->
        (* name / type legality *)
        if vname = "self" then (
          Printf.printf "ERROR: %s: Type-Check: cannot bind 'self' in let\n" vloc;
          exit 1
        );
        if tname <> "SELF_TYPE" && not (class_exists tname) then (
          Printf.printf "ERROR: %s: Type-Check: unknown type %s\n" tloc tname;
          exit 1
        );
        (match init_opt with
        | None -> ()
        | Some init ->
          let it = type_check current_class o init in
          let declared = if tname = "SELF_TYPE" then SELF_TYPE current_class else Class tname in
          if not (is_subtype it declared) then (
            Printf.printf "ERROR: %s: Type-Check: initializer type %s does not conform to type %s\n" lloc (string_of_type it) tname; exit 1 
          ););
          Hashtbl.add o vname (if tname = "SELF_TYPE" then SELF_TYPE current_class else Class tname);
        cleanup := vname :: !cleanup;
      ) bindings;
      let bt = type_check current_class o body in
      List.iter (fun vname -> Hashtbl.remove o vname) !cleanup;
      bt

    | Case (scrut, branches) ->
      ignore (type_check current_class o scrut);

      let seen = Hashtbl.create 31 in
      let acc : static_type option ref = ref None in

      List.iter (fun ((vloc, vname), (tl, tname), br) ->
        (* cannot bind self *)
        if vname = "self" then (
          Printf.printf "ERROR: %s: Type-Check: cannot bind 'self' in case branch\n" vloc; exit 1
        );
        if tname = "SELF_TYPE" then (
          Printf.printf "ERROR: %s: Type-Check: SELF_TYPE not allowed as case branch type\n" tl; 
          exit 1
        );  
        (* branch types must be distinct *)
        if Hashtbl.mem seen tname then (
          Printf.printf "ERROR: %s: Type-Check: case branch type %s is bound twice\n" vloc tname; exit 1
        ) else Hashtbl.add seen tname true;

        if not (class_exists tname) then (
          Printf.printf "ERROR: %s: Type-Check: unknown type %s\n" tl tname;
          exit 1
        );
        Hashtbl.add o vname (Class tname);
        let bt = type_check current_class o br in
        Hashtbl.remove o vname;

        (* accumulate LUB across branches *)
        acc := (match !acc with
          | None -> Some bt
          | Some sofar -> Some (lub sofar bt current_class))
      ) branches;

      (match !acc with
       | None -> Class "Object"  
       | Some t -> t)

		| New ((tloc, tname)) ->
			if tname = "SELF_TYPE" then SELF_TYPE current_class 
      else (
        if not (class_exists tname) then (
          Printf.printf "ERROR: %s: Type-Check: unknown type %s\n" tloc tname;
          exit 1
        );
        Class tname
      )

		| SelfDispatch ((mloc, mname), args) ->
			let recv_cls = current_class in
			(match lookup_method_sig recv_cls mname with
			| None ->
				Printf.printf "ERROR: %s: Type-Check: unknown method %s on %s\n" mloc mname recv_cls; exit 1
			| Some sign ->
				if List.length sign.formals <> List.length args then (
          Printf.printf "ERROR: %s: Type-Check: method '%s' expected %d arguments, but %d was given\n" expr.loc mname (List.length sign.formals) (List.length args); exit 1
        );
				List.iter2 (fun a ft ->
					let at = type_check current_class o a in
					if not (is_subtype at (Class ft)) then (
            Printf.printf "ERROR: %s: Type-Check: method '%s' expected type %s got %s\n" expr.loc mname ft (string_of_type at); exit 1 
          );
				) args sign.formals;
				if sign.ret = "SELF_TYPE" then SELF_TYPE recv_cls else Class sign.ret)

		| DynamicDispatch (recv, (mloc, mname), args) ->
			let rt = type_check current_class o recv in
			let (rc : string) =
				match rt with
				| SELF_TYPE c -> c
				| Class c -> c
			in
			(match lookup_method_sig rc mname with
			| None ->
				Printf.printf "ERROR: %s: Type-Check: unknown method %s on %s\n" mloc mname rc; exit 1
			| Some sign ->
				if List.length sign.formals <> List.length args then (
          Printf.printf "ERROR: %s: Type-Check: method '%s' expected %d arguments, but %d was given\n" expr.loc mname (List.length sign.formals) (List.length args); exit 1
        );
				List.iter2 (fun a ft ->
					let at = type_check current_class o a in
					if not (is_subtype at (Class ft)) then (
            Printf.printf "ERROR: %s: Type-Check: method '%s' expected type %s got %s\n" expr.loc mname ft (string_of_type at); exit 1 
          );  
				) args sign.formals;
				(match sign.ret with
				| "SELF_TYPE" ->
					(* result type is the dynamic receiver type *)
					rt
				| r -> Class r))

		| StaticDispatch (recv, (tl, tname), (mloc, mname), args) ->
			let rt = type_check current_class o recv in
      if tname = "SELF_TYPE" then (
        Printf.printf "ERROR: %s: Type-Check: SELF_TYPE not allowed as static dispatch annotation\n" tl;
        exit 1
      );
      if not (class_exists tname) then (
        Printf.printf "ERROR: %s: Type-Check: unknown type %s\n" tl tname;
        exit 1
      );
			let ann = tname in
			(* receiver must be subtype of annotated type *)
			if not (is_subtype rt (Class ann)) then (
        Printf.printf "ERROR: %s: Type-Check: receiver type expected %s got %s\n" expr.loc ann (string_of_type rt); exit 1
      );  
			(match lookup_method_sig ann mname with
			| None ->
				Printf.printf "ERROR: %s: Type-Check: unknown method %s on %s\n" mloc mname ann; exit 1
			| Some sign ->
				if List.length sign.formals <> List.length args then (
          Printf.printf "ERROR: %s: Type-Check: method '%s' expected %d arguments, but %d was given\n" expr.loc mname (List.length sign.formals) (List.length args); exit 1
        );
				List.iter2 (fun a ft ->
					let at = type_check current_class o a in
					if not (is_subtype at (Class ft)) then (
            Printf.printf "ERROR: %s: Type-Check: method '%s' expected type %s got %s\n" expr.loc mname ft (string_of_type at); exit 1 
          );
				) args sign.formals;
				if sign.ret = "SELF_TYPE" then
					(* static dispatch to A::m has SELF_TYPE *)
					Class ann
				else Class sign.ret)
    | Block es ->
      let rec go = function
        | [] -> Class "Object"      
        | [last] -> type_check current_class o last
        | h::t -> ignore (type_check current_class o h); go t
      in
      go es
	in
	expr.static_type <- Some st;
	st

let type_check_class (cname : string) ((_id, _), _inherits, features) =
  List.iter (fun feat ->
    match feat with
    | Attribute ((aloc, aname), (_tl, tname), init_opt) ->
      (match init_opt with
      | None -> ()
      | Some e ->
        let o = empty_object_env () in
        Hashtbl.add o "self" (SELF_TYPE cname);

        let attrs = collect_attributes cname in
        Hashtbl.iter (fun an tyname ->
          if an <> "self" then
            Hashtbl.replace o an (if tyname = "SELF_TYPE" then SELF_TYPE cname else Class tyname)
        ) attrs;

        (* type-check init and enforce conformance to declared type *)
        let it = type_check cname o e in
        let declared = if tname = "SELF_TYPE" then SELF_TYPE cname else Class tname in
        if not (is_subtype it declared) then (Printf.printf "ERROR: %s: Type-Check: %s does not conform to %s in initialized attribute\n" aloc (string_of_type it) (string_of_type declared); exit 1))

    | Method ((mloc, _mname), formals, (rtloc, rtype), body) ->
      let o = empty_object_env () in
      Hashtbl.add o "self" (SELF_TYPE cname);

      let attrs = collect_attributes cname in
      Hashtbl.iter (fun an tyname ->
        if an <> "self" then
          Hashtbl.replace o an (if tyname = "SELF_TYPE" then SELF_TYPE cname else Class tyname)
      ) attrs;

      List.iter (fun ((_fl,fname), (_tl,tname)) ->
        Hashtbl.add o fname (if tname = "SELF_TYPE" then SELF_TYPE cname else Class tname)
      ) formals;

      let bt = type_check cname o body in
      let declared = if rtype = "SELF_TYPE" then SELF_TYPE cname else Class rtype in
      if not (is_subtype bt declared) then (
        Printf.printf "ERROR: %s: Type-Check: method body type does not conform to declared return\n" mloc;
        exit 1
      )
  ) features


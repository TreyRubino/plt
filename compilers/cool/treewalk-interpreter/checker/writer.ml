(* @author Trey Rubino *)

open Ast
open Env

let rec output_expr (fout : out_channel) (e : expr) =
	Printf.fprintf fout "%s\n" e.loc;
	(match e.static_type with
	| None -> failwith "we forgot to do typechecking."
	| Some (Class c) -> Printf.fprintf fout "%s\n" c
	| Some (SELF_TYPE _) -> Printf.fprintf fout "SELF_TYPE\n");
	match e.expr_kind with
	| Assign ((l, n), rhs) ->
		Printf.fprintf fout "assign\n%s\n%s\n" l n;
		output_expr fout rhs
	| DynamicDispatch (recv, (ml, mn), args) ->
		Printf.fprintf fout "dynamic_dispatch\n";
		output_expr fout recv;
		Printf.fprintf fout "%s\n%s\n" ml mn;
		Printf.fprintf fout "%d\n" (List.length args);
		List.iter (output_expr fout) args
	| StaticDispatch (recv, (tl, tn), (ml, mn), args) ->
		Printf.fprintf fout "static_dispatch\n";
		output_expr fout recv;
		Printf.fprintf fout "%s\n%s\n" tl tn;
		Printf.fprintf fout "%s\n%s\n" ml mn;
		Printf.fprintf fout "%d\n" (List.length args);
		List.iter (output_expr fout) args
	| SelfDispatch ((ml, mn), args) ->
		Printf.fprintf fout "self_dispatch\n";
		Printf.fprintf fout "%s\n%s\n" ml mn;
		Printf.fprintf fout "%d\n" (List.length args);
		List.iter (output_expr fout) args
  | Let (bindings, body) ->
    Printf.fprintf fout "let\n";
    Printf.fprintf fout "%d\n" (List.length bindings);
    List.iter (function
      | (vloc, vname), (tloc, tname), None ->
        Printf.fprintf fout "let_binding_no_init\n";
        Printf.fprintf fout "%s\n%s\n" vloc vname;
        Printf.fprintf fout "%s\n%s\n" tloc tname
      | (vloc, vname), (tloc, tname), Some init ->
        Printf.fprintf fout "let_binding_init\n";
        Printf.fprintf fout "%s\n%s\n" vloc vname;
        Printf.fprintf fout "%s\n%s\n" tloc tname;
        output_expr fout init
    ) bindings;
    output_expr fout body
	| Case (scrut, branches) ->
		Printf.fprintf fout "case\n";
		output_expr fout scrut;
		Printf.fprintf fout "%d\n" (List.length branches);
		List.iter (fun ((vl, vn), (tl, tn), br) ->
			Printf.fprintf fout "%s\n%s\n" vl vn;
			Printf.fprintf fout "%s\n%s\n" tl tn;
			output_expr fout br
		) branches
	| If (p, t, e2) ->
		Printf.fprintf fout "if\n"; output_expr fout p; output_expr fout t; output_expr fout e2
	| While (p, b) ->
		Printf.fprintf fout "while\n"; output_expr fout p; output_expr fout b
	| New ((l, c)) ->
		Printf.fprintf fout "new\n%s\n%s\n" l c
	| Isvoid e1 ->
		Printf.fprintf fout "isvoid\n" ; output_expr fout e1
	| Plus (x, y)   -> Printf.fprintf fout "plus\n";   output_expr fout x; output_expr fout y
	| Minus (x, y)  -> Printf.fprintf fout "minus\n";  output_expr fout x; output_expr fout y
	| Times (x, y)  -> Printf.fprintf fout "times\n";  output_expr fout x; output_expr fout y
	| Divide (x, y) -> Printf.fprintf fout "divide\n"; output_expr fout x; output_expr fout y
	| Lt (x, y)     -> Printf.fprintf fout "lt\n";     output_expr fout x; output_expr fout y
	| Le (x, y)     -> Printf.fprintf fout "le\n";     output_expr fout x; output_expr fout y
	| Equals (x, y) -> Printf.fprintf fout "eq\n";     output_expr fout x; output_expr fout y
	| Not e1        -> Printf.fprintf fout "not\n";    output_expr fout e1
	| Tilde e1      -> Printf.fprintf fout "negate\n"; output_expr fout e1
	| Identifier (l, n) ->
		Printf.fprintf fout "identifier\n%s\n%s\n" l n
	| Integer lit  ->
		Printf.fprintf fout "integer\n%s\n" lit
	| String s     ->
		Printf.fprintf fout "string\n%s\n" s
	| True         -> Printf.fprintf fout "true\n"
	| False        -> Printf.fprintf fout "false\n"
  | Block es ->
    Printf.fprintf fout "block\n";
    Printf.fprintf fout "%d\n" (List.length es);
    List.iter (output_expr fout) es

let features_of (ast : cool_program) cname =
	try
		let _, _, feats = List.find (fun ((_, c2), _, _) -> c2 = cname) ast in
		feats
	with Not_found -> []

let parent_of (ast : cool_program) cname =
	if cname = "Object" then None
	else if List.mem cname ["Int"; "Bool"; "String"; "IO"] then Some "Object"
	else
		try
			let _, inh, _ = List.find (fun ((_, c2), _, _) -> c2 = cname) ast in
			match inh with
			| None -> Some "Object"
			| Some ((_, p)) -> Some p
		with Not_found -> None

let rec attributes_of (ast : cool_program) (cname : string)
  : (string * string * expr option) list =
  let inherited =
    match parent_of ast cname with
    | None -> []
    | Some p -> attributes_of ast p
  in
  let own =
    features_of ast cname
    |> List.filter_map (function
      | Attribute ((_, aname), (_, atype), init_opt) -> Some (aname, atype, init_opt)
      | Method _ -> None)
  in
  inherited @ own

let internal_methods cname =
	let sort_by_name lst = List.sort (fun (n1,_,_,_,_,_) (n2,_,_,_,_,_) -> compare n1 n2) lst in
	match cname with
	| "Object" ->
		sort_by_name [
			("abort",      [],         "Object",    "Object", None, true);
			("copy",       [],         "SELF_TYPE", "Object", None, true);
			("type_name",  [],         "String",    "Object", None, true)
		]
	| "IO" ->
		sort_by_name [
			("in_int",     [],         "Int",       "IO", None, true);
			("in_string",  [],         "String",    "IO", None, true);
			("out_int",    ["x"],      "SELF_TYPE", "IO", None, true);
			("out_string", ["x"],      "SELF_TYPE", "IO", None, true)
		]
	| "String" ->
		sort_by_name [
			("concat",     ["s"],      "String",    "String", None, true);
			("length",     [],         "Int",       "String", None, true);
			("substr",     ["i"; "l"],  "String",    "String", None, true)
		]
	| _ -> []

let user_methods (ast:cool_program) cname =
	let feats = features_of ast cname in
	let rec collect acc = function
		| [] -> List.rev acc
		| Attribute _ :: tl -> collect acc tl
		| Method ((_, mname), formals, (_, rtype), mbody) :: tl ->
			let fnames = List.map (fun ((_, fname), _) -> fname) formals in
			collect ((mname, fnames, rtype, cname, Some mbody, false) :: acc) tl
	in
	collect [] feats

let rec methods_of ast cname =
	let inherited =
		match parent_of ast cname with
		| None -> []
		| Some p -> methods_of ast p
	in
	let own_user = user_methods ast cname in
	let overlay acc (n, fns, rt, def, body_opt, is_int) =
		let rec replace = function
			| [] -> [ (n, fns, rt, def, body_opt, is_int) ]
			| ((n2,_,_,_,_,_)) :: tl when n2 = n -> (n, fns, rt, def, body_opt, is_int) :: tl
			| e :: tl -> e :: replace tl
		in
		if List.exists (fun (n2,_,_,_,_,_) -> n2 = n) acc then replace acc
		else acc @ [ (n, fns, rt, def, body_opt, is_int) ]
	in
	let with_overrides = List.fold_left overlay inherited own_user in
	let own_internal = internal_methods cname in
	List.fold_left overlay with_overrides own_internal

let write_all (fname : string) (ast : cool_program) (all_classes : string list) (parent_pairs : (string * string) list) =
	let fout = open_out fname in

  (* class_map *)
  Printf.fprintf fout "class_map\n%d\n" (List.length all_classes);
  List.iter (fun cname ->
    Printf.fprintf fout "%s\n" cname;
    let attributes = attributes_of ast cname in
    Printf.fprintf fout "%d\n" (List.length attributes);
    List.iter (fun (aname, atype, init_opt) ->
      match init_opt with
      | None ->
        Printf.fprintf fout "no_initializer\n%s\n%s\n" aname atype
      | Some init ->
        Printf.fprintf fout "initializer\n%s\n%s\n" aname atype;
        output_expr fout init
    ) attributes
  ) all_classes;

	(* implementation_map *)
	Printf.fprintf fout "implementation_map\n";
	Printf.fprintf fout "%d\n" (List.length all_classes);
	List.iter (fun cname ->
		Printf.fprintf fout "%s\n" cname;
		let methods = methods_of ast cname in
		Printf.fprintf fout "%d\n" (List.length methods);
		List.iter (fun (mname, fnames, rtype, definer, body_opt, is_internal) ->
			Printf.fprintf fout "%s\n" mname;
			Printf.fprintf fout "%d\n" (List.length fnames);
			List.iter (fun fn -> Printf.fprintf fout "%s\n" fn) fnames;
			Printf.fprintf fout "%s\n" definer;
			match is_internal, body_opt with
			| true, _ ->
				Printf.fprintf fout "0\n%s\ninternal\n%s.%s\n" rtype definer mname
			| false, Some body ->
				output_expr fout body
			| _ ->
				Printf.fprintf fout "0\n%s\ninternal\n%s.%s\n" rtype definer mname
		) methods
	) all_classes;

	(* parent_map *)
	Printf.fprintf fout "parent_map\n";
	Printf.fprintf fout "%d\n" ((List.length all_classes) - 1);
	let sorted_pairs = List.sort compare parent_pairs in
	List.iter (fun (child, parent) ->
		Printf.fprintf fout "%s\n%s\n" child parent
	) sorted_pairs;

  (* annotated_ast *)
	Printf.fprintf fout "%d\n" (List.length ast);
	List.iter (fun ((cloc, cname), inherits, features) ->
		(* class header *)
		Printf.fprintf fout "%s\n%s\n" cloc cname;
		(match inherits with
		| None ->
			Printf.fprintf fout "no_inherits\n"
		| Some (ploc, pname) ->
			Printf.fprintf fout "inherits\n";
			Printf.fprintf fout "%s\n%s\n" ploc pname);
		(* features *)
		Printf.fprintf fout "%d\n" (List.length features);
		List.iter (function
			| Attribute ((aloc, aname), (tloc, tname), None) ->
				Printf.fprintf fout "attribute_no_init\n";
				Printf.fprintf fout "%s\n%s\n" aloc aname;
				Printf.fprintf fout "%s\n%s\n" tloc tname
			| Attribute ((aloc, aname), (tloc, tname), Some init) ->
				Printf.fprintf fout "attribute_init\n";
				Printf.fprintf fout "%s\n%s\n" aloc aname;
				Printf.fprintf fout "%s\n%s\n" tloc tname;
				output_expr fout init
			| Method ((mloc, mname), formals, (rtloc, rtype), body) ->
				Printf.fprintf fout "method\n";
				Printf.fprintf fout "%s\n%s\n" mloc mname;
				Printf.fprintf fout "%d\n" (List.length formals);
				List.iter (fun ((floc, fname),(ftloc, ftname)) ->
					Printf.fprintf fout "%s\n%s\n" floc fname;
					Printf.fprintf fout "%s\n%s\n" ftloc ftname
				) formals;
				Printf.fprintf fout "%s\n%s\n" rtloc rtype;
				output_expr fout body
		) features
	) ast;

	close_out fout

(* @author Trey Rubino *)

open Ast

let parent_map : (string, string) Hashtbl.t = Hashtbl.create 255

type method_sig = {
	formals :  string list;
	ret :      string;
	definer :  string;
}
let method_env : (string, (string, method_sig) Hashtbl.t) Hashtbl.t = Hashtbl.create 255

let tbl_find_opt h k = try Some (Hashtbl.find h k) with Not_found -> None

let get_class_methods (cls : string) : (string, method_sig) Hashtbl.t =
	match tbl_find_opt method_env cls with
	| Some t -> t
	| None ->
		let t = Hashtbl.create 31 in
		Hashtbl.add method_env cls t;
		t

let add_method ~(cls:string) ~(name:string) ~(formals:string list) ~(ret:string) =
	let t = get_class_methods cls in
	Hashtbl.replace t name { formals; ret; definer = cls }

let seed_builtins () =
	(* Object *)
	add_method ~cls:"Object"  ~name:"abort"      ~formals:[]              ~ret:"Object";
	add_method ~cls:"Object"  ~name:"type_name"  ~formals:[]              ~ret:"String";
	add_method ~cls:"Object"  ~name:"copy"       ~formals:[]              ~ret:"SELF_TYPE";
	(* IO *)
	add_method ~cls:"IO"      ~name:"out_string" ~formals:["String"]      ~ret:"SELF_TYPE";
	add_method ~cls:"IO"      ~name:"out_int"    ~formals:["Int"]         ~ret:"SELF_TYPE";
	add_method ~cls:"IO"      ~name:"in_string"  ~formals:[]              ~ret:"String";
	add_method ~cls:"IO"      ~name:"in_int"     ~formals:[]              ~ret:"Int";
	(* String *)
	add_method ~cls:"String"  ~name:"length"     ~formals:[]              ~ret:"Int";
	add_method ~cls:"String"  ~name:"concat"     ~formals:["String"]      ~ret:"String";
	add_method ~cls:"String"  ~name:"substr"     ~formals:["Int"; "Int"]  ~ret:"String"

let seed_user_methods (ast : cool_program) =
	List.iter (fun ((_, cname), _inherits, features) ->
		List.iter (function
		| Method ((_, mname), formals, (_, rtype), _mbody) ->
			let ftypes = List.map (fun (_fid, (_tl, tname)) -> tname) formals in
			add_method ~cls:cname ~name:mname ~formals:ftypes ~ret:rtype
		| Attribute _ -> ()
		) features
	) ast

let rec lookup_method_sig (cls : string) (mname : string) : method_sig option =
	match tbl_find_opt method_env cls with
	| Some t ->
		(match tbl_find_opt t mname with
		| Some s -> Some s
		| None ->
			if cls = "Object" then None
			else
				let parent = try Hashtbl.find parent_map cls with Not_found -> "Object" in
				lookup_method_sig parent mname)
	| None ->
		if cls = "Object" then None
		else
			let parent = try Hashtbl.find parent_map cls with Not_found -> "Object" in
			lookup_method_sig parent mname

let attribute_env : (string, (string, string) Hashtbl.t) Hashtbl.t = Hashtbl.create 255

let get_class_attributes (cls : string) : (string, string) Hashtbl.t = 
  match tbl_find_opt attribute_env cls with
  | Some t -> t
  | None ->
    let t = Hashtbl.create 31 in
    Hashtbl.add attribute_env cls t;
    t
  
let add_attribute ~(cls:string) ~(name:string) ~(typ:string) =
  let t = get_class_attributes cls in
  Hashtbl.replace t name typ

let seed_user_attributes (ast : cool_program) = 
  List.iter (fun ((_, cname), _inherits, features) ->
    List.iter (function
      | Attribute ((_, aname), (_tl, atype), _init) ->
        add_attribute ~cls:cname ~name:aname ~typ:atype
      | Method _ -> ()  
    ) features
  ) ast

let collect_attributes (cls : string) : (string, string) Hashtbl.t = 
  let acc = Hashtbl.create 31 in
  let rec climb c = 
    (match tbl_find_opt attribute_env c with
    | Some t -> Hashtbl.iter (fun n ty -> Hashtbl.replace acc n ty) t 
    | None -> ()); 
    if c <> "Object" then
      let p = try Hashtbl.find parent_map c with Not_found -> "Object" in
      climb p 
    in 
    climb cls;
    acc

let type_to_str = function
	| Class x -> x
	| SELF_TYPE _ -> "SELF_TYPE"

let is_primitive = function
	| Class "Int" | Class "String" | Class "Bool" -> true
	| _ -> false

let rec is_subtype t1 t2 =
	match t1, t2 with
	| Class x, Class y ->
		let rec climb c =
			if c = y then true
			else if c = "Object" then (y = "Object")
			else
				let p = try Hashtbl.find parent_map c with Not_found -> "Object" in
				climb p
		in
		climb x
	| SELF_TYPE c, Class y ->
		is_subtype (Class c) (Class y)
	| Class x, SELF_TYPE c ->
		x = c
	| SELF_TYPE c1, SELF_TYPE c2 ->
		c1 = c2

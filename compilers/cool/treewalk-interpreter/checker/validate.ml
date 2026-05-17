(* 
@author Trey Rubino
@date 09/06/2025
*)

open Ast
open Env

(* has a base class been redefined? or has a user class been duplicated? *)
let dups_base_validation ~base_classes (ast : cool_program) =
  let seen = Hashtbl.create 255 in
  List.iter (fun ((cloc, cname), _inherits, _features) ->
    if List.mem cname base_classes then (
      Printf.printf "ERROR: %s: Type-Check: redefining base class %s\n" cloc cname; 
      exit 1
    );
    if Hashtbl.mem seen cname then (
      Printf.printf "ERROR: %s: Type-Check: duplicate class %s\n" cloc cname; 
      exit 1
    );
    Hashtbl.add seen cname true
  ) ast

(* inheritance type problems *)
let parent_validation ~all_classes (ast : cool_program) =
  let forbidden = ["Int"; "Bool"; "String"; "SELF_TYPE"] in  
  (* local checks *)
  List.iter (fun ((_cloc, cname), inherits, _features) ->
    match inherits with
    | None -> ()
    | Some (iloc, iname) ->
        if iname = cname then (
          Printf.printf "ERROR: %s: Type-Check: class cannot inherit from itself (%s)\n" iloc cname; 
          exit 1
        );
        if List.mem iname forbidden then (
          Printf.printf "ERROR: %s: Type-Check: class %s inherits from %s\n" iloc cname iname; 
          exit 1
        );
        if not (List.mem iname all_classes) then (
          Printf.printf "ERROR: %s: Type-Check: inheriting from undefined class %s\n" iloc iname; 
          exit 1
        )
  ) ast;
  (* cycle detection / rooting at Object *)
  let find_opt h k = try Some (Hashtbl.find h k) with Not_found -> None in
  let color : (string, int) Hashtbl.t = Hashtbl.create 255 in  
  (* 0/=white, 1=gray, 2=black *)
  let rec dfs c =
    if c = "Object" then false
    else
      match find_opt color c with
      | Some 1 -> true
      | Some 2 -> false
      | _ ->
          Hashtbl.replace color c 1;
          let parent = try Hashtbl.find parent_map c with Not_found -> "Object" in
          let cyc = dfs parent in
          Hashtbl.replace color c 2;
          cyc
  in
  (* is there an inheritance cycle? *)
  List.iter (fun c ->
    if c <> "Object" && dfs c then (
      Printf.printf "ERROR: 0: Type-Check: inheritance cycle\n"; 
      exit 1
    )
  ) all_classes

let decl_types_validation ~all_classes (ast : cool_program) = 
  let type_exists t = List.mem t all_classes in
  List.iter (fun ((_cloc, _cname), _inherits, features) ->
    List.iter (function
      | Attribute ((_aloc, _name), (tloc, tname), _init) ->
        if tname <> "SELF_TYPE" && not (type_exists tname) then (
          Printf.printf "ERROR: %s: Type-Check: unknown type %s\n" tloc tname;
          exit 1
        );
      | Method ((_mloc, _mname), formals, (rtloc, rtype), _body) -> 
        if rtype <> "SELF_TYPE" && not (type_exists rtype) then (
          Printf.printf "ERROR: %s: Type-Check: unknown return type %s\n" rtloc rtype;
          exit 1 
        );  
        List.iter (fun ((_floc, _fname), (ftloc, ftname)) -> 
          if ftname = "SELF_TYPE" then (
            Printf.printf "ERROR: %s: Type-Check: SELF_TYPE not allowed as formal type\n" ftloc; 
            exit 1
          );
          if not (type_exists ftname) then (
            Printf.printf "ERROR: %s: Type-Check: unknown type %s\n" ftloc ftname;
            exit 1
          )
        ) formals
    ) features
  ) ast

(* 
method override compatibility
for any method a class defines that also exists in an ancestor, 
verify the signature matches exactly 
*)
let override_validation (ast : cool_program) = 
  List.iter (fun ((_, cname), _inherits, features) -> 
    let parent = try Hashtbl.find parent_map cname with Not_found -> "Object" in
    List.iter(function
      | Method ((mloc, mname), formals, (_rtloc, rtype), _body) -> 
        (match lookup_method_sig parent mname with
        | None -> ()
        | Some parent_sig -> 
          let child_formals = List.map (fun (_fid, (_tl, tn)) -> tn) formals in
          if List.length parent_sig.formals <> List.length child_formals then (
            Printf.printf "ERROR: %s: Type-Check: redefining method %s with different arity\n" mloc mname;
            exit 1
          );
          List.iter2 (fun p c ->
            if p <> c then (
              Printf.printf "ERROR: %s: Type-Check: redefining method %s with different parameter types\n" mloc mname;
              exit 1
            )
          ) parent_sig.formals child_formals;
          if parent_sig.ret <> rtype then (
            Printf.printf "ERROR: %s: Type-Check: redefining method %s with different return type\n" mloc mname;
            exit 1
          ))
      | Attribute _ -> () 
    ) features
  ) ast

let names_scoping_validation (ast : cool_program) =
  List.iter (fun ((_, cname), _inherits, features) ->
    let parent =
      try Hashtbl.find parent_map cname with Not_found -> "Object"
    in
    let inherited_attrs = 
      if cname = "Object" then Hashtbl.create 1 else collect_attributes parent
    in

    let seen_attrs = Hashtbl.create 31 in
    let seen_meths = Hashtbl.create 31 in 

    List.iter (function
      | Attribute ((aloc, aname), _, _) ->
        (* no self as attr *)
        if aname = "self" then (
          Printf.printf "ERROR: %s: Type-Check: attribute cannot be named self\n" aloc;
          exit 1
        );  
        (* no duplicate attributes inside the same class *)
        if Hashtbl.mem seen_attrs aname then (
          Printf.printf "ERROR: %s: Type-Check: duplicate attribute %s in class %s\n" aloc aname cname;
          exit 1
        );
        Hashtbl.add seen_attrs aname true;
        (* no attribute redefinition from ancestors *)
        if Hashtbl.mem inherited_attrs aname then (
          Printf.printf "ERROR: %s: Type-Check: attribute %s redefined from ancestor in class %s\n" aloc aname cname;
          exit 1
        )
      | Method ((mloc, mname), formals, _ret, _body) ->
        (* no duplicate methods inside the same class *)
        if Hashtbl.mem seen_meths mname then (
          Printf.printf "ERROR: %s: Type-Check: duplicate method %s in class %s\n" mloc mname cname;
          exit 1
        );
        Hashtbl.add seen_meths mname true;

        (* formals need to be unique and not self *)
        let seen_formals = Hashtbl.create 31 in
        List.iter (fun ((floc, fname), _fty) ->
          if fname = "self" then (
            Printf.printf "ERROR: %s: Type-Check: formal cannot be named self\n" floc;
            exit 1
          );
          if Hashtbl.mem seen_formals fname then (
            Printf.printf "ERROR: %s: Type-Check: duplicate formal %s in method %s\n" floc fname mname;
            exit 1
          );
          Hashtbl.add seen_formals fname true
        ) formals
    ) features
  ) ast

(* is main class defined and has no parameters? *)
let main_validation (ast : Ast.cool_program) =
  match List.find_opt (fun ((_, cname), _, _) -> cname = "Main") ast with
  | None ->
    Printf.printf "ERROR: 0: Type-Check: class Main is missing\n"; exit 1
  | Some (_id, _inh, features) ->
    let has_paramless_main =
      List.exists (function
        | Ast.Method ((_mloc, mname), formals, _ret, _body) -> mname = "main" && formals = []
        | _ -> false) features
    in
    if not has_paramless_main then (
      Printf.printf "ERROR: 0: Type-Check: class Main method main with 0 parameters not found\n";
      exit 1
    )

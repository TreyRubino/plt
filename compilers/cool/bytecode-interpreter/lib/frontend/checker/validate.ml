(* 
@author Trey Rubino
@date 09/06/2025
*)

open Ast
open Env
open Error

(* has a base class been redefined? or has a user class been duplicated? *)
let dups_base_validation ~base_classes (ast : cool_program) =
  let seen = Hashtbl.create 255 in
  List.iter (fun ((cloc, cname), _inherits, _features) ->
    if List.mem cname base_classes then (
      Error.checker cloc "redefining base class %s" cname
    );
    if Hashtbl.mem seen cname then (
      Error.checker cloc "duplicate class %s" cname
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
        Error.checker iloc "class cannot inherit from itself (%s)" cname
      );
      if List.mem iname forbidden then (
        Error.checker iloc "class %s inherits from %s" cname iname
      );
      if not (List.mem iname all_classes) then (
        Error.checker iloc "inheriting from undefined class %s" iname
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
      Error.checker "0" "inheritance cycle"
    )
  ) all_classes

let decl_types_validation ~all_classes (ast : cool_program) = 
  let type_exists t = List.mem t all_classes in
  List.iter (fun ((_cloc, _cname), _inherits, features) ->
    List.iter (function
      | Attribute ((_aloc, _name), (tloc, tname), _init) ->
        if tname <> "SELF_TYPE" && not (type_exists tname) then (
          Error.checker tloc "unknown type %s" tname
        );
      | Method ((_mloc, _mname), formals, (rtloc, rtype), _body) -> 
        if rtype <> "SELF_TYPE" && not (type_exists rtype) then (
          Error.checker rtloc "unknown return type" rtype    
        );  
        List.iter (fun ((_floc, _fname), (ftloc, ftname)) -> 
          if ftname = "SELF_TYPE" then (
            Error.checker ftloc "SELF_TYPE not allowed as formal type"
          );
          if not (type_exists ftname) then (
            Error.checker ftloc "unknown type %s" ftname
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
            Error.checker mloc "redefining method %s with different arity" mname
          );
          List.iter2 (fun p c ->
            if p <> c then (
              Error.checker mloc "redefining method %s with different parameter types" mname
            )
          ) parent_sig.formals child_formals;
          if parent_sig.ret <> rtype then (
            Error.checker mloc "redefining method %s with different return type" mname
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
          Error.checker aloc "attribute cannot be name self"
        );  
        (* no duplicate attributes inside the same class *)
        if Hashtbl.mem seen_attrs aname then (
          Error.checker aloc "duplicate attribute %s in class %s" aname cname
        );
        Hashtbl.add seen_attrs aname true;
        (* no attribute redefinition from ancestors *)
        if Hashtbl.mem inherited_attrs aname then (
          Error.checker aloc "attribute %s redefined from ancestor in class %s" aname cname
        )
      | Method ((mloc, mname), formals, _ret, _body) ->
        (* no duplicate methods inside the same class *)
        if Hashtbl.mem seen_meths mname then (
          Error.checker mloc "duplicate method %s in class" mname cname
        );
        Hashtbl.add seen_meths mname true;

        (* formals need to be unique and not self *)
        let seen_formals = Hashtbl.create 31 in
        List.iter (fun ((floc, fname), _fty) ->
          if fname = "self" then (
            Error.checker floc "formal cannot be named self"
          );
          if Hashtbl.mem seen_formals fname then (
            Error.checker floc "duplicate formal %s in method %s" fname mname
          );
          Hashtbl.add seen_formals fname true
        ) formals
    ) features
  ) ast

(* is main class defined and has no parameters? *)
let main_validation (ast : Ast.cool_program) =
  match List.find_opt (fun ((_, cname), _, _) -> cname = "Main") ast with
  | None ->
    Error.checker "0" "class Main is missing"
  | Some (_id, _inh, features) ->
    let has_paramless_main =
      List.exists (function
        | Ast.Method ((_mloc, mname), formals, _ret, _body) -> mname = "main" && formals = []
        | _ -> false) features
    in
    if not has_paramless_main then (
      Error.checker "0" "class Main method main with 0 parameters not found"
    )

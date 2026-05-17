(**
@file   lower.ml
@brief  Core lowering logic. Translates typed COOL expressions, methods, and
        class bodies into IR and bytecode. Handles layout, dispatch, control
        flow, and constructors.
@author Trey Rubino
@date   11/16/2025
*)

open Semantics
open Ir
open Gen
open Emit
open Bytecode
open Layout
open Error

type lower_ctx = {
  st    : Gen.t;
  buf   : Emit.t;
  env   : Semantics.semantic_env;
  cname : string;
  frame : Layout.frame_layout;
}

(** @brief Calculates the index of a method within a class's linearized dispatch 
           table. This index is used by the VM to perform dynamic method lookup 
           at runtime.
    @param env The global semantic environment.
    @param cname The name of the class containing the method.
    @param mname The name of the method to locate.
    @return The integer slot index in the dispatch table. *)
let dispatch_slot env cname mname =
  let meths = linear_methods env cname in
  let rec find i = function
    | [] -> Error.codegen "" "no method %s found in %s" mname cname
    | (n, _) :: xs -> if n = mname then i else find (i + 1) xs
  in
  find 0 meths

let rec depth env cname = 
  if cname = "Object" then 0 
  else 
    let parent = 
      try Hashtbl.find env.parent_map cname
      with Not_found -> "Object"
    in
    1 + depth env parent

let lower_attr a offset =
  { name = a.aname; offset }

(** @brief Constructs the metadata for a class, including parentage, attribute 
           offsets, and the virtual dispatch table (vtable). It handles 
           method overriding by replacing parent method IDs in existing slots.
    @param st The global generation state.
    @param env The semantic environment.
    @param cname The name of the class to lower.
    @param attrs The list of attributes specifically defined in this class.
    @param _methods The list of methods available to this class.
    @return A finalized Ir.class_info record. *)
let lower_class st env cname attrs _methods =
  let id = Hashtbl.find st.class_ids cname in

  let parent =
    match Hashtbl.find_opt env.parent_map cname with
    | Some p -> p
    | None -> "Object"
  in

  let parent_id =
    if parent = cname then -1
    else
      match Hashtbl.find_opt st.class_ids parent with
      | Some pid -> pid
      | None -> -1
  in

  (* attributes *)
  let all_attrs = linear_attrs env cname in
  let attrs_arr =
    Array.mapi (fun i a -> { name = a.aname; offset = i })
      (Array.of_list all_attrs)
  in

  (* parent dispatch *)
  let parent_disp =
    if parent <> cname && parent_id >= 0 then
      (Gen.get_class st parent_id).dispatch |> Array.to_list
    else
      []
  in

  let disp = ref parent_disp in
  let meths = linear_methods env cname in

  List.iter (fun (mname, impl) ->
    match Hashtbl.find_opt st.method_ids (impl.definer, mname) with
    | None -> ()
    | Some mid ->
      let rec update = function
        | [] -> (false, [])
        | smid :: xs ->
          if (Gen.get_method st smid).name = mname then
            (true, mid :: xs)
          else
            let (found, rest) = update xs in
            (found, smid :: rest)
      in
      let (found, new_disp) = update !disp in
      if found then disp := new_disp else disp := !disp @ [mid]
  ) meths;

  {
    name = cname;
    id;
    parent_id;
    attributes = attrs_arr;
    dispatch = Array.of_list !disp;
  }

(** @brief Recursively translates a typed AST expression into a sequence of 
           VM bytecode instructions. Manages constants, local/attribute scoping, 
           and control flow patching for If/While/Case.
    @param ctx The current lowering context (buffer, environment, frame).
    @param expr The typed AST expression to lower. *)
let rec lower_expr (ctx : lower_ctx) (expr : Ast.expr) =
  match expr.expr_kind with
  | Integer s ->
    let id = Gen.add_const ctx.st (Ir.LInt (int_of_string s)) in
    emit_op_i ctx.buf OP_CONST id expr.loc

  | String s ->
    let id = Gen.add_const ctx.st (Ir.LString s) in
    emit_op_i ctx.buf OP_CONST id expr.loc

  | True ->
    emit_op ctx.buf OP_TRUE expr.loc

  | False ->
    emit_op ctx.buf OP_FALSE expr.loc

  | Identifier ((vloc, vname)) ->
    if vname = "self" then (
      emit_op ctx.buf OP_GET_SELF vloc
    ) else (
      match Hashtbl.find_opt ctx.frame.slot_env vname with
      | Some slot ->
        emit_op_i ctx.buf OP_GET_LOCAL slot vloc
      | None ->
        let attrs = linear_attrs ctx.env ctx.cname in
        let rec find i = function
          | [] -> Error.codegen vloc "unknown identifier %s" vname
          | a :: tl -> if a.aname = vname then i else find (i + 1) tl
        in
        let offset = find 0 attrs in
        emit_op ctx.buf OP_GET_SELF vloc;
        emit_op_i ctx.buf OP_GET_ATTR (offset + 1) vloc
    )

  | Assign ((aloc, aname), rhs) ->
    if aname = "self" then (
      Error.codegen aloc "cannot assign to self"
    ) else (
      match Hashtbl.find_opt ctx.frame.slot_env aname with
      | Some slot ->
        lower_expr ctx rhs;
        emit_op_i ctx.buf OP_SET_LOCAL slot aloc;
        emit_op_i ctx.buf OP_GET_LOCAL slot aloc
      | None ->
        let attrs = linear_attrs ctx.env ctx.cname in
        let rec find i = function
          | [] -> Error.codegen aloc "unknown identifier %s" aname
          | a :: tl -> if a.aname = aname then i else find (i + 1) tl
        in
        let offset = find 0 attrs in
        emit_op ctx.buf OP_GET_SELF aloc; 
        lower_expr ctx rhs;
        emit_op_i ctx.buf OP_SET_ATTR (offset + 1) aloc;
        emit_op ctx.buf OP_GET_SELF aloc; 
        emit_op_i ctx.buf OP_GET_ATTR (offset + 1) aloc
    )

  | Plus (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_ADD expr.loc

  | Minus (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_SUB expr.loc

  | Times (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_MUL expr.loc

  | Divide (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_DIV expr.loc

  | Tilde e ->
    lower_expr ctx e;
    emit_op ctx.buf OP_NEG expr.loc

  | Lt (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_LESS expr.loc

  | Le (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_LESS_EQUAL expr.loc

  | Equals (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_EQUAL expr.loc

  | Not e ->
    lower_expr ctx e;
    emit_op ctx.buf OP_NOT expr.loc

  | Isvoid e ->
    lower_expr ctx e;
    emit_op ctx.buf OP_ISVOID expr.loc

  | If (pred, t, e) ->
    lower_expr ctx pred;
    let jf = mark ctx.buf in
    emit_op_i ctx.buf OP_JUMP_IF_FALSE 0 expr.loc;
    lower_expr ctx t;
    let je = mark ctx.buf in
    emit_op_i ctx.buf OP_JUMP 0 expr.loc;
    patch ctx.buf jf OP_JUMP_IF_FALSE (OffsetArg (mark ctx.buf - jf - 1));
    lower_expr ctx e;
    patch ctx.buf je OP_JUMP (OffsetArg (mark ctx.buf - je - 1))

  | While (pred, body) ->
    let top = mark ctx.buf in
    lower_expr ctx pred;
    let jf = mark ctx.buf in
    emit_op_i ctx.buf OP_JUMP_IF_FALSE 0 expr.loc;
    lower_expr ctx body;
    emit_op ctx.buf OP_POP expr.loc;
    emit_op_i ctx.buf OP_JUMP (top - mark ctx.buf - 1) expr.loc;
    patch ctx.buf jf OP_JUMP_IF_FALSE (OffsetArg (mark ctx.buf - jf -1));
    emit_op ctx.buf OP_VOID expr.loc

  | Let (bindings, body) ->
    let fl_child = {
      slot_env = Hashtbl.copy ctx.frame.slot_env;
      next_slot = ctx.frame.next_slot;
      local_count = ctx.frame.local_count;
    } in
    let ctx_child = { ctx with frame = fl_child } in
    List.iter (fun ((_, vname), (_, tname), init_opt) ->
      let slot = Layout.allocate_local fl_child vname in
      match init_opt with
      | None -> 
        (match tname with
        | "Int" -> emit_op_i ctx.buf OP_CONST (Gen.add_const ctx.st (Ir.LInt 0)) expr.loc
        | "Bool" -> emit_op ctx.buf OP_FALSE expr.loc
        | "String" -> emit_op_i ctx.buf OP_CONST (Gen.add_const ctx.st (Ir.LString "")) expr.loc
        | _ -> emit_op ctx.buf OP_VOID expr.loc);
        emit_op_i ctx.buf OP_SET_LOCAL slot expr.loc
      | Some e ->
        lower_expr ctx_child e;
        emit_op_i ctx.buf OP_SET_LOCAL slot expr.loc
    ) bindings;
    lower_expr ctx_child body

  | Case (scrut, branches) ->
    let s_slot = Layout.allocate_local ctx.frame "_scrut" in
    lower_expr ctx scrut;
    emit_op_i ctx.buf OP_SET_LOCAL s_slot expr.loc;

    emit_op_i ctx.buf OP_GET_LOCAL s_slot expr.loc;
    emit_op ctx.buf OP_ISVOID expr.loc;

    let j_not_void = mark ctx.buf in
    emit_op_i ctx.buf OP_JUMP_IF_FALSE 0 expr.loc;
    emit_op ctx.buf OP_CASE_ABORT expr.loc;

    let match_start = mark ctx.buf in
    patch ctx.buf j_not_void OP_JUMP_IF_FALSE (OffsetArg (match_start - j_not_void - 1));

    (* sort the branches deepest first so more specific types are tried before ancestors *)
    let sorted = 
      List.sort (fun ((_, _), (_, t1), _) ((_, _), (_, t2), _) -> 
        compare (depth ctx.env t2) (depth ctx.env t1)
      ) branches
    in

    let end_jumps = ref [] in

    (* iterate branches: use OP_IS_SUBTYPE to walk the parent_id chain at runtime *)
    List.iter (fun ((_, vname), (_, tname), br) ->
      let branch_cid = Hashtbl.find ctx.st.class_ids tname in

      emit_op_i ctx.buf OP_GET_LOCAL s_slot expr.loc;
      emit_op_i ctx.buf OP_IS_SUBTYPE branch_cid expr.loc;

      let j_next = mark ctx.buf in
      emit_op_i ctx.buf OP_JUMP_IF_FALSE 0 expr.loc;

      let slot = Layout.allocate_local ctx.frame vname in
      emit_op_i ctx.buf OP_GET_LOCAL s_slot expr.loc;
      emit_op_i ctx.buf OP_SET_LOCAL slot expr.loc;

      lower_expr ctx br;

      let j_end = mark ctx.buf in
      emit_op_i ctx.buf OP_JUMP 0 expr.loc;
      end_jumps := j_end :: !end_jumps;

      patch ctx.buf j_next OP_JUMP_IF_FALSE (OffsetArg (mark ctx.buf - j_next - 1));

    ) sorted;

    emit_op ctx.buf OP_CASE_ABORT expr.loc;

    let end_pos = mark ctx.buf in
    List.iter (fun j -> patch ctx.buf j OP_JUMP (OffsetArg (end_pos - j - 1))) !end_jumps

  | New ((cloc, cname)) ->
    (match cname with
    | "SELF_TYPE" ->
      emit_op ctx.buf OP_NEW_SELF_TYPE cloc          (* alloc + init in VM *)
    | _ ->
      let cid =
        try Hashtbl.find ctx.st.class_ids cname
        with Not_found -> Error.codegen cloc "unknown class %s" cname
      in
      emit_op_i ctx.buf OP_NEW cid cloc;             (* allocate object *)
      let init_mid = 
        try Hashtbl.find ctx.st.init_ids cname
        with Not_found -> Error.codegen cloc "missing init %s" cname
      in
      emit_op_i ctx.buf OP_CALL init_mid cloc   
    )

  | SelfDispatch ((mloc, mname), args) ->
    List.iter (fun a -> lower_expr ctx a) args;       (* push args first *)
    emit_op ctx.buf OP_GET_SELF mloc;                (* receiver on top *)
    let slot = dispatch_slot ctx.env ctx.cname mname in
    emit_op_i ctx.buf OP_DISPATCH slot mloc

  | DynamicDispatch (recv, (mloc, mname), args) ->
    List.iter (fun a -> lower_expr ctx a) args;  (* push args first *)
    lower_expr ctx recv;                         (* receiver on top *)

    let cname =
      match recv.static_type with
      | Some (Class c) -> c
      | Some (SELF_TYPE c) -> c
      | _ -> Error.codegen recv.loc "no static type for dispatch"
    in

    let slot = dispatch_slot ctx.env cname mname in
    emit_op_i ctx.buf OP_DISPATCH slot mloc

  | StaticDispatch (recv, (_cloc, tname), (mloc, mname), args) ->
    List.iter (fun a -> lower_expr ctx a) args;  (* push args first *)
    lower_expr ctx recv;                         (* receiver on top *)

    let meths = linear_methods ctx.env tname in
    let impl =
      let rec find = function
        | [] -> Error.codegen mloc "method %s not found in %s" mname tname
        | (name, impl) :: tl -> if name = mname then impl else find tl
      in
      find meths
    in

    let mid =
      try Hashtbl.find ctx.st.method_ids (impl.definer, mname)
      with Not_found ->
        Error.codegen mloc "missing method id for %s.%s" impl.definer mname
    in
    emit_op_i ctx.buf OP_STATIC_DISPATCH mid mloc

  | Block exprs ->
    let fl_child = {
      slot_env = Hashtbl.copy ctx.frame.slot_env;
      next_slot = ctx.frame.next_slot;
      local_count = ctx.frame.local_count;
    } in
    let ctx_child = { ctx with frame = fl_child } in

    let rec emit_block_body = function 
    | [] -> emit_op ctx.buf OP_VOID expr.loc (* defensive but this shouldnt happen in a vlaid AST *)
    | [last] -> lower_expr ctx_child last
    | h :: t -> 
      lower_expr ctx_child h;
      emit_op ctx.buf OP_POP expr.loc;
      emit_block_body t
    in
    emit_block_body exprs

(** @brief Generates the '__init' constructor for a class. It linearizes all 
           attribute initializers from the entire inheritance chain into a 
           single flat routine to ensure correct object state.
    @param st The global generation state.
    @param env The semantic environment.
    @param cname The name of the class.
    @param attrs The list of attributes specifically defined in this class.
    @return An Ir.method_info record containing the initialization bytecode. *)
let lower_constructor (st : Gen.t) (env : Semantics.semantic_env) (cname : string) (attrs : Semantics.attr_impl list) 
: Ir.method_info =
  let buf = Emit.create () in
  let frame = Layout.create_frame_layout [] in
  let ctx = { st; buf; env; cname; frame } in

  let loc = 
    match attrs with 
    |  h :: _ -> (match h.init with Some e -> e.loc | None -> "0") 
    | [] -> "0" 
  in

  (* flat init: emit all inherited and own attribute initializers in linearized
     order with no parent constructor calls. this avoids re-entrant construction
     when attribute initializers allocate objects of ancestor/descendant types. *)
  let all_attrs = linear_attrs env cname in
  let offset_of_attr aname =
    let rec find i = function
      | [] -> Error.codegen "" "unknown attribute %s" aname
      | a :: tl -> if a.aname = aname then i else find (i + 1) tl
    in
    find 0 all_attrs
  in

  List.iter (fun a ->
    match a.init with
    | None -> ()
    | Some e ->
      let off = offset_of_attr a.aname in
      emit_op ctx.buf OP_GET_SELF e.loc;
      lower_expr ctx e;
      emit_op_i ctx.buf OP_SET_ATTR (off + 1) e.loc
  ) all_attrs;

  emit_op ctx.buf OP_GET_SELF loc;
  emit_op ctx.buf OP_RETURN loc;

  {
    name = "__init_" ^ cname;
    class_id = Hashtbl.find st.class_ids cname;
    n_locals = !(frame.local_count);
    n_formals = 0;
    code = Emit.to_program buf;
    line_map = Emit.get_line_map buf;
  }

(** @brief Translates a method body into bytecode. Automatically injects 
           the Main object's constructor call if the method is 'Main.main'.
    @param st The global generation state.
    @param env The semantic environment.
    @param cname The class containing the method.
    @param mname The name of the method.
    @param impl The implementation details.
    @return The completed Ir.method_info record. *)
let lower_method st env cname mname impl =
  let buf = Emit.create () in
  let frame = Layout.create_frame_layout impl.formals in
  let ctx = { st; buf; env; cname; frame } in

  let method_line = 
    match impl.body with
    | User body -> body.loc
    | Internal _ -> "0"
  in

  if cname = "Main" && mname = "main" then (
    let init_mid = Hashtbl.find st.init_ids "Main" in
    emit_op ctx.buf OP_GET_SELF method_line;
    emit_op_i ctx.buf OP_CALL init_mid method_line;
    emit_op ctx.buf OP_POP method_line
  );

  (match impl.body with
  | Internal _ ->
    emit_op buf OP_RETURN "0"
  | User body ->
    lower_expr ctx body;
    emit_op buf OP_RETURN body.loc);

  let n_formals =
    match impl.body with
    | Internal { qname; _ } ->
      (match qname with
      | "IO.out_int" -> 1
      | "IO.out_string" -> 1
      | "String.concat" -> 1
      | "String.substr" -> 2
      | _ -> 0)
    | User _ ->
      List.length impl.formals
  in
  {
    name = mname;
    class_id = Hashtbl.find st.class_ids cname;
    n_locals = !(frame.local_count);
    n_formals;
    code = Emit.to_program buf;
    line_map = Emit.get_line_map buf;
  }

(** @brief Pre-scans a class to register all its method IDs in the global 
           table. This allows method calls to be linked correctly even 
           before the method bodies are fully compiled.
    @param st The global generation state.
    @param env The semantic environment.
    @param cname The name of the class to scan. *)
let scan_method_ids st env cname =
  let meths = linear_methods env cname in
  List.iter (fun (mname, impl) ->
    if not (String.starts_with ~prefix:"__init_" mname) then (
      if impl.definer = cname then (
        let mid = Gen.add_method st {
          Ir.name = mname; class_id = Hashtbl.find st.class_ids cname;
          n_locals=0; n_formals=0; code=[||]; line_map = [||]
        } in
        Hashtbl.replace st.method_ids (cname, mname) mid
      )
    )
  ) meths

(** @brief Driver that lowers a full class. It coordinates 
           the generation of the constructor, the translation of all methods, 
           and the assembly of class-level metadata.
    @param st The global generation state.
    @param env The semantic environment.
    @param cname The name of the class to fully lower into IR. *)
let lower_class_group st env cname =
  let attrs =
    try Hashtbl.find env.class_map cname with _ -> []
  in

  (* constructor *)
  let construct = lower_constructor st env cname attrs in
  let construct_id = Hashtbl.find st.init_ids cname in
  Gen.set_method st construct_id construct;
  
  let meths = linear_methods env cname in
  List.iter (fun (mname, impl) ->
    if not (String.starts_with ~prefix:"__init_" mname) then (
      if impl.definer = cname then (
        let mi = lower_method st env cname mname impl in
        let mid = Hashtbl.find st.method_ids (cname, mname) in
        Gen.set_method st mid mi
      )
    )
  ) meths;
  let class_info = lower_class st env cname attrs meths in
  Gen.add_class st class_info;
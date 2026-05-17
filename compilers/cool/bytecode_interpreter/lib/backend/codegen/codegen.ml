(**
@file   codegen.ml
@brief  Entre point for the Code Generation phase of the pipeline.
@author Trey Rubino
@date   11/30/2025
*)

open Semantics
open Gen
open Lower
open Bytecode
open Debug

(** @brief Calculates the inheritance depth of a class by recursively traversing 
           the parent map until it reaches 'Object'. This is used to ensure 
           parent classes are processed and laid out before their children.
    @param env The semantic environment containing the hierarchy.
    @param cname The name of the class to measure.
    @return The integer depth (0 for Object). *)
let rec depth env cname = 
  if cname = "Object" then 0 
  else 
    let parent = 
      try Hashtbl.find env.parent_map cname
      with Not_found -> "Object"
    in
    1 + depth env parent

(** @brief The primary entry point for IR generation. It orchestrates class 
           ID assignment, generates constructor stubs, scans for method 
           signatures, and lowers the AST into bytecode.
    @param env The validated semantic environment from the previous compiler phase.
    @return A complete Ir.ir record ready for the Virtual Machine. *)
let emit (env : Semantics.semantic_env) : Ir.ir =
  let st = Gen.create () in

  let class_names =
    Hashtbl.fold (fun cname _ acc -> cname :: acc) env.class_map [] 
    |> List.sort (fun c1 c2 ->
        compare (depth env c1) (depth env c2)
      )
  in

  List.iteri (fun i cname ->
    Hashtbl.replace st.class_ids cname i
  ) class_names;

  List.iter (fun cname ->
    let class_id = Hashtbl.find st.class_ids cname in
    let mid = Gen.add_method st {
      Ir.name = "__init_" ^ cname;
      class_id;
      n_locals = 0;
      n_formals = 0;
      code = [||]; 
      line_map = [||];  (* empty, will be updated at later stages *)
    } in
    Hashtbl.replace st.init_ids cname mid
  ) class_names;

  List.iter (fun cname ->
    Lower.scan_method_ids st env cname
  ) class_names;

  List.iter (fun cname ->
    Lower.lower_class_group st env cname
  ) class_names;

  let entry_id =
    let found = ref None in
    List.iteri (fun i (m : Ir.method_info) ->
      if m.name = "main" && not (String.starts_with ~prefix:"__init_" m.name)
        then found := Some i
    ) !(st.methods);
    match !found with
    | Some id -> id
    | None -> Error.codegen "0" "Main.main not found"
  in

  Gen.to_ir st entry_id
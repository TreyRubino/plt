(**
@file   layout.ml
@brief  Implements logic for object memory layouts and dispatch tables, 
        including inheritance linearalization and stack frame allocation 
        for local variables.
@author Trey Rubino
@date   11/30/2025
*)

open Semantics

(** @brief Calculates the full inheritance path of a class, from 'Object' down 
           to the specified class. This sequence is used to determine the 
           order of attribute and method definitions.
    @param env The global semantic environment.
    @param cls The name of the class to trace.
    @return A list of class names representing the linear ancestry path. *)
let ancestry (env : Semantics.semantic_env) (cls : string) : string list =
  let rec go acc c = 
    if c = "Object" then ("Object" :: acc)
    else 
      let p = 
        try Hashtbl.find env.parent_map c 
        with Not_found -> "Object"
      in
      go (c :: acc) p
  in
  go [] cls

(** @brief Produces a linearized list of all methods available to a class, 
           including those inherited or overridden from parents. It ensures 
           that an overridden method maintains the same index in the 
           dispatch table as its original definition.
    @param env The global semantic environment.
    @param cls The name of the class for which to build the dispatch table.
    @return A list of method implementations sorted by their appearance in the hierarchy. *)
let linear_methods (env : Semantics.semantic_env) (cls : string) : (string * Semantics.method_impl) list =
  let path = ancestry env cls in
  let table = Hashtbl.create 32 in
  let order = ref [] in
  
  List.iter (fun c ->
    match Hashtbl.find_opt env.impl_map c with
    | None -> ()
    | Some tbl ->
      let current_level_meths = 
        Hashtbl.fold (fun m impl acc -> (m, impl) :: acc) tbl []
        |> List.sort (fun (a, _) (b, _) -> String.compare a b) 
      in
      
      List.iter (fun (mname, impl) ->
        if not (Hashtbl.mem table mname) then (
          order := mname :: !order;
          Hashtbl.add table mname impl
        ) else (
          Hashtbl.replace table mname impl
        )
      ) (List.rev current_level_meths)
  ) path;
  
  List.rev !order |> List.map (fun mname -> (mname, Hashtbl.find table mname))

(** @brief Calculates the memory layout of an object's attributes. It 
           ensures that inherited attributes appear first, followed by 
           attributes defined in the child class, maintaining fixed offsets 
           for polymorphic access.
    @param env The global semantic environment.
    @param cls The name of the class to layout.
    @return A list of attribute implementations in their physical slab order. *)
let linear_attrs (env : Semantics.semantic_env) (cls : string) : Semantics.attr_impl list =
  let path = ancestry env cls in
  let table = Hashtbl.create 32 in
  let order = ref [] in

  List.iter (fun c ->
    let attrs = try Hashtbl.find env.class_map c with Not_found -> [] in
    List.iter (fun a ->
      if not (Hashtbl.mem table a.aname) then (
        order := a.aname :: !order;
        Hashtbl.add table a.aname a
      )
    ) attrs 
  ) path;

  List.rev !order |> List.map (fun name -> Hashtbl.find table name)

type frame_layout = {
  slot_env    : (string, int) Hashtbl.t;
  next_slot   : int ref;
  local_count : int ref;
}

(** @brief Initializes the layout for a method's activation record. 
           It maps formal parameters to initial local slots and prepares 
           counters for additional local variables allocated during execution.
    @param formals The list of method parameters (name and type).
    @return A frame_layout record containing the slot mapping and counters. *)
let create_frame_layout (formals : (Ast.id * Ast.cool_type) list) : frame_layout =
  let env = Hashtbl.create 16 in
  List.iteri (fun i ((_, name), _) ->
    Hashtbl.add env name i
  ) formals;
  { 
    slot_env = env;
    next_slot = ref (List.length formals);
    local_count = ref 0;
  }

(** @brief Reserves a new slot in the current call frame for a local variable
          and updates the frame's local count.
    @param fl The current frame layout being constructed.
    @param name The name of the local variable to allocate.
    @return The integer index of the newly assigned stack slot. *)
let allocate_local (fl : frame_layout) (name : string) : int =
  let slot = !(fl.next_slot) in
  Hashtbl.add fl.slot_env name slot;
  incr fl.next_slot;
  incr fl.local_count;
  slot
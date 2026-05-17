(* 
@author Trey Rubino
@date 09/06/2025
*)

open Ast
open Eval
open Runtime
open Reader

let main () = 
  begin
    let fname = Sys.argv.(1) in
    let fin = open_in fname in
    let env = load_runtime_env fin in
    close_in fin;
    
    let main_obj = new_object_defaults env "Main" in
    run_initializers env main_obj ~scopes:[];

    (* look up the main method in class Main *)
    let methods = 
      try Hashtbl.find env.impl_map "Main"
      with Not_found -> runtime_error "0" "class Main has no methods"
    in
    let impl = 
      try Hashtbl.find methods "main"
      with Not_found -> runtime_error "0" "Main.main not found"
    in 

    (* call the main function *)
    ignore (call_method env ~recv:main_obj ~scopes:[] impl []);
  end;;
main ();;
(** @file main.ml
    @brief main entry
    @author Trey Rubino
    @date 05/21/2026 *)
    
let () = 
  let args = Array.to_list Sys.argv in
  match args with
  | [ _; _; _; ] -> 
    Printf.eprintf "usage: loxi <file.lox>";
    exit 64;  
  | [ _; filename ] -> 
    print_endline ("Executing: " ^ filename)
  | _ ->
    print_endline ("Running REPL")
    
    



  



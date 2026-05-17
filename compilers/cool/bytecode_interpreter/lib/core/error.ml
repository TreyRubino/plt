(**
@file   error.ml
@brief  Unified error module for easy pipeline reporting
@author Trey Rubino
@date   11/15/2025
*)

type t = {
  phase : string;
  line  : string;
  msg   : string
}

(** @brief A specialized exception variant that carries an error record [t]. 
           This allows the compiler to bail out of a specific phase and 
           report the error at the top level. *)
exception E of t

(** @brief Formats and raises a phase-specific error. It uses OCaml's [ksprintf] 
           to support a variable number of formatting arguments.
    @param phase The name of the pipeline stage (e.g., "Parser", "VM").
    @param line The source line number or program counter where the error occurred.
    @param fmt The format string for the error message. *)
let raisef ~phase ~line fmt =
  Printf.ksprintf (fun msg ->
    raise (E { phase; line; msg })
  ) fmt

(** @brief Prints a formatted error message to standard output, adhering to 
           the standard "ERROR: line: phase: message" format as per the COOL spec.
    @param record The error record to be printed. *)
let print { phase; line; msg } = 
  Printf.printf "ERROR: %s: %s: %s\n" line phase msg

(** @brief Specialized error reporter for the Lexer phase. It extracts the 
           current line number directly from the lexing buffer.
    @param lexbuf The current Lexing.lexbuf being processed.
    @param fmt The format string for the lexical error. *)
let lexer lexbuf fmt = 
  let line = string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
  raisef ~phase:"Lexer" ~line fmt

(** @brief Specialized error reporter for the Parser phase. It identifies 
           the current line and the lexeme that triggered the syntax error.
    @param lexbuf The current Lexing.lexbuf where the syntax error was detected. *)
let parser lexbuf = 
  let line = string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
  let lexeme = Lexing.lexeme lexbuf in
  print {
    phase = "Parser";
    line;
    msg = Printf.sprintf "syntax error near %s" lexeme;
  }

let checker loc fmt =
  raisef ~phase:"Type-Check" ~line:loc fmt

let codegen loc fmt =
  raisef ~phase:"Codegen" ~line:loc fmt

let vm pc fmt = 
  raisef ~phase:"Exception" ~line:pc fmt

  
{

(* Trey Rubino - COOL Lexer - Dr. Schwesinger *)

type token = 
  | INTEGER of int
  | STRING of string
  | IDENTIFIER of string
  | TYPE of string
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | AT
  | LT
  | SEMI
  | EQUALS
  | TILDE
  | DOT
  | COMMA
  | COLON
  | RBRACE
  | LBRACE
  | RPAREN    
  | LPAREN
  | LE
  | LARROW
  | RARROW
  | TRUE
  | FALSE
  | NEW
  | NOT
  | LET
  | ELSE
  | CLASS
  | CASE 
  | ESAC
  | INHERITS
  | LOOP
  | POOL
  | ISVOID
  | OF
  | IN
  | IF
  | FI
  | WHILE
  | THEN

exception UNKNOWN
exception MAX_LENGTH
exception NO_CLOSURE
exception NULL_IN_STRING
exception EOF 

}

rule token = parse
  | [' ' '\t' '\r' '\012' '\011']	                                            { token lexbuf } 
  | '\n'		                                                                  { Lexing.new_line lexbuf ; token lexbuf  }

  | "--" [^ '\n']*                                                            { token lexbuf }
  | "(*"                                                                      { comment 1 lexbuf.lex_curr_p.pos_lnum lexbuf }
          
  | '+'			                                                                  { PLUS } 
  | '-'			                                                                  { MINUS }
  | '*' 		                                                                  { TIMES }
  | '/' 		                                                                  { DIVIDE }
  | '='                                                                       { EQUALS }
  | '@'                                                                       { AT }
  | '<'                                                                       { LT }
  | ';'                                                                       { SEMI }
  | '~'                                                                       { TILDE }
  | '.'                                                                       { DOT }
  | ','                                                                       { COMMA }
  | ':'                                                                       { COLON }
  | '}'                                                                       { RBRACE }
  | '{'                                                                       { LBRACE }
  | ')'                                                                       { RPAREN }
  | '('                                                                       { LPAREN }
  | "<="                                                                      { LE }   
  | "<-"                                                                      { LARROW }
  | "=>"                                                                      { RARROW }

  | 't'['r' 'R']['u' 'U']['e' 'E']                                            { TRUE } 
  | 'f'['a' 'A']['l' 'L']['s' 'S']['e' 'E']                                   { FALSE }

  | ['n' 'N']['e' 'E']['w' 'W']                                               { NEW }
  | ['n' 'N']['o' 'O']['t' 'T']                                               { NOT }
  | ['l' 'L']['e' 'E']['t' 'T']                                               { LET }
  | ['e' 'E']['l' 'L']['s' 'S']['e' 'E']                                      { ELSE }
  | ['c' 'C']['l' 'L']['a' 'A']['s' 'S']['s' 'S']                             { CLASS }
  | ['c' 'C']['a' 'A']['s' 'S']['e' 'E']                                      { CASE }
  | ['e' 'E']['s' 'S']['a' 'A']['c' 'C']                                      { ESAC }
  | ['i' 'I']['n' 'N']['h' 'H']['e' 'E']['r' 'R']['i' 'I']['t' 'T']['s' 'S']  { INHERITS }
  | ['l' 'L']['o' 'O']['o' 'O']['p' 'P']                                      { LOOP }
  | ['p' 'P']['o' 'O']['o' 'O']['l' 'L']                                      { POOL }
  | ['i' 'I']['s' 'S']['v' 'V']['o' 'O']['i' 'I']['d' 'D']                    { ISVOID }
  | ['o' 'O']['f' 'F']                                                        { OF }
  | ['i' 'I']['n' 'N']                                                        { IN }
  | ['i' 'I']['f' 'F']                                                        { IF }
  | ['f' 'F']['i' 'I']                                                        { FI }
  | ['w' 'W']['h' 'H']['i' 'I']['l' 'L']['e' 'E']                             { WHILE }
  | ['t' 'T']['h' 'H']['e' 'E']['n' 'N']                                      { THEN }

  | ['0'-'9']+ as lxm 	                                                      { INTEGER(int_of_string lxm) }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm                            { TYPE(lxm) }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm                            { IDENTIFIER(lxm) }    

  | '"' ([^ '"' '\\' '\n' '\000'] | '\\' _)* '\000' [^ '"']* '"'              { raise NULL_IN_STRING }
  | '"' ( [^ '"' '\\' '\n'] | '\\' _ )* '"' as lxm  { if String.length lxm > 1024 then ( raise MAX_LENGTH ) else STRING(String.sub lxm 1 (String.length lxm - 2)) }

  | _                                                                         { raise UNKNOWN }
  | eof 			                                                                { raise EOF }

and comment depth start_line = parse
  | "(*"                                                                      { comment (depth + 1) start_line lexbuf }
  | "*)"                                                                      { if depth = 1 then token lexbuf else comment (depth - 1) start_line lexbuf }
  | '\n'                                                                      { Lexing.new_line lexbuf ; comment depth start_line lexbuf }
  | eof                                                                       { raise NO_CLOSURE }
  | _                                                                         { comment depth start_line lexbuf }

{

let serialize token line = 
  Printf.sprintf "%d\n%s\n" 
    (line)
    (match token with
    | PLUS          -> "plus"
    | MINUS         -> "minus"
    | TIMES         -> "times"
    | DIVIDE        -> "divide"
    | AT            -> "at"
    | LT            -> "lt"
    | SEMI          -> "semi"
    | TILDE         -> "tilde"
    | DOT           -> "dot"
    | COMMA         -> "comma"
    | COLON         -> "colon"
    | RBRACE        -> "rbrace"
    | LBRACE        -> "lbrace"
    | RPAREN        -> "rparen"
    | LPAREN        -> "lparen"
    | LE            -> "le"
    | LARROW        -> "larrow"
    | RARROW        -> "rarrow"
    | TRUE          -> "true"
    | FALSE         -> "false"
    | NEW           -> "new"
    | EQUALS        -> "equals"
    | NOT           -> "not"
    | LET           -> "let"
    | ELSE          -> "else"
    | CLASS         -> "class"
    | CASE          -> "case"
    | ESAC          -> "esac"
    | INHERITS      -> "inherits"
    | LOOP          -> "loop"
    | POOL          -> "pool"
    | ISVOID        -> "isvoid"
    | OF            -> "of"
    | IN            -> "in"
    | IF            -> "if"
    | FI            -> "fi"
    | WHILE         -> "while"
    | THEN          -> "then"
    | INTEGER(i)    -> Printf.sprintf "integer\n%d" i
    | STRING(s)     -> Printf.sprintf "string\n%s" s
    | IDENTIFIER(i) -> Printf.sprintf "identifier\n%s" i
    | TYPE(t)       -> Printf.sprintf "type\n%s" t
    )

let main () = 
begin
  try 
    let infile = Sys.argv.(1) in
    let in_channel = open_in_bin infile in
    let lexbuf = Lexing.from_channel in_channel in
    let outbuf = Buffer.create 255 in
    try
      while true do
        let result = token lexbuf in
        Printf.bprintf outbuf "%s" (serialize result lexbuf.lex_curr_p.pos_lnum)
      done
    with 
    | UNKNOWN     -> Printf.printf "ERROR: %d: Lexer: invalid character: %s\n" lexbuf.lex_curr_p.pos_lnum (Lexing.lexeme lexbuf) ; exit 1
    | MAX_LENGTH  -> Printf.printf "ERROR: %d: Lexer: string constant is too long: %s\n" lexbuf.lex_curr_p.pos_lnum (Lexing.lexeme lexbuf) ; exit 1
    | NO_CLOSURE  -> Printf.printf "ERROR: %d: Lexer: EOF in comment\n"(lexbuf.lex_curr_p.pos_lnum); exit 1
    | NULL_IN_STRING -> Printf.printf "ERROR: %d: Lexer: null in string: %s\n" lexbuf.lex_curr_p.pos_lnum (Lexing.lexeme lexbuf) ; exit 1
    | EOF         -> 
      begin
        try 
          let outfile = Sys.argv.(1) ^ "-lex" in
          let out_channel = open_out outfile in 
          Printf.fprintf out_channel "%s" (Buffer.contents outbuf) ;
          close_out out_channel
        with Sys_error msg -> (Printf.eprintf "Failed to open out channel: %s\n" msg ; exit 1 : unit)
        ; close_in in_channel ; exit 0
      end
  with Sys_error msg -> Printf.eprintf "Failed to open file: %s\n" msg ; exit 1
end;; 
main ();;

}
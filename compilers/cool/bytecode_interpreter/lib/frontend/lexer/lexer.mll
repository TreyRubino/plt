(* 
@author Trey Rubino
@date 11/13/2025
*)

{
open Parser
open Error

let line_str lexbuf =
  string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
}

rule token = parse
  | [' ' '\t' '\r' '\012' '\011']	                                  { token lexbuf } 
  | '\n'		                                                        { Lexing.new_line lexbuf ; token lexbuf  }

  | "--" [^ '\n']*                                                  { token lexbuf }
  | "(*"                                                            { comment 1 lexbuf.lex_curr_p.pos_lnum lexbuf }
          
  | '+'			                                                        { PLUS   (line_str lexbuf) } 
  | '-'			                                                        { MINUS  (line_str lexbuf) }
  | '*' 		                                                        { TIMES  (line_str lexbuf) }
  | '/' 		                                                        { DIVIDE (line_str lexbuf) }
  | '='                                                             { EQUALS (line_str lexbuf) }
  | '@'                                                             { AT     (line_str lexbuf) }
  | '<'                                                             { LT     (line_str lexbuf) }
  | ';'                                                             { SEMI   (line_str lexbuf) }
  | '~'                                                             { TILDE  (line_str lexbuf) }
  | '.'                                                             { DOT    (line_str lexbuf) }
  | ','                                                             { COMMA  (line_str lexbuf) }
  | ':'                                                             { COLON  (line_str lexbuf) }
  | '}'                                                             { RBRACE (line_str lexbuf) }
  | '{'                                                             { LBRACE (line_str lexbuf) }
  | ')'                                                             { RPAREN (line_str lexbuf) }
  | '('                                                             { LPAREN (line_str lexbuf) }
  | "<="                                                            { LE     (line_str lexbuf) }   
  | "<-"                                                            { LARROW (line_str lexbuf) }
  | "=>"                                                            { RARROW (line_str lexbuf) }

  | 't'['r' 'R']['u' 'U']['e' 'E']                                  { TRUE   (line_str lexbuf) } 
  | 'f'['a' 'A']['l' 'L']['s' 'S']['e' 'E']                         { FALSE  (line_str lexbuf) }

  | ['n' 'N']['e' 'E']['w' 'W']                                     { NEW    (line_str lexbuf) }
  | ['n' 'N']['o' 'O']['t' 'T']                                     { NOT    (line_str lexbuf) }
  | ['l' 'L']['e' 'E']['t' 'T']                                     { LET    (line_str lexbuf) }
  | ['e' 'E']['l' 'L']['s' 'S']['e' 'E']                            { ELSE   (line_str lexbuf) }
  | ['c' 'C']['l' 'L']['a' 'A']['s' 'S']['s' 'S']                   { CLASS  (line_str lexbuf) }
  | ['c' 'C']['a' 'A']['s' 'S']['e' 'E']                            { CASE   (line_str lexbuf) }
  | ['e' 'E']['s' 'S']['a' 'A']['c' 'C']                            { ESAC   (line_str lexbuf) }
  | ['i' 'I']['n' 'N']['h' 'H']['e' 'E']['r' 'R']['i' 'I']['t' 'T']['s' 'S']
                                                                    { INHERITS (line_str lexbuf) }
  | ['l' 'L']['o' 'O']['o' 'O']['p' 'P']                            { LOOP   (line_str lexbuf) }
  | ['p' 'P']['o' 'O']['o' 'O']['l' 'L']                            { POOL   (line_str lexbuf) }
  | ['i' 'I']['s' 'S']['v' 'V']['o' 'O']['i' 'I']['d' 'D']
                                                                    { ISVOID (line_str lexbuf) }
  | ['o' 'O']['f' 'F']                                              { OF     (line_str lexbuf) }
  | ['i' 'I']['n' 'N']                                              { IN     (line_str lexbuf) }
  | ['i' 'I']['f' 'F']                                              { IF     (line_str lexbuf) }
  | ['f' 'F']['i' 'I']                                              { FI     (line_str lexbuf) }
  | ['w' 'W']['h' 'H']['i' 'I']['l' 'L']['e' 'E']                   { WHILE  (line_str lexbuf) }
  | ['t' 'T']['h' 'H']['e' 'E']['n' 'N']                            { THEN   (line_str lexbuf) }

  | ['0'-'9']+ as lxm 	                                            { INTEGER    (line_str lexbuf, lxm) }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm                  { TYPE       (line_str lexbuf, lxm) }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm                  { IDENTIFIER (line_str lexbuf, lxm) }    

  | '"' ([^ '"' '\\' '\n' '\000'] | '\\' _)* '\000' [^ '"']* '"' as lxm
                                                                    { Error.lexer lexbuf "null in string %s" lxm }
  | '"' ( [^ '"' '\\' '\n'] | '\\' _ )* '"' as lxm      { 
        if String.length lxm > 1024 then
          Error.lexer lexbuf "string constant is too long"
        else
          let inner = String.sub lxm 1 (String.length lxm - 2) in
          STRING (line_str lexbuf, inner)
      }

  | _ as lxm                                                        { Error.lexer lexbuf "invalid character %c" lxm }
  | eof 			                                                      { EOF }

and comment depth start_line = parse
  | "(*"                                                            { comment (depth + 1) start_line lexbuf }
  | "*)"                                                            { if depth = 1 then token lexbuf else comment (depth - 1) start_line lexbuf }
  | '\n'                                                            { Lexing.new_line lexbuf ; comment depth start_line lexbuf }
  | eof                                                             { Error.lexer lexbuf "EOF in comment" }
  | _                                                               { comment depth start_line lexbuf }

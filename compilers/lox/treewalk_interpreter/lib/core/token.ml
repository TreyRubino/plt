(** @author Trey Rubino 
    @date 05/21/2026 *) 

type token_type = 
    | LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT 
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    | IDENTIFIER
    | STRING
    | NUMBER
    | AND
    | CLASS
    | ELSE
    | FALSE
    | TRUE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | VAR
    | WHILE
    | EOF

type token = {
    token_type : token_type;
    line : string;
    lexeme : string;
}
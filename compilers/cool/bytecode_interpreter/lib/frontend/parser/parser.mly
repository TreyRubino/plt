(*
@author Trey Rubino
@date 11/13/2025
*)

%{
open Ast
let create_node loc kind = 
  { loc; expr_kind = kind; static_type = None }
%}

%token <string * string> STRING IDENTIFIER TYPE INTEGER
%token <string> PLUS MINUS TIMES DIVIDE EQUALS
%token <string> AT LT SEMI TILDE DOT COMMA COLON
%token <string> RBRACE LBRACE RPAREN LPAREN LE
%token <string> LARROW RARROW TRUE FALSE NEW NOT
%token <string> LET ELSE CLASS CASE ESAC INHERITS LOOP
%token <string> POOL ISVOID OF IN IF FI WHILE THEN
%token EOF

%right LARROW
%left NOT
%left LT LE EQUALS
%left PLUS MINUS
%left TIMES DIVIDE
%left ISVOID
%left TILDE
%left AT
%left DOT

%type <cool_class list> cool_program
%type <expr> expr assign_expr cmp_expr sum_expr product_expr 
%type <expr> unary_expr primary_expr primary_base atom
%type <expr list> expr_list block_elems

%start cool_program

%%

cool_program:
    class_list                                  { $1 }
  ;

class_list:
    cool_class SEMI                              { [$1] }
  | cool_class SEMI class_list                   { $1 :: $3 }
  ;

cool_class:
    CLASS TYPE LBRACE feature_list RBRACE                 { ($2, None, $4) } 
  | CLASS TYPE INHERITS TYPE LBRACE feature_list RBRACE   { ($2, Some $4, $6) }
  ;

feature_list:
    /* lambda */                                { [] }
  | feature SEMI feature_list                   { $1 :: $3 }
  ;

feature:
    IDENTIFIER COLON TYPE                       { Attribute($1, $3, None) }  
  | IDENTIFIER COLON TYPE LARROW expr           { Attribute($1, $3, Some $5) }
  | IDENTIFIER LPAREN formal_list RPAREN COLON TYPE LBRACE expr RBRACE
                                                { Method($1, $3, $6, $8) }  
  ;

formal_list:
    /* lambda */                                { [] }
  | formal                                      { [$1] }
  | formal COMMA formal_list                    { $1 :: $3 } 
  ;

formal:
    IDENTIFIER COLON TYPE                       { $1, $3 }
  ;

expr_list:
    /* lambda */                                { [] }
  | expr                                        { [$1] }
  | expr COMMA expr_list                        { $1 :: $3 }
  ;

expr:
  |  assign_expr                                { $1 }
  ;

assign_expr:
    IDENTIFIER LARROW expr                      { let (l,_) = $1 in create_node l (Assign($1, $3)) }
  | cmp_expr                                    { $1 }
  ;

cmp_expr:
    cmp_expr LT     sum_expr                    { create_node $1.loc (Lt($1, $3)) }
  | cmp_expr LE     sum_expr                    { create_node $1.loc (Le($1, $3)) }
  | cmp_expr EQUALS sum_expr                    { create_node $1.loc (Equals($1, $3)) }
  | sum_expr                                    { $1 }
  ;

sum_expr:
    sum_expr PLUS  product_expr                 { create_node $1.loc (Plus($1, $3)) }
  | sum_expr MINUS product_expr                 { create_node $1.loc (Minus($1, $3)) }
  | product_expr                                { $1 }
  ;

product_expr:
    product_expr TIMES  unary_expr              { create_node $1.loc (Times($1, $3)) }
  | product_expr DIVIDE unary_expr              { create_node $1.loc (Divide($1, $3))}
  | unary_expr                                  { $1 }
  ;

unary_expr:
    TILDE  unary_expr                           { create_node $1 (Tilde($2)) }
  | NOT    unary_expr                           { create_node $1 (Not($2)) }
  | ISVOID unary_expr                           { create_node $1 (Isvoid($2)) }
  | atom                                        { $1 }
  ;

atom: 
    IF expr THEN expr ELSE expr FI              { create_node $1 (If($2, $4, $6)) }
  | WHILE expr LOOP expr POOL                   { create_node $1 (While($2, $4)) }
  | LET let_binding_list IN expr                { create_node $1 (Let($2, $4)) }
  | CASE expr OF case_list ESAC                 { create_node $1 (Case($2, $4)) }
  | primary_expr                                { $1 }
  ; 

primary_expr:
    primary_base                                { $1 }
  | primary_expr DOT IDENTIFIER LPAREN expr_list RPAREN
                                                { create_node $1.loc (DynamicDispatch($1, $3, $5)) }
  | primary_expr AT TYPE DOT IDENTIFIER LPAREN expr_list RPAREN
                                                { create_node $1.loc (StaticDispatch($1, $3, $5, $7)) }
  ;

primary_base:
    IDENTIFIER LPAREN expr_list RPAREN          { let (l, _) = $1 in create_node l (SelfDispatch($1, $3)) }
  | NEW TYPE                                    { create_node $1 (New($2)) }
  | LPAREN expr RPAREN                          { $2 }
  | LBRACE block_elems RBRACE                   { create_node $1 (Block $2) }
  | IDENTIFIER                                  { let (l, _) = $1 in create_node l (Identifier($1)) }
  | INTEGER                                     { let (l, lit) = $1 in create_node l (Integer(lit)) }
  | STRING                                      { let (l, lit) = $1 in create_node l (String(lit)) }
  | TRUE                                        { create_node $1 (True) }
  | FALSE                                       { create_node $1 (False) }
  ;

block_elems:
    expr SEMI                                   { [$1] }
  | expr SEMI block_elems                       { $1 :: $3 }
  ;

let_binding:
    IDENTIFIER COLON TYPE                       { ($1, $3, None) }
  | IDENTIFIER COLON TYPE LARROW expr           { ($1, $3, Some $5) }
  ;

let_binding_list:
    let_binding                                 { [$1] }
  | let_binding COMMA let_binding_list          { $1 :: $3 }
  ;

case_branch:
    IDENTIFIER COLON TYPE RARROW expr           { ($1, $3, $5) }
  ;

case_list:
    case_branch SEMI                            { [$1] }
  | case_branch SEMI case_list                  { $1 :: $3 }
  ;

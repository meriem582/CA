%{
open Ast
%}
 
%token LET IN IF THEN ELSE FUN EQ
%token LT GT LEQ GEQ EQEQ


%token <int> INT
%token <string> IDENT

%token TRUE FALSE
%token REC
%token EOF LPAREN RPAREN COMMA 
%token  RIGHT_ARROW
%token PLUS MINUS TIMES DIV


%nonassoc LET 
%nonassoc IN
%nonassoc  IF                          /* lowest precedence */
%right     RIGHT_ARROW
/* %right     COMMA*/
                
%nonassoc  IDENT LPAREN         /* highest precedence */        

%start prog         /* the entry point */

%type <Ast.expr>  prog

%%

prog :
  | expression EOF { $1 }
;
expression :
  | LPAREN expression RPAREN                        { $2 }
  | IDENT                                           { Ident($1) }
  | INT                                             { Number($1) }
  | FALSE                                           { False }
  | TRUE                                            { True }
  | FUN pat RIGHT_ARROW expression                  { Lambda($2,$4) }
  | LET pat EQ expression IN expression             { Let($2,$4,$6) }
  | LET REC pat EQ expression IN expression         { LetRec($3,$5,$7) }
  | IF expression THEN expression ELSE expression   { If($2,$4,$6) }
  | expression expression                           { Apply($1,$2) }
  | LPAREN expression COMMA expression RPAREN       { Mlpair($2,$4) }
  | expression PLUS expression                      { Apply(Apply(Ident "+", $1), $3) }
  | expression MINUS expression                     { Apply(Apply(Ident "-", $1), $3) }
  | expression TIMES expression                     { Apply(Apply(Ident "*", $1), $3) }
  | expression DIV expression                       { Apply(Apply(Ident "/", $1), $3) }
  | expression LT expression                        { Apply(Apply(Ident "<", $1), $3) }
  | expression GT expression                        { Apply(Apply(Ident ">", $1), $3) }
  | expression LEQ expression                       { Apply(Apply(Ident "<=", $1), $3) }
  | expression GEQ expression                       { Apply(Apply(Ident ">=", $1), $3) }
  | expression EQEQ expression                      { Apply(Apply(Ident "==", $1), $3) }
  | MINUS expression                     { Apply(Ident "-", $2) }  
;

pat:
  | LPAREN pat RPAREN                        { $2 }
  | IDENT                                    { IdentPat($1) }
  | LPAREN RPAREN                            { NullPat } 
  | LPAREN pat COMMA pat RPAREN              { Pairpat($2,$4) }
;
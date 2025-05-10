%{
open Ast
%}
 
%token LET IN IF THEN ELSE FUN EQ FST SND
%token LT GT LEQ GEQ


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
  | FALSE                                           { Bool false }
  | TRUE                                            { Bool true }
  | FUN pat RIGHT_ARROW expression                  { Lambda($2,$4) }
  | LET pat EQ expression IN expression             { MLin (Let ($2, $4), $6) }
  | LET IDENT pat EQ expression IN expression       { MLin (Let (Identpat $2, Lambda ($3, $5)), $7) }
  | LET REC pat EQ expression IN expression         { MLin (Letrec ($3, $5), $7) }
  | LET REC IDENT pat EQ expression IN expression   { MLin (Letrec (Identpat $3, Lambda ($4, $6)), $8) }
  | IF expression THEN expression ELSE expression   { If($2,$4,$6) }
  | expression expression                           { Apply($1,$2) }
  | LPAREN expression COMMA expression RPAREN       { MLpair($2,$4) }
  | expression PLUS expression                      { Apply (Op MLadd, MLpair($1, $3)) }
  | expression MINUS expression                     { Apply (Op MLsub, MLpair($1, $3)) }
  | expression TIMES expression                     { Apply (Op MLmult, MLpair($1, $3)) }
  | expression DIV expression                       { Apply (Op MLdiv, MLpair($1, $3)) }
  | expression LT expression                        { Apply (Op MLlt, MLpair($1, $3)) }
  | expression GT expression                        { Apply (Op MLgt, MLpair($1, $3)) }
  | expression EQ expression                        { Apply (Op MLeq, MLpair($1, $3)) } 
  | expression LEQ expression                       { Apply (Op MLleq, MLpair($1, $3)) }
  | expression GEQ expression                       { Apply (Op MLgeq, MLpair($1, $3)) }
  | FST expression                                  { Apply (MLfst, $2) }
  | SND expression                                  { Apply (MLsnd, $2) }

;

pat:
  | LPAREN pat RPAREN                                { $2 }
  | IDENT                                            { Identpat($1) }
  | LPAREN RPAREN                                    { Nullpat } 
  | LPAREN pat COMMA pat RPAREN                      { Pairpat($2,$4) }
;
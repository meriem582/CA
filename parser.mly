%{
open Ast
%}

%left PLUS MINUS
%left MUL DIV AND OR NOT
%nonassoc LPAREN RPAREN
%token MUL, PLUS, LPAREN, RPAREN, EOL, MINUS, DIV, AND, OR, NOT, PRINT, SEMICOLON, LT, GT, EQ, LET, INPUT, GOTO, END
%token <string> REM
%token<int> INTEGER
%token <string> IDENT
%start calc
%type <Ast.program> calc
%type <Ast.expr> expr
%type <Ast.expr> term
%type <Ast.expr> factor


%%

calc:
  | line_list { $1 }
;

line_list:
  | line EOL { [$1] }
  | line_list EOL line { $3 :: $1 }
;

line:
  | INTEGER instr  { ($1, $2) }
;

instr : PRINT seq { Print($2) } 
     | REM IDENT { Rem($2) } 
     | LET IDENT EQ expr { Let($2, $4) }
     | INPUT IDENT { Input($2) }
     | GOTO INTEGER { Goto($2) }
     | END { End }
;

seq : {[]}
    | expr { [$1] }
    | expr SEMICOLON seq { $1::$3 }
    ;


expr : expr PLUS expr { Add($1, $3) }  
     | expr MINUS expr { Sub($1, $3) }
     | expr EQ expr { Eq($1, $3) }
     | expr LT expr { Lt($1, $3) }
     | expr GT expr { Gt($1, $3) }
     | term  { $1 };

term : term MUL term { Mul($1, $3) }
     | term DIV term {Div($1, $3)}
     | term AND term { And($1, $3) }
     | term OR term { Or($1, $3) }
     | NOT term { Not($2) }
     | factor  { $1 } ;

factor : INTEGER { Integer($1) }
       | LPAREN expr RPAREN { $2 } 
       | MINUS expr { Neg($2) }
       | PLUS expr { Pos($2) };

%%
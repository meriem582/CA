%{
open Ast
%}
%token EOF
%left PLUS MINUS
%left MUL DIV AND OR NOT
%nonassoc LPAREN RPAREN
%token MUL, PLUS, LPAREN, RPAREN, EOL, MINUS, DIV, AND, OR, NOT, PRINT, SEMICOLON, LT, GT, EQ, LET, INPUT, GOTO, END, IF, GOSUB, RETURN
%token <string> REM
%token<int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%start calc
%type <Ast.program> calc
%type <Ast.expr> expr
%type <Ast.expr> term
%type <Ast.expr> factor


%%

calc:
  | line_list EOF { List.mapi (fun i instr -> (i + 1, instr)) $1 }
;

line_list:
  | line { [$1] }
  | line line_list { $1 :: $2 }
;

line:
  | instr EOL { $1 }
;

instr : PRINT seq { Printf.printf "PARSER : instruction print traitée\n";  Print($2) } 
     | REM STRING { Printf.printf "PARSER : instruction REM traitée\n";  Rem($2) } 
     | LET IDENT EQ expr { Printf.printf "PARSER : instruction LET traitée\n";  Let($2, $4) }
     | INPUT IDENT { Input($2) }
     | GOTO INTEGER { Goto($2) }
     | END {  Printf.printf "PARSER : instruction END traitée\n";  End }
     | IF expr GOTO INTEGER { Printf.printf "PARSER : instruction IFG traitée\n"; If($2,$4) }
     | GOSUB INTEGER  { Printf.printf "PARSER : instruction GOSUB traitée\n"; Gosub ($2) }
     | RETURN { Printf.printf "PARSER : instruction RETURN traitée\n"; Return }
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
       | FLOAT { Float($1) } 
       | STRING { String($1) }
       | LPAREN expr RPAREN { $2 } 
       | MINUS expr { Neg($2) }
       | PLUS expr { Pos($2) };

%%
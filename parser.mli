type token =
  | PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR
  | NOT
  | LPAREN
  | RPAREN
  | EOL
  | PRINT
  | SEMICOLON
  | LT
  | GT
  | EQ
  | LET
  | INPUT
  | GOTO
  | END
  | REM of (
# 9 "parser.mly"
        string
# 25 "parser.mli"
)
  | INTEGER of (
# 10 "parser.mly"
       int
# 30 "parser.mli"
)
  | IDENT of (
# 11 "parser.mly"
        string
# 35 "parser.mli"
)

val calc :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program

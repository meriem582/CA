type token =
  | EOF
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
  | IF
  | GOSUB
  | RETURN
  | REM of (
# 9 "parser.mly"
        string
# 29 "parser.mli"
)
  | INTEGER of (
# 10 "parser.mly"
       int
# 34 "parser.mli"
)
  | FLOAT of (
# 11 "parser.mly"
        float
# 39 "parser.mli"
)
  | STRING of (
# 12 "parser.mly"
        string
# 44 "parser.mli"
)
  | IDENT of (
# 13 "parser.mly"
        string
# 49 "parser.mli"
)

val calc :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program

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
  | REM of (string)
  | INTEGER of (int)
  | IDENT of (string)

val calc :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program

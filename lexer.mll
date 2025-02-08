{
  open Parser
  exception Eof
}

let integer = ['0'-'9']+
let float = ['0'-'9']+ "." ['0'-'9']+
let spaces = [' ' '\t']
let eol = '\n'
let ident = (['a'-'z' 'A'-'Z'])(['a'-'z' 'A'-'Z' '0'-'9'])*

rule token = parse
    spaces        { token lexbuf }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { MUL }
  | "/"           { DIV }
  | "("           { LPAREN }
  | ")"           { RPAREN }
  | ";"           { SEMICOLON }
  | "and"         { AND }
  | "or"          { OR }
  | "not"         { NOT }
  | "print"       { Printf.printf "LEXER : instruction print détectée\n" ;  PRINT }
  | "<"           { LT }
  | ">"           { GT }
  | "="           { EQ }
  | "rem"         { REM (Lexing.lexeme lexbuf) }  
  | "let"         { LET }
  | "input"       { INPUT }
  | "goto"        { GOTO }
  | ident as x    { IDENT(x) }
  | integer as x  { INTEGER(int_of_string x) }
  | eol           { Printf.printf "LEXER : fin de ligne \n"; EOL }
  | eof           { Printf.printf "LEXER : fin de fichier \n"; raise Eof }

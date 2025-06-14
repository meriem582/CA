{
  open Parser
  exception EOF
}

let integer = ['0'-'9']+
let float = ['0'-'9']+ "." ['0'-'9']+
let string = '"' [^'"']* '"'
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
  | "print"       { PRINT }
  | "<"           { LT }
  | ">"           { GT }
  | "="           { EQ }
  | "rem"         { REM (Lexing.lexeme lexbuf) }  
  | "let"         { Printf.printf "LEXER : LET \n"; LET }
  | "input"       { INPUT }
  | "goto"        { GOTO }
  | "end"         { Printf.printf "LEXER : instruction END détectée\n" ; END }
  | "if"          { Printf.printf "LEXER : instruction IF détectée\n" ; IF }  
  | "gosub"       { Printf.printf "LEXER : instruction GOSUB détectée\n" ; GOSUB }
  | "return"      { Printf.printf "LEXER : instruction RETURN détectée\n" ; RETURN }
  | integer as x  { INTEGER(int_of_string x) }
  | float as x    { FLOAT(float_of_string x) }  
  | string as s   { STRING(String.sub s 1 (String.length s - 2)) } 
  | ident as x    { IDENT(x) }
  | eol           { EOL }
  | eof           {  EOF }

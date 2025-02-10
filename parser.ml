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
  | REM of (string)
  | INTEGER of (int)
  | IDENT of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 31 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* PLUS *);
  258 (* MINUS *);
  259 (* MUL *);
  260 (* DIV *);
  261 (* AND *);
  262 (* OR *);
  263 (* NOT *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* EOL *);
  267 (* PRINT *);
  268 (* SEMICOLON *);
  269 (* LT *);
  270 (* GT *);
  271 (* EQ *);
  272 (* LET *);
  273 (* INPUT *);
  274 (* GOTO *);
  275 (* END *);
    0|]

let yytransl_block = [|
  276 (* REM *);
  277 (* INTEGER *);
  278 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\005\000\005\000\006\000\007\000\007\000\007\000\007\000\
\007\000\008\000\008\000\008\000\002\000\002\000\002\000\002\000\
\002\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\003\000\002\000\002\000\004\000\002\000\
\002\000\000\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\003\000\003\000\003\000\003\000\002\000\001\000\
\001\000\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\029\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\003\000\000\000\000\000\
\000\000\000\000\025\000\000\000\000\000\024\000\005\000\000\000\
\008\000\009\000\006\000\004\000\000\000\000\000\023\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\012\000\000\000\
\000\000\000\000\019\000\020\000\021\000\022\000\000\000"

let yydgoto = "\002\000\
\004\000\020\000\021\000\022\000\005\000\006\000\012\000\023\000"

let yysindex = "\001\000\
\241\254\000\000\253\254\000\000\009\000\241\254\003\255\246\254\
\250\254\255\254\005\255\025\255\000\000\000\000\003\255\003\255\
\003\255\003\255\000\000\039\255\137\255\000\000\000\000\027\255\
\000\000\000\000\000\000\000\000\131\255\131\255\000\000\046\255\
\003\255\003\255\003\255\003\255\003\255\003\255\003\255\003\255\
\003\255\003\255\003\255\000\000\131\255\131\255\000\000\043\255\
\043\255\043\255\000\000\000\000\000\000\000\000\043\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\043\000\036\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\255\024\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\061\255\073\255\000\000\000\000\
\000\000\000\000\036\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\085\255\097\255\000\000\107\255\
\117\255\127\255\000\000\000\000\000\000\000\000\058\255"

let yygindex = "\000\000\
\000\000\241\255\246\255\000\000\063\000\000\000\000\000\014\000"

let yytablesize = 146
let yytable = "\029\000\
\030\000\001\000\032\000\015\000\016\000\003\000\031\000\007\000\
\013\000\017\000\018\000\024\000\008\000\009\000\010\000\025\000\
\011\000\045\000\046\000\026\000\048\000\049\000\050\000\019\000\
\018\000\018\000\027\000\055\000\051\000\052\000\053\000\054\000\
\018\000\018\000\028\000\018\000\018\000\018\000\018\000\033\000\
\034\000\043\000\002\000\033\000\034\000\010\000\033\000\034\000\
\047\000\011\000\035\000\036\000\037\000\038\000\044\000\036\000\
\037\000\038\000\036\000\037\000\038\000\028\000\028\000\028\000\
\028\000\028\000\028\000\007\000\014\000\028\000\028\000\000\000\
\028\000\027\000\027\000\027\000\027\000\027\000\027\000\000\000\
\000\000\027\000\027\000\000\000\027\000\013\000\013\000\013\000\
\013\000\013\000\013\000\000\000\000\000\013\000\013\000\000\000\
\013\000\014\000\014\000\014\000\014\000\014\000\014\000\000\000\
\000\000\014\000\014\000\000\000\014\000\016\000\016\000\016\000\
\016\000\000\000\000\000\016\000\016\000\000\000\016\000\017\000\
\017\000\017\000\017\000\000\000\000\000\017\000\017\000\000\000\
\017\000\015\000\015\000\015\000\015\000\000\000\000\000\015\000\
\015\000\000\000\015\000\039\000\040\000\041\000\042\000\036\000\
\037\000\038\000"

let yycheck = "\015\000\
\016\000\001\000\018\000\001\001\002\001\021\001\017\000\011\001\
\000\000\007\001\008\001\022\001\016\001\017\001\018\001\022\001\
\020\001\033\000\034\000\021\001\036\000\037\000\038\000\021\001\
\001\001\002\001\022\001\043\000\039\000\040\000\041\000\042\000\
\009\001\010\001\010\001\012\001\013\001\014\001\015\001\001\001\
\002\001\015\001\000\000\001\001\002\001\010\001\001\001\002\001\
\035\000\010\001\012\001\013\001\014\001\015\001\009\001\013\001\
\014\001\015\001\013\001\014\001\015\001\001\001\002\001\003\001\
\004\001\005\001\006\001\010\001\006\000\009\001\010\001\255\255\
\012\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\009\001\010\001\255\255\012\001\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\255\255\009\001\010\001\255\255\
\012\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\009\001\010\001\255\255\012\001\003\001\004\001\005\001\
\006\001\255\255\255\255\009\001\010\001\255\255\012\001\003\001\
\004\001\005\001\006\001\255\255\255\255\009\001\010\001\255\255\
\012\001\003\001\004\001\005\001\006\001\255\255\255\255\009\001\
\010\001\255\255\012\001\003\001\004\001\005\001\006\001\013\001\
\014\001\015\001"

let yynames_const = "\
  EOF\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  AND\000\
  OR\000\
  NOT\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  PRINT\000\
  SEMICOLON\000\
  LT\000\
  GT\000\
  EQ\000\
  LET\000\
  INPUT\000\
  GOTO\000\
  END\000\
  "

let yynames_block = "\
  REM\000\
  INTEGER\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'line_list) in
    Obj.repr(
# 22 "parser.mly"
                  ( _1 )
# 185 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 26 "parser.mly"
         ( [_1] )
# 192 "parser.ml"
               : 'line_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'line) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line_list) in
    Obj.repr(
# 27 "parser.mly"
                   ( _1 :: _2 )
# 200 "parser.ml"
               : 'line_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 31 "parser.mly"
                      ( (_1, _2) )
# 208 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 34 "parser.mly"
                  ( Printf.printf "PARSER : instruction print traitée\n";  Print(_2) )
# 215 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "parser.mly"
                 ( Printf.printf "PARSER : instruction REM traitée\n";  Rem(_2) )
# 223 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 36 "parser.mly"
                         ( Printf.printf "PARSER : instruction LET traitée\n";  Let(_2, _4) )
# 231 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "parser.mly"
                   ( Input(_2) )
# 238 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 38 "parser.mly"
                    ( Goto(_2) )
# 245 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
      ([])
# 251 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 43 "parser.mly"
           ( [_1] )
# 258 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 44 "parser.mly"
                         ( _1::_3 )
# 266 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                      ( Add(_1, _3) )
# 274 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                       ( Sub(_1, _3) )
# 282 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                    ( Eq(_1, _3) )
# 290 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                    ( Lt(_1, _3) )
# 298 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                    ( Gt(_1, _3) )
# 306 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
             ( _1 )
# 313 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                     ( Mul(_1, _3) )
# 321 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                     (Div(_1, _3))
# 329 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                     ( And(_1, _3) )
# 337 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 58 "parser.mly"
                    ( Or(_1, _3) )
# 345 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 59 "parser.mly"
                ( Not(_2) )
# 352 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 60 "parser.mly"
               ( _1 )
# 359 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 62 "parser.mly"
                 ( Integer(_1) )
# 366 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                            ( _2 )
# 373 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                    ( Neg(_2) )
# 380 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                   ( Pos(_2) )
# 387 "parser.ml"
               : Ast.expr))
(* Entry calc *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let calc (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
;;

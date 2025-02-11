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
# 29 "parser.ml"
)
  | INTEGER of (
# 10 "parser.mly"
       int
# 34 "parser.ml"
)
  | FLOAT of (
# 11 "parser.mly"
        float
# 39 "parser.ml"
)
  | STRING of (
# 12 "parser.mly"
        string
# 44 "parser.ml"
)
  | IDENT of (
# 13 "parser.mly"
        string
# 49 "parser.ml"
)

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 56 "parser.ml"
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
  276 (* IF *);
  277 (* GOSUB *);
  278 (* RETURN *);
    0|]

let yytransl_block = [|
  279 (* REM *);
  280 (* INTEGER *);
  281 (* FLOAT *);
  282 (* STRING *);
  283 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\005\000\005\000\006\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\008\000\008\000\008\000\
\002\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\002\000\002\000\002\000\004\000\002\000\
\002\000\001\000\004\000\002\000\001\000\000\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\003\000\003\000\
\003\000\003\000\002\000\001\000\001\000\001\000\001\000\003\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
\000\000\013\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\030\000\031\000\000\000\000\000\
\028\000\005\000\000\000\008\000\009\000\000\000\012\000\006\000\
\001\000\003\000\004\000\000\000\000\000\027\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\032\000\000\000\000\000\016\000\000\000\
\000\000\000\000\023\000\024\000\025\000\026\000\000\000\011\000"

let yydgoto = "\002\000\
\012\000\023\000\024\000\025\000\013\000\014\000\015\000\026\000"

let yysindex = "\004\000\
\165\255\000\000\005\255\239\254\243\254\248\254\000\000\005\255\
\255\254\000\000\001\255\000\000\024\000\165\255\031\255\005\255\
\005\255\005\255\005\255\000\000\000\000\000\000\148\255\254\254\
\000\000\000\000\038\255\000\000\000\000\100\255\000\000\000\000\
\000\000\000\000\000\000\025\255\025\255\000\000\155\255\005\255\
\005\255\005\255\005\255\005\255\005\255\005\255\005\255\005\255\
\005\255\005\255\030\255\000\000\025\255\025\255\000\000\164\255\
\164\255\164\255\000\000\000\000\000\000\000\000\164\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\047\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\059\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\050\255\094\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\016\255\046\255\000\000\000\000\000\000\
\000\000\047\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\064\255\082\255\000\000\116\255\
\126\255\136\255\000\000\000\000\000\000\000\000\051\255\000\000"

let yygindex = "\000\000\
\000\000\248\255\253\255\000\000\048\000\000\000\000\000\021\000"

let yytablesize = 188
let yytable = "\030\000\
\046\000\047\000\048\000\049\000\001\000\016\000\017\000\036\000\
\037\000\027\000\039\000\018\000\019\000\028\000\038\000\029\000\
\034\000\034\000\034\000\034\000\034\000\034\000\031\000\033\000\
\034\000\034\000\032\000\034\000\020\000\021\000\022\000\053\000\
\054\000\034\000\056\000\057\000\058\000\043\000\044\000\045\000\
\035\000\063\000\059\000\060\000\061\000\062\000\033\000\033\000\
\033\000\033\000\033\000\033\000\050\000\064\000\033\000\033\000\
\014\000\033\000\002\000\015\000\007\000\034\000\055\000\033\000\
\017\000\017\000\017\000\017\000\017\000\017\000\000\000\000\000\
\017\000\017\000\000\000\017\000\000\000\000\000\000\000\000\000\
\000\000\017\000\018\000\018\000\018\000\018\000\018\000\018\000\
\000\000\000\000\018\000\018\000\000\000\018\000\022\000\022\000\
\000\000\000\000\000\000\018\000\040\000\041\000\022\000\022\000\
\000\000\022\000\022\000\022\000\022\000\000\000\000\000\022\000\
\043\000\044\000\045\000\000\000\000\000\051\000\020\000\020\000\
\020\000\020\000\000\000\000\000\020\000\020\000\000\000\020\000\
\021\000\021\000\021\000\021\000\000\000\020\000\021\000\021\000\
\000\000\021\000\019\000\019\000\019\000\019\000\000\000\021\000\
\019\000\019\000\000\000\019\000\040\000\041\000\000\000\000\000\
\000\000\019\000\000\000\040\000\041\000\000\000\000\000\042\000\
\043\000\044\000\045\000\052\000\040\000\041\000\000\000\043\000\
\044\000\045\000\000\000\000\000\000\000\000\000\000\000\003\000\
\043\000\044\000\045\000\000\000\004\000\005\000\006\000\007\000\
\008\000\009\000\010\000\011\000"

let yycheck = "\008\000\
\003\001\004\001\005\001\006\001\001\000\001\001\002\001\016\000\
\017\000\027\001\019\000\007\001\008\001\027\001\018\000\024\001\
\001\001\002\001\003\001\004\001\005\001\006\001\024\001\000\000\
\009\001\010\001\026\001\012\001\024\001\025\001\026\001\040\000\
\041\000\018\001\043\000\044\000\045\000\013\001\014\001\015\001\
\010\001\050\000\046\000\047\000\048\000\049\000\001\001\002\001\
\003\001\004\001\005\001\006\001\015\001\024\001\009\001\010\001\
\010\001\012\001\000\000\010\001\010\001\014\000\042\000\018\001\
\001\001\002\001\003\001\004\001\005\001\006\001\255\255\255\255\
\009\001\010\001\255\255\012\001\255\255\255\255\255\255\255\255\
\255\255\018\001\001\001\002\001\003\001\004\001\005\001\006\001\
\255\255\255\255\009\001\010\001\255\255\012\001\001\001\002\001\
\255\255\255\255\255\255\018\001\001\001\002\001\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\255\255\255\255\018\001\
\013\001\014\001\015\001\255\255\255\255\018\001\003\001\004\001\
\005\001\006\001\255\255\255\255\009\001\010\001\255\255\012\001\
\003\001\004\001\005\001\006\001\255\255\018\001\009\001\010\001\
\255\255\012\001\003\001\004\001\005\001\006\001\255\255\018\001\
\009\001\010\001\255\255\012\001\001\001\002\001\255\255\255\255\
\255\255\018\001\255\255\001\001\002\001\255\255\255\255\012\001\
\013\001\014\001\015\001\009\001\001\001\002\001\255\255\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\011\001\
\013\001\014\001\015\001\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001"

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
  IF\000\
  GOSUB\000\
  RETURN\000\
  "

let yynames_block = "\
  REM\000\
  INTEGER\000\
  FLOAT\000\
  STRING\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'line_list) in
    Obj.repr(
# 24 "parser.mly"
                  ( List.mapi (fun i instr -> (i + 1, instr)) _1 )
# 235 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 28 "parser.mly"
         ( [_1] )
# 242 "parser.ml"
               : 'line_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'line) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line_list) in
    Obj.repr(
# 29 "parser.mly"
                   ( _1 :: _2 )
# 250 "parser.ml"
               : 'line_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 33 "parser.mly"
              ( _1 )
# 257 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 36 "parser.mly"
                  ( Printf.printf "PARSER : instruction print traitée\n";  Print(_2) )
# 264 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "parser.mly"
                  ( Printf.printf "PARSER : instruction REM traitée\n";  Rem(_2) )
# 272 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 38 "parser.mly"
                         ( Printf.printf "PARSER : instruction LET traitée\n";  Let(_2, _4) )
# 280 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                   ( Input(_2) )
# 287 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 40 "parser.mly"
                    ( Goto(_2) )
# 294 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
           (  Printf.printf "PARSER : instruction END traitée\n";  End )
# 300 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                            ( Printf.printf "PARSER : instruction IFG traitée\n"; If(_2,_4) )
# 308 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 43 "parser.mly"
                      ( Printf.printf "PARSER : instruction GOSUB traitée\n"; Gosub (_2) )
# 315 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
              ( Printf.printf "PARSER : instruction RETURN traitée\n"; Return )
# 321 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
      ([])
# 327 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
           ( [_1] )
# 334 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 49 "parser.mly"
                         ( _1::_3 )
# 342 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
                      ( Add(_1, _3) )
# 350 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                       ( Sub(_1, _3) )
# 358 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                    ( Eq(_1, _3) )
# 366 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                    ( Lt(_1, _3) )
# 374 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                    ( Gt(_1, _3) )
# 382 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 58 "parser.mly"
             ( _1 )
# 389 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 60 "parser.mly"
                     ( Mul(_1, _3) )
# 397 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
                     (Div(_1, _3))
# 405 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                     ( And(_1, _3) )
# 413 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                    ( Or(_1, _3) )
# 421 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                ( Not(_2) )
# 428 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
               ( _1 )
# 435 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
                 ( Integer(_1) )
# 442 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 68 "parser.mly"
               ( Float(_1) )
# 449 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                ( String(_1) )
# 456 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                            ( _2 )
# 463 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                    ( Neg(_2) )
# 470 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
                   ( Pos(_2) )
# 477 "parser.ml"
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

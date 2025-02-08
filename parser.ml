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
  | Eof
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
  257 (* PLUS *);
  258 (* MINUS *);
  259 (* MUL *);
  260 (* DIV *);
  261 (* AND *);
  262 (* OR *);
  263 (* NOT *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* Eof *);
  267 (* EOL *);
  268 (* PRINT *);
  269 (* SEMICOLON *);
  270 (* LT *);
  271 (* GT *);
  272 (* EQ *);
  273 (* LET *);
  274 (* INPUT *);
  275 (* GOTO *);
  276 (* END *);
    0|]

let yytransl_block = [|
  277 (* REM *);
  278 (* INTEGER *);
  279 (* IDENT *);
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
\000\000\000\000\000\000\029\000\000\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\003\000\000\000\000\000\
\000\000\000\000\025\000\000\000\000\000\024\000\005\000\000\000\
\008\000\009\000\006\000\004\000\000\000\000\000\023\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\012\000\000\000\
\000\000\000\000\019\000\020\000\021\000\022\000\000\000"

let yydgoto = "\002\000\
\004\000\020\000\021\000\022\000\005\000\006\000\012\000\023\000"

let yysindex = "\005\000\
\242\254\000\000\133\255\000\000\248\254\000\000\003\255\245\254\
\246\254\243\254\253\254\013\255\000\000\000\000\003\255\003\255\
\003\255\003\255\000\000\041\255\152\255\000\000\000\000\017\255\
\000\000\000\000\000\000\000\000\001\255\001\255\000\000\044\255\
\003\255\003\255\003\255\003\255\003\255\003\255\003\255\003\255\
\003\255\003\255\003\255\000\000\001\255\001\255\000\000\047\255\
\047\255\047\255\000\000\000\000\000\000\000\000\047\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\255\025\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\063\255\076\255\000\000\000\000\
\000\000\000\000\024\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\089\255\102\255\000\000\113\255\
\124\255\135\255\000\000\000\000\000\000\000\000\033\255"

let yygindex = "\000\000\
\000\000\241\255\246\255\000\000\000\000\042\000\000\000\015\000"

let yytablesize = 158
let yytable = "\029\000\
\030\000\013\000\032\000\015\000\016\000\001\000\031\000\003\000\
\026\000\017\000\018\000\024\000\025\000\003\000\036\000\037\000\
\038\000\045\000\046\000\027\000\048\000\049\000\050\000\028\000\
\019\000\018\000\018\000\055\000\051\000\052\000\053\000\054\000\
\043\000\018\000\010\000\018\000\011\000\018\000\018\000\018\000\
\018\000\033\000\034\000\007\000\033\000\034\000\014\000\033\000\
\034\000\047\000\000\000\000\000\044\000\035\000\036\000\037\000\
\038\000\036\000\037\000\038\000\036\000\037\000\038\000\028\000\
\028\000\028\000\028\000\028\000\028\000\000\000\000\000\028\000\
\000\000\028\000\000\000\028\000\027\000\027\000\027\000\027\000\
\027\000\027\000\000\000\000\000\027\000\000\000\027\000\000\000\
\027\000\013\000\013\000\013\000\013\000\013\000\013\000\000\000\
\000\000\013\000\000\000\013\000\000\000\013\000\014\000\014\000\
\014\000\014\000\014\000\014\000\000\000\000\000\014\000\000\000\
\014\000\000\000\014\000\016\000\016\000\016\000\016\000\000\000\
\000\000\016\000\000\000\016\000\000\000\016\000\017\000\017\000\
\017\000\017\000\000\000\000\000\017\000\000\000\017\000\000\000\
\017\000\015\000\015\000\015\000\015\000\000\000\000\000\015\000\
\007\000\015\000\000\000\015\000\000\000\008\000\009\000\010\000\
\000\000\011\000\039\000\040\000\041\000\042\000"

let yycheck = "\015\000\
\016\000\010\001\018\000\001\001\002\001\001\000\017\000\022\001\
\022\001\007\001\008\001\023\001\023\001\022\001\014\001\015\001\
\016\001\033\000\034\000\023\001\036\000\037\000\038\000\011\001\
\022\001\001\001\002\001\043\000\039\000\040\000\041\000\042\000\
\016\001\009\001\011\001\011\001\011\001\013\001\014\001\015\001\
\016\001\001\001\002\001\011\001\001\001\002\001\005\000\001\001\
\002\001\035\000\255\255\255\255\009\001\013\001\014\001\015\001\
\016\001\014\001\015\001\016\001\014\001\015\001\016\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\009\001\
\255\255\011\001\255\255\013\001\001\001\002\001\003\001\004\001\
\005\001\006\001\255\255\255\255\009\001\255\255\011\001\255\255\
\013\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\009\001\255\255\011\001\255\255\013\001\001\001\002\001\
\003\001\004\001\005\001\006\001\255\255\255\255\009\001\255\255\
\011\001\255\255\013\001\003\001\004\001\005\001\006\001\255\255\
\255\255\009\001\255\255\011\001\255\255\013\001\003\001\004\001\
\005\001\006\001\255\255\255\255\009\001\255\255\011\001\255\255\
\013\001\003\001\004\001\005\001\006\001\255\255\255\255\009\001\
\012\001\011\001\255\255\013\001\255\255\017\001\018\001\019\001\
\255\255\021\001\003\001\004\001\005\001\006\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  AND\000\
  OR\000\
  NOT\000\
  LPAREN\000\
  RPAREN\000\
  Eof\000\
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
# 187 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 26 "parser.mly"
         ( [_1] )
# 194 "parser.ml"
               : 'line_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'line_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 27 "parser.mly"
                   ( _2 :: _1 )
# 202 "parser.ml"
               : 'line_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    Obj.repr(
# 31 "parser.mly"
                      ( (_1, _2) )
# 210 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 34 "parser.mly"
                  ( Print(_2) )
# 217 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "parser.mly"
                 ( Rem(_2) )
# 225 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 36 "parser.mly"
                         ( Let(_2, _4) )
# 233 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "parser.mly"
                   ( Input(_2) )
# 240 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 38 "parser.mly"
                    ( Goto(_2) )
# 247 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
      ([])
# 253 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 43 "parser.mly"
           ( [_1] )
# 260 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 44 "parser.mly"
                         ( _1::_3 )
# 268 "parser.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                      ( Add(_1, _3) )
# 276 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                       ( Sub(_1, _3) )
# 284 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                    ( Eq(_1, _3) )
# 292 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                    ( Lt(_1, _3) )
# 300 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                    ( Gt(_1, _3) )
# 308 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
             ( _1 )
# 315 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                     ( Mul(_1, _3) )
# 323 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                     (Div(_1, _3))
# 331 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                     ( And(_1, _3) )
# 339 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 58 "parser.mly"
                    ( Or(_1, _3) )
# 347 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 59 "parser.mly"
                ( Not(_2) )
# 354 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 60 "parser.mly"
               ( _1 )
# 361 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 62 "parser.mly"
                 ( Integer(_1) )
# 368 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                            ( _2 )
# 375 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                    ( Neg(_2) )
# 382 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                   ( Pos(_2) )
# 389 "parser.ml"
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

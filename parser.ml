type token =
  | ID of (string)
  | LIT_INT of (int)
  | FUNC
  | GO
  | WHILE
  | IF
  | ELSE
  | RETURN
  | PRINT
  | INT
  | BOOL
  | CHAN_INT
  | NEWCHANNEL
  | TRUE
  | FALSE
  | LPAREN
  | RPAREN
  | COMMA
  | LBRACE
  | RBRACE
  | SEMICOLON
  | LARROW
  | DECL
  | ASSIGN
  | AND
  | EQ
  | GT
  | NOT
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | EOF

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Ast
# 41 "parser.ml"
let yytransl_const = [|
  259 (* FUNC *);
  260 (* GO *);
  261 (* WHILE *);
  262 (* IF *);
  263 (* ELSE *);
  264 (* RETURN *);
  265 (* PRINT *);
  266 (* INT *);
  267 (* BOOL *);
  268 (* CHAN_INT *);
  269 (* NEWCHANNEL *);
  270 (* TRUE *);
  271 (* FALSE *);
  272 (* LPAREN *);
  273 (* RPAREN *);
  274 (* COMMA *);
  275 (* LBRACE *);
  276 (* RBRACE *);
  277 (* SEMICOLON *);
  278 (* LARROW *);
  279 (* DECL *);
  280 (* ASSIGN *);
  281 (* AND *);
  282 (* EQ *);
  283 (* GT *);
  284 (* NOT *);
  285 (* PLUS *);
  286 (* MINUS *);
  287 (* MULTIPLY *);
  288 (* DIVIDE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* LIT_INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\005\000\005\000\006\000\006\000\
\006\000\004\000\009\000\009\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\012\000\
\012\000\014\000\014\000\015\000\015\000\011\000\011\000\011\000\
\016\000\016\000\016\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\013\000\013\000\013\000\018\000\019\000\019\000\
\008\000\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\002\000\007\000\006\000\000\000\002\000\
\004\000\003\000\001\000\003\000\002\000\003\000\002\000\003\000\
\003\000\003\000\003\000\005\000\002\000\004\000\002\000\003\000\
\001\000\003\000\001\000\003\000\001\000\003\000\003\000\001\000\
\003\000\003\000\001\000\001\000\001\000\001\000\002\000\002\000\
\003\000\004\000\000\000\001\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\053\000\000\000\000\000\000\000\000\000\
\001\000\000\000\002\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\000\000\046\000\
\047\000\048\000\000\000\000\000\000\000\038\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\037\000\000\000\021\000\
\023\000\015\000\010\000\000\000\000\000\050\000\051\000\052\000\
\000\000\000\000\000\000\014\000\017\000\016\000\018\000\000\000\
\000\000\039\000\040\000\000\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\006\000\000\000\000\000\
\000\000\022\000\000\000\041\000\028\000\024\000\026\000\030\000\
\031\000\033\000\034\000\000\000\005\000\009\000\045\000\042\000\
\020\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\011\000\007\000\024\000\057\000\038\000\
\021\000\022\000\039\000\058\000\059\000\041\000\042\000\043\000\
\044\000\045\000\046\000"

let yysindex = "\007\000\
\023\255\000\000\035\255\000\000\039\000\029\255\023\255\026\255\
\000\000\065\255\000\000\000\000\048\255\011\255\029\255\039\255\
\039\255\039\255\039\255\049\255\037\255\038\255\000\000\041\255\
\034\255\039\255\039\255\015\255\039\255\000\000\044\255\000\000\
\000\000\000\000\039\255\061\255\039\255\000\000\036\255\029\255\
\040\255\046\255\239\254\244\254\000\000\000\000\029\255\000\000\
\000\000\000\000\000\000\065\255\255\254\000\000\000\000\000\000\
\057\255\058\255\062\255\000\000\000\000\000\000\000\000\039\255\
\068\255\000\000\000\000\039\255\000\000\039\255\039\255\039\255\
\039\255\039\255\039\255\071\255\000\000\000\000\029\255\048\255\
\039\255\000\000\069\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\255\000\000\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\072\255\000\000\000\000\000\000\000\000\000\000\072\255\000\000\
\000\000\000\000\000\000\000\000\084\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\082\255\000\000\000\000\
\000\000\086\255\000\000\000\000\000\000\000\000\063\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\104\255\000\000\
\123\255\114\255\093\255\079\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\090\255\098\255\000\000\000\000\000\000\000\000\000\000\086\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\084\255\
\086\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\109\000\241\255\000\000\037\000\073\000\244\255\
\075\000\000\000\235\255\242\255\199\255\000\000\057\000\204\255\
\099\000\000\000\000\000"

let yytablesize = 144
let yytable = "\030\000\
\025\000\040\000\047\000\048\000\049\000\060\000\083\000\001\000\
\054\000\055\000\056\000\072\000\073\000\062\000\063\000\031\000\
\032\000\010\000\074\000\075\000\065\000\090\000\091\000\095\000\
\069\000\003\000\026\000\061\000\033\000\034\000\035\000\076\000\
\027\000\028\000\029\000\008\000\036\000\078\000\009\000\031\000\
\032\000\013\000\037\000\054\000\055\000\056\000\085\000\010\000\
\023\000\050\000\088\000\089\000\033\000\034\000\035\000\086\000\
\051\000\053\000\052\000\064\000\036\000\066\000\068\000\093\000\
\070\000\014\000\037\000\025\000\015\000\016\000\017\000\071\000\
\018\000\019\000\080\000\081\000\097\000\092\000\082\000\049\000\
\049\000\049\000\049\000\049\000\084\000\096\000\020\000\049\000\
\049\000\049\000\003\000\049\000\049\000\049\000\049\000\035\000\
\035\000\035\000\035\000\035\000\007\000\011\000\043\000\035\000\
\035\000\035\000\008\000\035\000\035\000\032\000\032\000\032\000\
\032\000\032\000\044\000\012\000\094\000\032\000\032\000\032\000\
\029\000\029\000\029\000\029\000\029\000\079\000\077\000\087\000\
\029\000\029\000\027\000\027\000\027\000\027\000\027\000\067\000\
\000\000\000\000\027\000\025\000\025\000\025\000\025\000\025\000"

let yycheck = "\015\000\
\013\000\016\000\017\000\018\000\019\000\027\000\064\000\001\000\
\010\001\011\001\012\001\029\001\030\001\028\000\029\000\001\001\
\002\001\019\001\031\001\032\001\035\000\074\000\075\000\081\000\
\040\000\003\001\016\001\013\001\014\001\015\001\016\001\047\000\
\022\001\023\001\024\001\001\001\022\001\053\000\000\000\001\001\
\002\001\016\001\028\001\010\001\011\001\012\001\068\000\019\001\
\001\001\001\001\072\000\073\000\014\001\015\001\016\001\070\000\
\020\001\017\001\021\001\016\001\022\001\001\001\027\001\079\000\
\025\001\001\001\028\001\080\000\004\001\005\001\006\001\026\001\
\008\001\009\001\018\001\018\001\092\000\007\001\017\001\017\001\
\018\001\019\001\020\001\021\001\017\001\017\001\022\001\025\001\
\026\001\027\001\019\001\029\001\030\001\031\001\032\001\017\001\
\018\001\019\001\020\001\021\001\017\001\020\001\017\001\025\001\
\026\001\027\001\017\001\029\001\030\001\017\001\018\001\019\001\
\020\001\021\001\017\001\007\000\080\000\025\001\026\001\027\001\
\017\001\018\001\019\001\020\001\021\001\053\000\052\000\071\000\
\025\001\026\001\017\001\018\001\019\001\020\001\021\001\037\000\
\255\255\255\255\025\001\017\001\018\001\019\001\020\001\021\001"

let yynames_const = "\
  FUNC\000\
  GO\000\
  WHILE\000\
  IF\000\
  ELSE\000\
  RETURN\000\
  PRINT\000\
  INT\000\
  BOOL\000\
  CHAN_INT\000\
  NEWCHANNEL\000\
  TRUE\000\
  FALSE\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  LBRACE\000\
  RBRACE\000\
  SEMICOLON\000\
  LARROW\000\
  DECL\000\
  ASSIGN\000\
  AND\000\
  EQ\000\
  GT\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  MULTIPLY\000\
  DIVIDE\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  LIT_INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 16 "parser.mly"
                                  ( _1 )
# 241 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'proc_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 19 "parser.mly"
                                  ( Prog (_1, _2) )
# 249 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "parser.mly"
                                  ( [] )
# 255 "parser.ml"
               : 'proc_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'proc) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'proc_list) in
    Obj.repr(
# 23 "parser.mly"
                                  ( [_1] @ _2 )
# 263 "parser.ml"
               : 'proc_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'param) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'types) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 26 "parser.mly"
                                            ( Proc (_2, _4, Some(_6), _7) )
# 273 "parser.ml"
               : 'proc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 27 "parser.mly"
                                            ( Proc (_2, _4, None, _6) )
# 282 "parser.ml"
               : 'proc))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
                                  ( [] )
# 288 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vars) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 31 "parser.mly"
                                  ( [(_1, _2)] )
# 296 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'vars) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 32 "parser.mly"
                                  ( [(_1, _2)] @ _4 )
# 305 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 35 "parser.mly"
                                  ( _2 )
# 312 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 38 "parser.mly"
                                            ( _1 )
# 319 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'statement_list) in
    Obj.repr(
# 39 "parser.mly"
                                            ( Seq (_1, _3) )
# 327 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 42 "parser.mly"
                                  ( Go (_2) )
# 334 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 43 "parser.mly"
                                  ( Transmit (_1, _3) )
# 342 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                                  ( RcvStmt (_2) )
# 349 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 45 "parser.mly"
                                  ( Decl (_1, _3) )
# 357 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 46 "parser.mly"
                                  ( DeclChan (_1) )
# 364 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 47 "parser.mly"
                                  ( Assign (_1, _3) )
# 372 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 48 "parser.mly"
                                  ( While (_2, _3) )
# 380 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 49 "parser.mly"
                                  ( ITE (_2, _3, _5) )
# 389 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 50 "parser.mly"
                                  ( Return (_2) )
# 396 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arg) in
    Obj.repr(
# 51 "parser.mly"
                                  ( FuncCall (_1, _3) )
# 404 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 52 "parser.mly"
                                  ( Print (_2) )
# 411 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 55 "parser.mly"
                                  ( And (_1, _3) )
# 419 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cexp) in
    Obj.repr(
# 56 "parser.mly"
                                  ( _1 )
# 426 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cterm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cterm) in
    Obj.repr(
# 59 "parser.mly"
                                  ( Eq (_1, _3) )
# 434 "parser.ml"
               : 'cexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cterm) in
    Obj.repr(
# 60 "parser.mly"
                                  ( _1 )
# 441 "parser.ml"
               : 'cexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 63 "parser.mly"
                                  ( Gt (_1, _3) )
# 449 "parser.ml"
               : 'cterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 64 "parser.mly"
                                  ( _1 )
# 456 "parser.ml"
               : 'cterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 67 "parser.mly"
                                  ( Plus (_1, _3) )
# 464 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 68 "parser.mly"
                                  ( Minus (_1, _3) )
# 472 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 69 "parser.mly"
                                  ( _1 )
# 479 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 72 "parser.mly"
                                  ( Times (_1, _3) )
# 487 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 73 "parser.mly"
                                  ( Division (_1, _3) )
# 495 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 74 "parser.mly"
                                  ( _1 )
# 502 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ints) in
    Obj.repr(
# 77 "parser.mly"
                                  ( _1 )
# 509 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bools) in
    Obj.repr(
# 78 "parser.mly"
                                  ( _1 )
# 516 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vars) in
    Obj.repr(
# 79 "parser.mly"
                                  ( _1 )
# 523 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
                                  ( RcvExp (_2) )
# 530 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 81 "parser.mly"
                                  ( Not (_2) )
# 537 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bexp) in
    Obj.repr(
# 82 "parser.mly"
                                  ( _2 )
# 544 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arg) in
    Obj.repr(
# 83 "parser.mly"
                                  ( FuncExp (_1, _3) )
# 552 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                                  ( [] )
# 558 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 87 "parser.mly"
                                  ( [_1] )
# 565 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 88 "parser.mly"
                                  ( [_1] @ _3 )
# 573 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
                                  ( IConst (_1) )
# 580 "parser.ml"
               : 'ints))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                                  ( BConst (true) )
# 586 "parser.ml"
               : 'bools))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                                  ( BConst (false) )
# 592 "parser.ml"
               : 'bools))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
                                  ( Var (_1) )
# 599 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
                                  ( TyInt )
# 605 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
                                  ( TyBool )
# 611 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                                  ( TyChan TyInt )
# 617 "parser.ml"
               : 'types))
(* Entry parse *)
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
let parse (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)

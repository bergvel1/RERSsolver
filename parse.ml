type token =
  | INT of (int)
  | OUTPUT of (int)
  | CRASH of (int)
  | STRING of (string)
  | IDENT of (string)
  | DEREFIDENT of (string)
  | REFIDENT of (string)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | LT
  | GT
  | LEQ
  | GEQ
  | EQ
  | NEQ
  | SEMI
  | COLON
  | TERN
  | ASSIGN
  | PLUSASSIGN
  | MINUSASSIGN
  | IF
  | ELSE
  | NEG
  | LOGICALAND
  | LOGICALOR
  | LBRAC
  | RBRAC
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | EOF
  | VOIDTYPE
  | INTTYPE
  | INPUTERROR
  | INPUT
  | WHILE

open Parsing;;
let _ = parse_error;;
# 2 "parse.mly"
	open Abs_syn
# 50 "parse.ml"
let yytransl_const = [|
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* DIV *);
  268 (* MOD *);
  269 (* LT *);
  270 (* GT *);
  271 (* LEQ *);
  272 (* GEQ *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* SEMI *);
  276 (* COLON *);
  277 (* TERN *);
  278 (* ASSIGN *);
  279 (* PLUSASSIGN *);
  280 (* MINUSASSIGN *);
  281 (* IF *);
  282 (* ELSE *);
  283 (* NEG *);
  284 (* LOGICALAND *);
  285 (* LOGICALOR *);
  286 (* LBRAC *);
  287 (* RBRAC *);
  288 (* LPAREN *);
  289 (* RPAREN *);
  290 (* LBRACE *);
  291 (* RBRACE *);
  292 (* COMMA *);
    0 (* EOF *);
  293 (* VOIDTYPE *);
  294 (* INTTYPE *);
  295 (* INPUTERROR *);
  296 (* INPUT *);
  297 (* WHILE *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* OUTPUT *);
  259 (* CRASH *);
  260 (* STRING *);
  261 (* IDENT *);
  262 (* DEREFIDENT *);
  263 (* REFIDENT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\002\000\002\000\004\000\007\000\007\000\
\007\000\007\000\009\000\009\000\010\000\010\000\008\000\008\000\
\005\000\012\000\012\000\014\000\014\000\011\000\011\000\015\000\
\015\000\015\000\015\000\015\000\015\000\016\000\017\000\017\000\
\017\000\017\000\017\000\017\000\018\000\019\000\013\000\013\000\
\013\000\013\000\020\000\020\000\021\000\021\000\022\000\022\000\
\023\000\023\000\023\000\024\000\024\000\024\000\024\000\024\000\
\025\000\025\000\025\000\026\000\026\000\026\000\026\000\027\000\
\027\000\028\000\028\000\028\000\028\000\030\000\030\000\030\000\
\030\000\030\000\030\000\031\000\031\000\029\000\029\000\006\000\
\006\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\001\000\003\000\001\000\001\000\
\004\000\003\000\001\000\003\000\002\000\001\000\002\000\003\000\
\005\000\001\000\003\000\001\000\003\000\001\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000\005\000\005\000\
\007\000\011\000\010\000\010\000\007\000\001\000\001\000\003\000\
\003\000\003\000\001\000\005\000\001\000\003\000\001\000\003\000\
\001\000\003\000\003\000\001\000\003\000\003\000\003\000\003\000\
\001\000\003\000\003\000\001\000\003\000\003\000\003\000\001\000\
\002\000\001\000\004\000\003\000\004\000\001\000\001\000\001\000\
\001\000\001\000\003\000\001\000\003\000\001\000\001\000\001\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\081\000\080\000\082\000\002\000\000\000\
\004\000\005\000\000\000\083\000\001\000\003\000\000\000\008\000\
\000\000\000\000\000\000\000\000\006\000\010\000\000\000\000\000\
\011\000\073\000\074\000\000\000\071\000\072\000\079\000\078\000\
\000\000\000\000\000\000\018\000\039\000\000\000\000\000\000\000\
\000\000\000\000\000\000\060\000\064\000\000\000\066\000\027\000\
\028\000\000\000\015\000\029\000\000\000\000\000\038\000\022\000\
\024\000\025\000\026\000\000\000\013\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\017\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\065\000\
\000\000\000\000\016\000\023\000\030\000\012\000\040\000\041\000\
\042\000\000\000\068\000\076\000\000\000\075\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\061\000\062\000\063\000\000\000\000\000\
\067\000\069\000\000\000\021\000\000\000\000\000\000\000\077\000\
\044\000\032\000\000\000\031\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\000\000\000\000\000\000\000\000\035\000\
\036\000\000\000\034\000"

let yydgoto = "\003\000\
\006\000\007\000\008\000\009\000\010\000\011\000\017\000\021\000\
\024\000\025\000\054\000\035\000\055\000\071\000\056\000\057\000\
\058\000\059\000\060\000\037\000\038\000\039\000\040\000\041\000\
\042\000\043\000\044\000\045\000\046\000\047\000\101\000"

let yysindex = "\100\000\
\025\255\025\255\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\137\255\000\000\000\000\000\000\231\254\000\000\
\000\255\049\255\050\000\026\255\000\000\000\000\009\255\055\255\
\000\000\000\000\000\000\075\255\000\000\000\000\000\000\000\000\
\074\000\074\000\022\255\000\000\000\000\247\254\024\255\136\255\
\117\255\163\255\146\255\000\000\000\000\091\000\000\000\000\000\
\000\000\036\255\000\000\000\000\047\255\068\255\000\000\000\000\
\000\000\000\000\000\000\035\255\000\000\000\000\025\255\074\000\
\074\000\074\000\074\000\061\000\063\255\000\000\147\255\000\000\
\074\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
\091\000\091\000\091\000\091\000\091\000\091\000\008\255\000\000\
\074\000\074\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\088\255\000\000\000\000\056\255\000\000\000\000\074\000\
\086\255\024\255\136\255\117\255\117\255\163\255\163\255\163\255\
\163\255\146\255\146\255\000\000\000\000\000\000\095\255\102\255\
\000\000\000\000\074\000\000\000\091\000\005\255\112\255\000\000\
\000\000\000\000\109\255\000\000\020\000\122\255\120\255\161\255\
\125\255\143\255\000\000\020\000\153\255\172\255\213\255\000\000\
\000\000\224\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\084\255\000\000\
\000\000\000\000\000\000\116\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\255\145\001\246\254\
\087\001\227\000\145\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\116\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\163\001\084\000\107\001\127\001\251\000\019\001\043\001\
\067\001\174\000\203\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\040\000\000\000\000\000\000\000\243\255\000\000\000\000\
\000\000\088\000\129\255\000\000\237\255\000\000\204\255\000\000\
\000\000\000\000\226\255\055\000\000\000\117\000\119\000\108\000\
\059\000\107\000\227\255\000\000\000\000\000\000\000\000"

let yytablesize = 711
let yytable = "\036\000\
\013\000\092\000\069\000\135\000\023\000\136\000\018\000\130\000\
\047\000\047\000\047\000\073\000\142\000\061\000\070\000\146\000\
\088\000\047\000\047\000\074\000\047\000\019\000\047\000\007\000\
\047\000\047\000\026\000\048\000\049\000\027\000\028\000\029\000\
\030\000\020\000\031\000\007\000\098\000\067\000\131\000\068\000\
\072\000\012\000\105\000\132\000\095\000\096\000\097\000\014\000\
\100\000\023\000\050\000\075\000\032\000\093\000\116\000\117\000\
\118\000\033\000\119\000\120\000\051\000\004\000\005\000\043\000\
\043\000\052\000\053\000\089\000\026\000\048\000\049\000\027\000\
\028\000\029\000\030\000\043\000\031\000\043\000\090\000\043\000\
\043\000\022\000\092\000\092\000\124\000\004\000\005\000\062\000\
\122\000\092\000\063\000\123\000\050\000\092\000\032\000\102\000\
\064\000\065\000\066\000\033\000\001\000\002\000\091\000\128\000\
\067\000\125\000\068\000\052\000\053\000\026\000\048\000\049\000\
\027\000\028\000\029\000\030\000\014\000\031\000\121\000\014\000\
\026\000\048\000\049\000\027\000\028\000\029\000\030\000\126\000\
\031\000\078\000\079\000\080\000\081\000\050\000\127\000\032\000\
\110\000\111\000\112\000\113\000\033\000\015\000\016\000\134\000\
\050\000\133\000\032\000\137\000\052\000\053\000\094\000\033\000\
\076\000\077\000\138\000\084\000\085\000\086\000\140\000\052\000\
\053\000\026\000\048\000\049\000\027\000\028\000\029\000\030\000\
\141\000\031\000\082\000\083\000\026\000\048\000\049\000\027\000\
\028\000\029\000\030\000\129\000\031\000\103\000\104\000\108\000\
\109\000\050\000\143\000\032\000\114\000\115\000\106\000\000\000\
\033\000\107\000\000\000\139\000\050\000\000\000\032\000\000\000\
\052\000\053\000\000\000\033\000\000\000\000\000\144\000\000\000\
\000\000\000\000\000\000\052\000\053\000\026\000\048\000\049\000\
\027\000\028\000\029\000\030\000\000\000\031\000\000\000\000\000\
\026\000\048\000\049\000\027\000\028\000\029\000\030\000\000\000\
\031\000\000\000\000\000\000\000\000\000\050\000\000\000\032\000\
\000\000\000\000\000\000\000\000\033\000\000\000\000\000\145\000\
\050\000\000\000\032\000\000\000\052\000\053\000\000\000\033\000\
\000\000\000\000\147\000\000\000\000\000\000\000\000\000\052\000\
\053\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\000\000\033\000\000\000\000\000\026\000\048\000\049\000\027\000\
\028\000\029\000\030\000\000\000\031\000\000\000\000\000\000\000\
\000\000\033\000\000\000\033\000\000\000\004\000\005\000\000\000\
\033\000\000\000\000\000\033\000\050\000\000\000\032\000\000\000\
\033\000\033\000\026\000\033\000\000\000\027\000\028\000\029\000\
\030\000\000\000\031\000\052\000\053\000\026\000\000\000\000\000\
\027\000\028\000\029\000\030\000\000\000\031\000\000\000\000\000\
\000\000\000\000\026\000\000\000\032\000\027\000\028\000\029\000\
\030\000\033\000\031\000\034\000\000\000\000\000\000\000\032\000\
\000\000\000\000\000\000\026\000\033\000\099\000\027\000\087\000\
\029\000\030\000\000\000\031\000\032\000\000\000\048\000\048\000\
\048\000\033\000\000\000\000\000\000\000\000\000\000\000\048\000\
\048\000\000\000\048\000\000\000\048\000\032\000\048\000\048\000\
\000\000\000\000\033\000\070\000\070\000\070\000\070\000\070\000\
\070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
\070\000\000\000\000\000\000\000\000\000\000\000\000\000\070\000\
\070\000\000\000\070\000\000\000\070\000\000\000\070\000\070\000\
\057\000\057\000\000\000\000\000\000\000\057\000\057\000\057\000\
\057\000\057\000\057\000\057\000\057\000\057\000\000\000\000\000\
\000\000\000\000\000\000\000\000\057\000\057\000\000\000\057\000\
\000\000\057\000\000\000\057\000\057\000\058\000\058\000\000\000\
\000\000\000\000\058\000\058\000\058\000\058\000\058\000\058\000\
\058\000\058\000\058\000\000\000\000\000\000\000\000\000\000\000\
\000\000\058\000\058\000\000\000\058\000\000\000\058\000\000\000\
\058\000\058\000\059\000\059\000\000\000\000\000\000\000\059\000\
\059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
\000\000\000\000\000\000\000\000\000\000\000\000\059\000\059\000\
\000\000\059\000\000\000\059\000\000\000\059\000\059\000\052\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
\000\000\000\000\000\000\000\000\000\000\000\000\052\000\052\000\
\000\000\052\000\000\000\052\000\000\000\052\000\052\000\053\000\
\053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
\000\000\000\000\000\000\000\000\000\000\000\000\053\000\053\000\
\000\000\053\000\000\000\053\000\000\000\053\000\053\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
\000\000\000\000\000\000\000\000\000\000\000\000\054\000\054\000\
\000\000\054\000\000\000\054\000\000\000\054\000\054\000\055\000\
\055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
\000\000\000\000\000\000\000\000\000\000\000\000\055\000\055\000\
\000\000\055\000\000\000\055\000\000\000\055\000\055\000\056\000\
\056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
\000\000\000\000\000\000\000\000\000\000\000\000\056\000\056\000\
\000\000\056\000\000\000\056\000\000\000\056\000\056\000\049\000\
\049\000\049\000\049\000\049\000\000\000\000\000\000\000\000\000\
\000\000\000\000\049\000\049\000\000\000\049\000\000\000\049\000\
\000\000\049\000\049\000\050\000\050\000\050\000\050\000\050\000\
\000\000\000\000\000\000\000\000\000\000\000\000\050\000\050\000\
\000\000\050\000\000\000\050\000\000\000\050\000\050\000\051\000\
\051\000\051\000\051\000\051\000\000\000\000\000\000\000\000\000\
\000\000\000\000\051\000\051\000\000\000\051\000\000\000\051\000\
\000\000\051\000\051\000\045\000\045\000\045\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\000\000\000\045\000\
\000\000\045\000\000\000\045\000\045\000\046\000\046\000\046\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\
\000\000\046\000\000\000\046\000\000\000\046\000\046\000"

let yycheck = "\019\000\
\000\000\054\000\033\000\131\000\018\000\133\000\032\001\003\001\
\019\001\020\001\021\001\021\001\140\000\005\001\034\000\143\000\
\046\000\028\001\029\001\029\001\031\001\022\001\033\001\022\001\
\035\001\036\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\034\001\009\001\034\001\067\000\030\001\034\001\032\001\
\019\001\002\000\073\000\039\001\064\000\065\000\066\000\008\000\
\068\000\063\000\025\001\028\001\027\001\019\001\084\000\085\000\
\086\000\032\001\089\000\090\000\035\001\037\001\038\001\019\001\
\020\001\040\001\041\001\032\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\031\001\009\001\033\001\032\001\035\001\
\036\001\033\001\135\000\136\000\104\000\037\001\038\001\033\001\
\033\001\142\000\036\001\036\001\025\001\146\000\027\001\033\001\
\022\001\023\001\024\001\032\001\001\000\002\000\035\001\123\000\
\030\001\020\001\032\001\040\001\041\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\033\001\009\001\031\001\036\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\033\001\
\009\001\013\001\014\001\015\001\016\001\025\001\033\001\027\001\
\078\000\079\000\080\000\081\000\032\001\005\001\006\001\035\001\
\025\001\034\001\027\001\026\001\040\001\041\001\063\000\032\001\
\017\001\018\001\035\001\010\001\011\001\012\001\034\001\040\001\
\041\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\026\001\009\001\008\001\009\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\125\000\009\001\035\001\036\001\076\000\
\077\000\025\001\034\001\027\001\082\000\083\000\074\000\255\255\
\032\001\075\000\255\255\035\001\025\001\255\255\027\001\255\255\
\040\001\041\001\255\255\032\001\255\255\255\255\035\001\255\255\
\255\255\255\255\255\255\040\001\041\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\255\255\009\001\255\255\255\255\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\255\255\
\009\001\255\255\255\255\255\255\255\255\025\001\255\255\027\001\
\255\255\255\255\255\255\255\255\032\001\255\255\255\255\035\001\
\025\001\255\255\027\001\255\255\040\001\041\001\255\255\032\001\
\255\255\255\255\035\001\255\255\255\255\255\255\255\255\040\001\
\041\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\255\255\009\001\255\255\255\255\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\255\255\009\001\255\255\255\255\255\255\
\255\255\025\001\255\255\027\001\255\255\037\001\038\001\255\255\
\032\001\255\255\255\255\035\001\025\001\255\255\027\001\255\255\
\040\001\041\001\001\001\032\001\255\255\004\001\005\001\006\001\
\007\001\255\255\009\001\040\001\041\001\001\001\255\255\255\255\
\004\001\005\001\006\001\007\001\255\255\009\001\255\255\255\255\
\255\255\255\255\001\001\255\255\027\001\004\001\005\001\006\001\
\007\001\032\001\009\001\034\001\255\255\255\255\255\255\027\001\
\255\255\255\255\255\255\001\001\032\001\033\001\004\001\005\001\
\006\001\007\001\255\255\009\001\027\001\255\255\019\001\020\001\
\021\001\032\001\255\255\255\255\255\255\255\255\255\255\028\001\
\029\001\255\255\031\001\255\255\033\001\027\001\035\001\036\001\
\255\255\255\255\032\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\255\255\255\255\255\255\255\255\255\255\255\255\028\001\
\029\001\255\255\031\001\255\255\033\001\255\255\035\001\036\001\
\008\001\009\001\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\255\255\255\255\
\255\255\255\255\255\255\255\255\028\001\029\001\255\255\031\001\
\255\255\033\001\255\255\035\001\036\001\008\001\009\001\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\255\255\255\255\255\255\255\255\
\255\255\028\001\029\001\255\255\031\001\255\255\033\001\255\255\
\035\001\036\001\008\001\009\001\255\255\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\028\001\029\001\
\255\255\031\001\255\255\033\001\255\255\035\001\036\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\028\001\029\001\
\255\255\031\001\255\255\033\001\255\255\035\001\036\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\028\001\029\001\
\255\255\031\001\255\255\033\001\255\255\035\001\036\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\028\001\029\001\
\255\255\031\001\255\255\033\001\255\255\035\001\036\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\028\001\029\001\
\255\255\031\001\255\255\033\001\255\255\035\001\036\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\028\001\029\001\
\255\255\031\001\255\255\033\001\255\255\035\001\036\001\017\001\
\018\001\019\001\020\001\021\001\255\255\255\255\255\255\255\255\
\255\255\255\255\028\001\029\001\255\255\031\001\255\255\033\001\
\255\255\035\001\036\001\017\001\018\001\019\001\020\001\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\028\001\029\001\
\255\255\031\001\255\255\033\001\255\255\035\001\036\001\017\001\
\018\001\019\001\020\001\021\001\255\255\255\255\255\255\255\255\
\255\255\255\255\028\001\029\001\255\255\031\001\255\255\033\001\
\255\255\035\001\036\001\019\001\020\001\021\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\029\001\255\255\031\001\
\255\255\033\001\255\255\035\001\036\001\019\001\020\001\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\029\001\
\255\255\031\001\255\255\033\001\255\255\035\001\036\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  LT\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  EQ\000\
  NEQ\000\
  SEMI\000\
  COLON\000\
  TERN\000\
  ASSIGN\000\
  PLUSASSIGN\000\
  MINUSASSIGN\000\
  IF\000\
  ELSE\000\
  NEG\000\
  LOGICALAND\000\
  LOGICALOR\000\
  LBRAC\000\
  RBRAC\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  EOF\000\
  VOIDTYPE\000\
  INTTYPE\000\
  INPUTERROR\000\
  INPUT\000\
  WHILE\000\
  "

let yynames_block = "\
  INT\000\
  OUTPUT\000\
  CRASH\000\
  STRING\000\
  IDENT\000\
  DEREFIDENT\000\
  REFIDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'entries) in
    Obj.repr(
# 26 "parse.mly"
                                        ( _1 )
# 438 "parse.ml"
               : Abs_syn.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Abs_syn.block) in
    Obj.repr(
# 29 "parse.mly"
                        ( [_1] )
# 445 "parse.ml"
               : 'entries))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'entries) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Abs_syn.block) in
    Obj.repr(
# 30 "parse.mly"
                               ( (_1) @ [_2] )
# 453 "parse.ml"
               : 'entries))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_defintion) in
    Obj.repr(
# 34 "parse.mly"
                      ( _1 )
# 460 "parse.ml"
               : Abs_syn.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 35 "parse.mly"
                 ( GlobalInsts ([_1]) )
# 467 "parse.ml"
               : Abs_syn.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'declarator) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compound_statement) in
    Obj.repr(
# 38 "parse.mly"
                                              ( FunctionBlock(_2,(_3 @ [ReturnInst(get_node_count())])) )
# 476 "parse.ml"
               : 'function_defintion))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parse.mly"
                  ( (_1,[]) )
# 483 "parse.ml"
               : 'declarator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parse.mly"
                       ( (_1,[]) )
# 490 "parse.ml"
               : 'declarator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'parameter_list) in
    Obj.repr(
# 43 "parse.mly"
                                     ( (_1,_3) )
# 498 "parse.ml"
               : 'declarator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 44 "parse.mly"
                          ( (_1,[]) )
# 505 "parse.ml"
               : 'declarator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parameter_declaration) in
    Obj.repr(
# 47 "parse.mly"
                            ( [_1] )
# 512 "parse.ml"
               : 'parameter_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parameter_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parameter_declaration) in
    Obj.repr(
# 48 "parse.mly"
                                             ( (_1) @ [_3] )
# 520 "parse.ml"
               : 'parameter_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parse.mly"
                      ( (BasicVar _2) )
# 528 "parse.ml"
               : 'parameter_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_specifier) in
    Obj.repr(
# 52 "parse.mly"
                     ( (BasicVar "") )
# 535 "parse.ml"
               : 'parameter_declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parse.mly"
                        ( [] )
# 541 "parse.ml"
               : 'compound_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 56 "parse.mly"
                                   ( _2 )
# 548 "parse.ml"
               : 'compound_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'declarator) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'initial_value) in
    Obj.repr(
# 59 "parse.mly"
                                                     ( ExpInst(get_node_count(),AssignExp((BasicVar (fst _2)),_4)) )
# 557 "parse.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 62 "parse.mly"
                         ( [_1] )
# 564 "parse.ml"
               : 'initial_value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'initializer_list) in
    Obj.repr(
# 63 "parse.mly"
                                 ( _2 )
# 571 "parse.ml"
               : 'initial_value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 66 "parse.mly"
                              ( [_1] )
# 578 "parse.ml"
               : 'initializer_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'initializer_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 67 "parse.mly"
                                                ( (_1) @ [_3] )
# 586 "parse.ml"
               : 'initializer_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 70 "parse.mly"
                ( [_1] )
# 593 "parse.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 71 "parse.mly"
                            ( (_1) @ [_2] )
# 601 "parse.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_statement) in
    Obj.repr(
# 74 "parse.mly"
                      ( _1 )
# 608 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'selection_statement) in
    Obj.repr(
# 75 "parse.mly"
                       ( _1 )
# 615 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'iteration_statement) in
    Obj.repr(
# 76 "parse.mly"
                        ( _1 )
# 622 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 77 "parse.mly"
              ( OutputInst(get_node_count(),ConstExp(IntConst _1)) )
# 629 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "parse.mly"
            ( CrashInst (get_node_count(),_1) )
# 636 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parse.mly"
              ( InputInst (get_node_count()) )
# 642 "parse.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 82 "parse.mly"
                   ( ExpInst (get_node_count(),_1) )
# 649 "parse.ml"
               : 'expression_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    Obj.repr(
# 85 "parse.mly"
                                                   ( IfInst(get_node_count(),_3,[ErrorMsgInst(get_node_count())]) )
# 656 "parse.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 86 "parse.mly"
                                                 ( IfInst(get_node_count(),_3,[CrashInst(get_node_count(),_5)]) )
# 664 "parse.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 87 "parse.mly"
                                                                                  ( IfInst(get_node_count(),_3,_6) )
# 672 "parse.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'statement_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 88 "parse.mly"
                                                                                                ( IfElseInst(get_node_count(),_3,_6,_10) )
# 681 "parse.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'expression) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 89 "parse.mly"
                                                                                 ( IfElseInst(get_node_count(),_3,[],_9) )
# 689 "parse.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'statement_list) in
    Obj.repr(
# 90 "parse.mly"
                                                                                 ( IfElseInst(get_node_count(),_3,_6,[]) )
# 697 "parse.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 93 "parse.mly"
                                                              ( WhileInst (get_node_count(),_3,_6) )
# 705 "parse.ml"
               : 'iteration_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 96 "parse.mly"
                         ( _1 )
# 712 "parse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conditional_expression) in
    Obj.repr(
# 99 "parse.mly"
                                  ( _1 )
# 719 "parse.ml"
               : 'assignment_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 100 "parse.mly"
                                       ( AssignExp((BasicVar _1),[_3]) )
# 727 "parse.ml"
               : 'assignment_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 101 "parse.mly"
                                          ( AssignExp((BasicVar _1),[(BinOpAppExp (PlusOp,(VarExp (BasicVar _1)),_3))]) )
# 735 "parse.ml"
               : 'assignment_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 102 "parse.mly"
                                           ( AssignExp((BasicVar _1),[(BinOpAppExp (MinusOp,(VarExp (BasicVar _1)),_3))]) )
# 743 "parse.ml"
               : 'assignment_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logical_or_expression) in
    Obj.repr(
# 105 "parse.mly"
                                  ( _1 )
# 750 "parse.ml"
               : 'conditional_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'logical_or_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'conditional_expression) in
    Obj.repr(
# 106 "parse.mly"
                                                                      ( TernExp(_1,_3,_5) )
# 759 "parse.ml"
               : 'conditional_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logical_and_expression) in
    Obj.repr(
# 109 "parse.mly"
                                ( _1 )
# 766 "parse.ml"
               : 'logical_or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logical_or_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'logical_and_expression) in
    Obj.repr(
# 110 "parse.mly"
                                                         ( BinOpAppExp (OrOp,_1,_3) )
# 774 "parse.ml"
               : 'logical_or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equality_expression) in
    Obj.repr(
# 113 "parse.mly"
                              ( _1 )
# 781 "parse.ml"
               : 'logical_and_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logical_and_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'equality_expression) in
    Obj.repr(
# 114 "parse.mly"
                                                        ( BinOpAppExp (AndOp,_1,_3) )
# 789 "parse.ml"
               : 'logical_and_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 117 "parse.mly"
                             ( _1 )
# 796 "parse.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 118 "parse.mly"
                                               ( BinOpAppExp (EqOp,_1,_3) )
# 804 "parse.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 119 "parse.mly"
                                                ( BinOpAppExp (NEqOp,_1,_3) )
# 812 "parse.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'additive_expression) in
    Obj.repr(
# 122 "parse.mly"
                            ( _1 )
# 819 "parse.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_expression) in
    Obj.repr(
# 123 "parse.mly"
                                               ( BinOpAppExp (LessOp,_1,_3) )
# 827 "parse.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_expression) in
    Obj.repr(
# 124 "parse.mly"
                                               ( BinOpAppExp (GreaterOp,_1,_3) )
# 835 "parse.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_expression) in
    Obj.repr(
# 125 "parse.mly"
                                                ( BinOpAppExp (LEqOp,_1,_3) )
# 843 "parse.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_expression) in
    Obj.repr(
# 126 "parse.mly"
                                                ( BinOpAppExp (GEqOp,_1,_3) )
# 851 "parse.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 129 "parse.mly"
                                 ( _1 )
# 858 "parse.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 130 "parse.mly"
                                                     ( BinOpAppExp (PlusOp,_1,_3) )
# 866 "parse.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 131 "parse.mly"
                                                      ( BinOpAppExp (MinusOp,_1,_3) )
# 874 "parse.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 134 "parse.mly"
                          ( _1 )
# 881 "parse.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multiplicative_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 135 "parse.mly"
                                                   ( BinOpAppExp (TimesOp,_1,_3) )
# 889 "parse.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multiplicative_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 136 "parse.mly"
                                                  ( BinOpAppExp (DivOp,_1,_3) )
# 897 "parse.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multiplicative_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 137 "parse.mly"
                                                  ( BinOpAppExp (ModOp,_1,_3) )
# 905 "parse.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'postfix_expression) in
    Obj.repr(
# 140 "parse.mly"
                        ( _1 )
# 912 "parse.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unary_operator) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 141 "parse.mly"
                                   ( MonOpAppExp(_1,_2) )
# 920 "parse.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expression) in
    Obj.repr(
# 144 "parse.mly"
                           ( _1 )
# 927 "parse.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 145 "parse.mly"
                                    ( ArrAccExp((BasicVar _1),_3) )
# 935 "parse.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 146 "parse.mly"
                             ( CallExp(_1,[]) )
# 942 "parse.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'argument_expression_list) in
    Obj.repr(
# 147 "parse.mly"
                                               ( CallExp(_1,_3) )
# 950 "parse.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 150 "parse.mly"
            ( VarExp (BasicVar _1) )
# 957 "parse.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 151 "parse.mly"
                 ( VarExp (DerefVar _1) )
# 964 "parse.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 152 "parse.mly"
               ( VarExp (RefVar _1) )
# 971 "parse.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 153 "parse.mly"
            ( ConstExp (IntConst _1) )
# 978 "parse.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 154 "parse.mly"
               ( ConstExp (StringConst _1) )
# 985 "parse.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 155 "parse.mly"
                           ( _2 )
# 992 "parse.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 158 "parse.mly"
                               ( [_1] )
# 999 "parse.ml"
               : 'argument_expression_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argument_expression_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 159 "parse.mly"
                                                       ( (_1) @ [_3] )
# 1007 "parse.ml"
               : 'argument_expression_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "parse.mly"
        ( BoolNegOp )
# 1013 "parse.ml"
               : 'unary_operator))
; (fun __caml_parser_env ->
    Obj.repr(
# 164 "parse.mly"
           ( IntNegOp )
# 1019 "parse.ml"
               : 'unary_operator))
; (fun __caml_parser_env ->
    Obj.repr(
# 167 "parse.mly"
           ( )
# 1025 "parse.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 168 "parse.mly"
            ( )
# 1031 "parse.ml"
               : 'type_specifier))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry external_declaration *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Abs_syn.program)
let external_declaration (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Abs_syn.block)

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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Abs_syn.program
val external_declaration :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Abs_syn.block

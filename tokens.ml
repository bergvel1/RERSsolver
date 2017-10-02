type token =
  | INT of (int)
  | STRING of (string)
  | IDENT of (string)
  | DEREFIDENT of (string)
  | REFIDENT of (string)
  | OUTPUT of (int) 
  | INPUT
  | CRASH of (int)
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
  | WHILE
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

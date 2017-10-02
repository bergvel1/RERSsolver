%{
	open Abs_syn
%}

/* Define the tokens of the language: */
%token <int> INT OUTPUT CRASH
%token <string> STRING IDENT DEREFIDENT REFIDENT
%token PLUS MINUS TIMES DIV MOD LT GT LEQ GEQ EQ NEQ SEMI COLON TERN ASSIGN PLUSASSIGN MINUSASSIGN 
	IF ELSE NEG LOGICALAND LOGICALOR LBRAC RBRAC LPAREN RPAREN LBRACE RBRACE COMMA EOF 
	VOIDTYPE INTTYPE INPUTERROR INPUT WHILE

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%start main
%type <Abs_syn.program> main

/* using external_declaration as entry point (for now) */
%start external_declaration
%type <Abs_syn.block> external_declaration

%%

/* Adapted from Jeff Lee's 1985 Yacc grammar for C */

main:   entries EOF                     { $1 }

entries:
	external_declaration			{ [$1] }
|	entries external_declaration	{ ($1) @ [$2] }


external_declaration:
	function_defintion			{ $1 }
|	declaration 			{ GlobalInsts ([$1]) } 

function_defintion:
	type_specifier declarator compound_statement	{ FunctionBlock($2,($3 @ [ReturnInst(get_node_count())])) }

declarator:
	IDENT												{ ($1,[]) }
|	DEREFIDENT											{ ($1,[]) }
|	IDENT LPAREN parameter_list RPAREN	{ ($1,$3) }
|	IDENT LPAREN RPAREN					{ ($1,[]) }

parameter_list:
	parameter_declaration						{ [$1] }
|	parameter_list COMMA parameter_declaration	{ ($1) @ [$3] }

parameter_declaration:
	type_specifier IDENT	{ (BasicVar $2) }
| 	type_specifier 			{ (BasicVar "") } /* remove this */

compound_statement:
	LBRACE RBRACE 									{ [] }
|	LBRACE statement_list RBRACE					{ $2 }

declaration:
	type_specifier declarator ASSIGN initial_value SEMI	{ ExpInst(get_node_count(),AssignExp((BasicVar (fst $2)),$4)) }

initial_value:
	assignment_expression			{ [$1] }
|	LBRACE initializer_list RBRACE	{ $2 }

initializer_list:
	assignment_expression 							{ [$1] }
|	initializer_list COMMA assignment_expression 	{ ($1) @ [$3] }

statement_list:
	statement 					{ [$1] }
|	statement_list statement 	{ ($1) @ [$2] }

statement:
	expression_statement	{ $1 }
|	selection_statement		{ $1 }
| 	iteration_statement 	{ $1 }
|	OUTPUT 					{ OutputInst(get_node_count(),ConstExp(IntConst $1)) }
|	CRASH					{ CrashInst (get_node_count(),$1) }
| 	INPUT 					{ InputInst (get_node_count()) }

expression_statement:
	expression SEMI			{ ExpInst (get_node_count(),$1) }

selection_statement:
	IF LPAREN expression RPAREN INPUTERROR												{ IfInst(get_node_count(),$3,[ErrorMsgInst(get_node_count())]) }
|	IF LPAREN expression RPAREN CRASH 													{ IfInst(get_node_count(),$3,[CrashInst(get_node_count(),$5)]) }
|	IF LPAREN expression RPAREN LBRACE statement_list RBRACE %prec LOWER_THAN_ELSE		{ IfInst(get_node_count(),$3,$6) }
|	IF LPAREN expression RPAREN LBRACE statement_list RBRACE ELSE LBRACE statement_list RBRACE 			{ IfElseInst(get_node_count(),$3,$6,$10) }
|	IF LPAREN expression RPAREN LBRACE RBRACE ELSE LBRACE statement_list RBRACE 			{ IfElseInst(get_node_count(),$3,[],$9) }
|	IF LPAREN expression RPAREN LBRACE statement_list RBRACE ELSE LBRACE RBRACE 			{ IfElseInst(get_node_count(),$3,$6,[]) }

iteration_statement:
	WHILE LPAREN expression RPAREN LBRACE statement_list RBRACE		{ WhileInst (get_node_count(),$3,$6) }

expression:
	assignment_expression			{ $1 }

assignment_expression:
	conditional_expression											{ $1 }
|	IDENT ASSIGN assignment_expression			{ AssignExp((BasicVar $1),[$3]) }
|	IDENT PLUSASSIGN assignment_expression		{ AssignExp((BasicVar $1),[(BinOpAppExp (PlusOp,(VarExp (BasicVar $1)),$3))]) }
|	IDENT MINUSASSIGN assignment_expression		{ AssignExp((BasicVar $1),[(BinOpAppExp (MinusOp,(VarExp (BasicVar $1)),$3))]) }

conditional_expression:
	logical_or_expression												{ $1 }
|	logical_or_expression TERN expression COLON conditional_expression 	{ TernExp($1,$3,$5) }

logical_or_expression:
	logical_and_expression									{ $1 }
|	logical_or_expression LOGICALOR logical_and_expression	{ BinOpAppExp (OrOp,$1,$3) }

logical_and_expression:		
	equality_expression										{ $1 }
|	logical_and_expression LOGICALAND equality_expression	{ BinOpAppExp (AndOp,$1,$3) }

equality_expression:
	relational_expression							{ $1 }
|	equality_expression EQ relational_expression	{ BinOpAppExp (EqOp,$1,$3) }
|	equality_expression NEQ relational_expression	{ BinOpAppExp (NEqOp,$1,$3) }

relational_expression:
	additive_expression								{ $1 }
|	relational_expression LT additive_expression	{ BinOpAppExp (LessOp,$1,$3) }
|	relational_expression GT additive_expression	{ BinOpAppExp (GreaterOp,$1,$3) }
|	relational_expression LEQ additive_expression	{ BinOpAppExp (LEqOp,$1,$3) }
|	relational_expression GEQ additive_expression	{ BinOpAppExp (GEqOp,$1,$3) }

additive_expression:
	multiplicative_expression							{ $1 }
|	additive_expression PLUS multiplicative_expression	{ BinOpAppExp (PlusOp,$1,$3) }
|	additive_expression	MINUS multiplicative_expression	{ BinOpAppExp (MinusOp,$1,$3) }

multiplicative_expression:
	unary_expression									{ $1 }	
|	multiplicative_expression TIMES unary_expression	{ BinOpAppExp (TimesOp,$1,$3) }
|	multiplicative_expression DIV unary_expression		{ BinOpAppExp (DivOp,$1,$3) }
|	multiplicative_expression MOD unary_expression		{ BinOpAppExp (ModOp,$1,$3) }

unary_expression:
	postfix_expression					{ $1 }
|	unary_operator unary_expression		{ MonOpAppExp($1,$2) }
	
postfix_expression:
	primary_expression								{ $1 }
|	IDENT LBRAC expression RBRAC 					{ ArrAccExp((BasicVar $1),$3) }
|	IDENT LPAREN RPAREN 							{ CallExp($1,[]) }
|	IDENT LPAREN argument_expression_list RPAREN	{ CallExp($1,$3) }

primary_expression:
	IDENT						{ VarExp (BasicVar $1) }
|	DEREFIDENT					{ VarExp (DerefVar $1) }
|	REFIDENT					{ VarExp (RefVar $1) }
|	INT 						{ ConstExp (IntConst $1) }
|	STRING 						{ ConstExp (StringConst $1) }
|	LPAREN expression RPAREN	{ $2 }

argument_expression_list:
	assignment_expression									{ [$1] }
|	argument_expression_list COMMA assignment_expression	{ ($1) @ [$3] }

/* operators and other basic things */
unary_operator:
	NEG 			{ BoolNegOp }
|	MINUS 			{ IntNegOp }

type_specifier:
	INTTYPE			{ }
|	VOIDTYPE		{ }

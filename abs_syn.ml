(* abstract syntax for C used in RERS problems *)

type var = 
	BasicVar of string
| 	DerefVar of string
| 	RefVar of string 

let string_of_var = function
	BasicVar v -> v
| 	DerefVar v -> "*" ^ v
| 	RefVar v -> "&" ^ v

type const = 
	IntConst of int
| 	StringConst of string
| 	BoolConst of bool 

let string_of_const = function
    IntConst i -> string_of_int i
|  	StringConst s -> s 
| 	BoolConst b -> string_of_bool b

type mon_op =
	BoolNegOp	(* ! _ *)
|  	IntNegOp 	(* - _ *) 

let string_of_mon_op = function
	BoolNegOp -> "!"
| 	IntNegOp -> "-" 

type bin_op =
	PlusOp		(* _ + _ *)
|   MinusOp		(* _ - _ *)
|  	TimesOp		(* _ * _ *)
| 	ModOp		(* _ % _ *) 
| 	DivOp		(* _ / _ *) (* Integer division, expected to yield an integer *)
| 	EqOp 		(* _ == _ *)
| 	NEqOp		(* _ != _ *) 
|  	GreaterOp	(* _ > _ *) 
|  	LessOp		(* _ < _ *)
|  	GEqOp		(* _ >= _ *)
|  	LEqOp		(* _ <= _ *)
| 	AndOp		(* _ && _ *)   
| 	OrOp		(* _ || _ *)

let string_of_bin_op = function
	PlusOp -> "+"
| 	MinusOp -> "-"
| 	TimesOp ->  "*"
| 	DivOp -> "/"
| 	ModOp -> "%"
| 	EqOp -> "=="
| 	NEqOp -> "!="
| 	GreaterOp -> ">"
| 	LessOp -> "<"
| 	GEqOp -> ">="
| 	LEqOp -> "<="
| 	AndOp -> "&&"
| 	OrOp -> "||" 

type exp =
	VarExp of var 						(* variables *)
|  	ConstExp of const 					(* constants *)
|   MonOpAppExp of mon_op * exp 		(* % exp1, where % is a mon_op *)
|  	BinOpAppExp of bin_op * exp * exp 	(* exp1 % exp2, where % is a bin_op *)
|  	TernExp of exp * exp * exp 			(* exp1 ? exp2 : exp 3 *)
| 	ArrAccExp of var * exp 				(* var[exp] *)
| 	CallExp of string * (exp list)		(* fun(ex1,exp2,...) *)
| 	AssignExp of var * (exp list)

let rec string_of_exp = function
	VarExp v -> string_of_var v
| 	ConstExp c -> string_of_const c
| 	MonOpAppExp (m,e) -> (string_of_mon_op m) ^ " " ^ (string_of_exp e)
|       BinOpAppExp (b,e1,e2) -> (string_of_exp e1) ^ " " ^ (string_of_bin_op b) ^ " " ^ (string_of_exp e2)
| 	TernExp (c,e1,e2) -> "(" ^ (string_of_exp c) ^ ") ? (" ^ (string_of_exp e1) ^ ") : (" ^ (string_of_exp e2) ^ ")"
|  	ArrAccExp (v,e) -> (string_of_var v) ^ "[" ^ (string_of_exp e) ^ "]"
| 	CallExp (f,args) -> f ^ "(" ^ (String.concat "," (List.map string_of_exp args)) ^ ")"
| 	AssignExp (v,exps) -> (match exps with
			[] -> "ERROR: empty assignment to " ^ (string_of_var v)
		|   (hd :: []) -> (string_of_var v) ^ " = " ^ (string_of_exp hd) ^ ";"
		| 	_ -> (string_of_var v) ^ " = {" ^ (String.concat "," (List.map string_of_exp exps)) ^ "};")

type inst =
	OutputInst of int * exp 							(* Evaluates an expression and prints the result *)
|  	InputInst of int 
|   ErrorMsgInst of int								(* Prints error message upon receiving invalid input *)
|   CrashInst of int * int 							(* __VERIFIER_error(int) *)
|  	IfInst of int * exp * inst list 					(* if exp true, then do instruction block *)
|   IfElseInst of int * exp * inst list * inst list	(* choose which instruction block to do based on evaluation of exp *)
| 	WhileInst of int * exp * inst list 				(* Loops through instructions while exp is true *)
| 	ExpInst of int * exp
| 	ReturnInst of int 								(* Used by the parser to mark the ends of functon definitions *)

let rec string_of_inst = function
   	OutputInst (n,e) -> "printf(\"%d\\\n\", " ^ (string_of_exp e) ^ "); fflush(stdout);"
| 	InputInst n -> "scanf(\"%d\", &input);"
|   ErrorMsgInst n -> "fprintf(stderr, \"Invalid input: %d\\n\", " ^ "input" ^ ");"
|  	CrashInst (n,i) -> "__VERIFIER_error(" ^ (string_of_int i) ^ ");"
|   IfInst (n,e,l) -> "if(" ^ (string_of_exp e) ^ ") {\n" ^ (String.concat "\n" (List.map string_of_inst l)) ^ "}"
|   IfElseInst (n,e,l1,l2) -> "if(" ^ (string_of_exp e) ^ ") {\n" ^ (String.concat "\n" (List.map string_of_inst l1)) ^ "} else {\n" ^ (String.concat "\n" (List.map string_of_inst l2)) ^ "}"
| 	WhileInst (n,e,l) -> "while(" ^ (string_of_exp e) ^ ") {\n" ^ (String.concat "\n" (List.map string_of_inst l)) ^ "}"
|  	ExpInst (n,e) -> (string_of_exp e) ^ ";"
| 	ReturnInst n -> "return;"

type block = 
    GlobalInsts of inst list
  (* Instructions not contained within a function, e.g. global assignments *)
  | FunctionBlock of (string * (var list)) * inst list
  (* A function block described by the function (*output type,*) name, input arguments, and instructions *)

let string_of_block = function
	GlobalInsts (l) -> (String.concat "\n" (List.map string_of_inst l))
|   FunctionBlock ((f,params),l) -> f ^ "(" ^ (String.concat "," (List.map string_of_var params)) ^ ") {\n" ^ (String.concat "\n" (List.map string_of_inst l)) ^ "}"

type program = block list

let (get_node_count, see_node_count, reset_node_count) =
  let count = ref 1 in
  ((fun () -> (let c = !count in (count := c + 1; c))),
   (fun () -> (!count)),
   (fun () -> count := 1))

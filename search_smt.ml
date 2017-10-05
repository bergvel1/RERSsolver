open Aez
open Smt
open Eval_exp
module T = Term
module F = Formula
module Solver = Make (struct end)
module Varmap = Map.Make(String)

type task = 
	{	task_num:int;
		curr_node_idx:int;
		depth:int;
		input_count:int; (* possibly redundant with depth *)
		formula_count:int;
		solver_state:Solver.state;
		var_env:int Varmap.t
	}

let (get_task_count, see_task_count, reset_task_count) =
  let count = ref 1 in
  ((fun () -> (let c = !count in (count := c + 1; c))),
   (fun () -> (!count)),
   (fun () -> count := 1))

let (get_formula_count, see_formula_count, reset_formula_count) =
  let count = ref 1 in
  ((fun () -> (let c = !count in (count := c + 1; c))),
   (fun () -> (!count)),
   (fun () -> count := 1))

let rec term_of_exp e =
(match e with
  VarExp v -> let v' = (Hstring.make (string_of_var v)) in (if not (Symbol.declared v') then Symbol.declare v' [] Type.type_int); T.make_app v' []
| ConstExp (IntConst i) -> T.make_int (Num.Int i)
| ConstExp (BoolConst true) -> T.t_true
| ConstExp (BoolConst false) -> T.t_false
| MonOpAppExp (IntNegOp,x) -> T.make_arith T.Minus (T.make_int (Num.Int 0)) (term_of_exp x)
| BinOpAppExp (PlusOp,x,y) -> T.make_arith T.Plus (term_of_exp x) (term_of_exp y)
| BinOpAppExp (MinusOp,x,y) -> T.make_arith T.Minus (term_of_exp x) (term_of_exp y)
| BinOpAppExp (TimesOp,x,y) -> T.make_arith T.Mult (term_of_exp x) (term_of_exp y)
| BinOpAppExp (DivOp,x,y) -> T.make_arith T.Div (term_of_exp x) (term_of_exp y)
| TernExp (p,x,y) -> T.make_ite (formula_of_pred (Exp_pred p)) (term_of_exp x) (term_of_exp y)
| _ -> failwith "term_of_exp"
)

and formula_of_pred p =
(match p with
  True -> F.f_true
| False -> F.f_false
| And (x,y) -> F.make F.And [(formula_of_pred x);(formula_of_pred y)]
| Or (x,y) -> F.make F.Or [(formula_of_pred x);(formula_of_pred y)]
| Implies (x,y) -> F.make F.Imp [(formula_of_pred x);(formula_of_pred y)]
| Not x -> F.make F.Not [(formula_of_pred x)]
| Exp_pred (ConstExp (BoolConst true)) -> F.f_true
| Exp_pred (ConstExp (BoolConst false)) -> F.f_false
| Exp_pred (BinOpAppExp(GreaterOp,x,y)) -> F.make_lit F.Lt [(term_of_exp y);(term_of_exp x)]
| Exp_pred (BinOpAppExp(LEqOp,x,y)) -> F.make_lit F.Le [(term_of_exp x);(term_of_exp y)]
| Exp_pred (BinOpAppExp(LessOp,x,y)) -> F.make_lit F.Lt [(term_of_exp x);(term_of_exp y)]
| Exp_pred (BinOpAppExp(GEqOp,x,y)) -> F.make_lit F.Le [(term_of_exp y);(term_of_exp x)]
| Exp_pred (BinOpAppExp(EqOp,x,y)) -> F.make_lit F.Eq [(term_of_exp x);(term_of_exp y)]
| Exp_pred (BinOpAppExp(NEqOp,x,y)) -> F.make_lit F.Neq [(term_of_exp x);(term_of_exp y)]
| Exp_pred (BinOpAppExp(AndOp,x,y)) -> F.make F.And [(formula_of_pred (Exp_pred x));(formula_of_pred (Exp_pred y))]
| Exp_pred (BinOpAppExp(OrOp,x,y)) -> F.make F.Or [(formula_of_pred (Exp_pred x));(formula_of_pred (Exp_pred y))]
| Exp_pred (MonOpAppExp(BoolNegOp,x)) -> F.make F.Not [(formula_of_pred (Exp_pred x))]
| _ -> failwith "formula_of_pred"
)

let rec exp_subst subvar subexps exp = 
(match subexps with
	  [] -> failwith "empty substitution"
	| subexp::[] -> (match exp with
		  VarExp v -> if (String.equal (string_of_var v) (string_of_var subvar)) then subexp else (VarExp v)
		| ConstExp c -> ConstExp c
		| MonOpAppExp (m,e) -> MonOpAppExp (m,(exp_subst subvar subexps e))
		| BinOpAppExp (b,e1,e2) -> BinOpAppExp (b,(exp_subst subvar subexps e1),(exp_subst subvar subexps e2))
		| TernExp (c,e1,e2) -> TernExp ((exp_subst subvar subexps c),(exp_subst subvar subexps e1),(exp_subst subvar subexps e2))
		| ArrAccExp (v,e) -> failwith "not implemented yet"
		| CallExp (f,args) -> failwith "error?"
		| AssignExp (v,exps) -> failwith "error?")
	| _ -> failwith "arrays not supported yet")

let rec pred_subst subvar subexps p = 
(match subexps with
	  [] -> failwith "empty substitution"
	| subexp::[] -> (match p with
		  True -> True
		| False -> False
		| And (p1,p2) -> And ((pred_subst subvar [subexp] p1),(pred_subst subvar [subexp] p2))
		| Or (p1,p2) -> Or ((pred_subst subvar [subexp] p1),(pred_subst subvar [subexp] p2))
		| Implies (p1,p2) -> Implies ((pred_subst subvar [subexp] p1),(pred_subst subvar [subexp] p2))
		| Not p1 -> Not (pred_subst subvar [subexp] p1)
		| Exp_pred e -> Exp_pred (exp_subst subvar [subexp] e))
	| _ -> p) (*failwith "arrays not supported yet")*)

let try_pred p t n = 
let n' = get_formula_count() in
print_string ("Task " ^ (string_of_int t) ^ " Formula " ^ (string_of_int n') ^ ": " ^ (string_of_pred p) ^ "\n");
try
	let f = (formula_of_pred p) in
	Solver.assume ~id:n' f;
	Solver.check();
	[]
with Unsat l -> Solver.clear();l

(* get fresh name for v and update var_env *)
let phi var_env e =
(match e with
  VarExp v ->
	  (let vstr = (string_of_var v) in
	  let count = (try (Varmap.find vstr var_env) with Not_found -> 0)+1 in
	  let var_env' = (Varmap.add vstr count var_env) in
	  (*let count = (try (List.assoc vstr var_env) with Not_found -> 0)+1 in
	  let var_env' = (vstr,count)::(List.remove_assoc vstr var_env) in*)
	  let app = ("\'" ^ (string_of_int count)) in
	  let vstr' = String.concat "" [vstr;app] in (var_env',VarExp (BasicVar vstr')))
| _ -> failwith "Bad input (phi)")

(* replace variables in e with their proper phi versions *)
let rec phi_subst_exp var_env exp = (match exp with 
  VarExp v -> let vstr = (string_of_var v) in 
  			  if (Varmap.mem vstr var_env)
  			    then (let app = ("\'" ^ (string_of_int (Varmap.find vstr var_env))) in
  			    	  let vstr' = String.concat "" [vstr;app] in
  			    	    (var_env,VarExp (BasicVar vstr')))
  				else (let var_env' = (Varmap.add vstr 1 var_env) in
  					  let app = "\'1" in
  			    	  let vstr' = String.concat "" [vstr;app] in
  			    	    (var_env',VarExp (BasicVar vstr')))
| ConstExp c -> (var_env,ConstExp c)
| MonOpAppExp (m,e) -> let (var_env',e') = (phi_subst_exp var_env e) in 
						 (var_env',MonOpAppExp (m,e'))
| BinOpAppExp (m,e1,e2) -> let (var_env',e1') = (phi_subst_exp var_env e1) in
						   let (var_env'',e2') = (phi_subst_exp var_env' e2) in
						   	 (var_env'',BinOpAppExp (m,e1',e2'))
| TernExp (c,e1,e2) -> let (var_env',c') = (phi_subst_exp var_env c) in
					   let (var_env'',e1') = (phi_subst_exp var_env' e1) in
					   let (var_env''',e2') = (phi_subst_exp var_env'' e2) in
					   	 (var_env''',TernExp (c',e1',e2'))
| _ -> failwith ("unexpected input (phi_subst_exp)" ^ (string_of_exp exp)))

let rec queued_smt_reverse_search graph input_alph task_queue =
if (Queue.is_empty task_queue) then (print_string "no more tasks\n"; []) else
	(let curr_task = Queue.pop task_queue in
	(*print_string ("Now working on " ^ (string_of_task curr_task));*)
	if (curr_task.depth > 0) then
		(let curr_node = (Array.get graph curr_task.curr_node_idx) in
		(match (get_in_nodes curr_node) with
		  [] -> Solver.clear(); Solver.restore_state(curr_task.solver_state);
		  		(try 
		  			(Solver.check(); 
		  			print_string ("\tTask " ^ (string_of_int curr_task.task_num) ^ " sat!\n"))
		  		with Unsat l -> print_string ("\tT@sk " ^ (string_of_int curr_task.task_num) ^ " unsat by: " ^ (String.concat "," (List.map string_of_int l)) ^ "\n")); queued_smt_reverse_search graph input_alph task_queue
		| in_edges -> let _ = List.map (fun e -> (match e with
			  (n,Seq) -> (match (Array.get graph n) with
			  	  Assign_node {var;asg_value;node_num;out_node;in_nodes} -> 
			  	     Solver.clear(); Solver.restore_state(curr_task.solver_state);
			  	     let lhs = exp_subst (BasicVar "input") [VarExp (BasicVar ("input"^(string_of_int curr_task.input_count)))] (VarExp var) in
			  	     let rhs = exp_subst (BasicVar "input") [VarExp (BasicVar ("input"^(string_of_int curr_task.input_count)))] (List.hd asg_value) in
			  	     let (var_env',_) = phi curr_task.var_env lhs in (* this could be done better *)
			  	     let (var_env'',rhs') = phi_subst_exp var_env' rhs in
			  	     let lhs' = snd (phi_subst_exp curr_task.var_env lhs) in
			  	     let p = Exp_pred (BinOpAppExp (EqOp,lhs',rhs')) in
				  	 let t = if (e = (List.hd in_edges)) then (curr_task.task_num) else (get_task_count()) in
				  	 let l = try_pred p t curr_task.formula_count in
				  	 if (l = []) (*change to match*)then (Queue.push 
					{task_num = t; curr_node_idx = n; depth = curr_task.depth;
					 input_count = curr_task.input_count; formula_count = curr_task.formula_count+1;
					 solver_state = Solver.save_state(); var_env = var_env''} task_queue;)
				  	else print_string ("\tTask " ^ (string_of_int t) ^ " unsat by: " ^ (String.concat "," (List.map string_of_int l)) ^ "\n")
				| Input_node _ -> Queue.push
					{task_num = if (e = (List.hd in_edges)) then (curr_task.task_num) else (get_task_count());
					 curr_node_idx = n; depth = curr_task.depth-1;
					 input_count = curr_task.input_count+1; formula_count = curr_task.formula_count;
					 solver_state = curr_task.solver_state; var_env = curr_task.var_env} task_queue;
				| _ -> Queue.push 
					{task_num = if (e = (List.hd in_edges)) then (curr_task.task_num) else (get_task_count());
					 curr_node_idx = n; depth = curr_task.depth;
					 input_count = curr_task.input_count; formula_count = curr_task.formula_count;
					 solver_state = curr_task.solver_state; var_env = curr_task.var_env} task_queue;)
			| (n,Yes) -> (match (Array.get graph n) with
				  Cond_node {cond;node_num;then_node;else_node;in_nodes} -> 
				     Solver.clear(); Solver.restore_state(curr_task.solver_state);
				     let cond' = exp_subst (BasicVar "input") [VarExp (BasicVar ("input"^(string_of_int curr_task.input_count)))] cond in
			  	     let (var_env',p) = (let phi_subst = (phi_subst_exp curr_task.var_env cond') in (fst phi_subst, Exp_pred (snd phi_subst))) in
				  	 let t = if (e = (List.hd in_edges)) then (curr_task.task_num) else (get_task_count()) in
				  	 let l = try_pred p t curr_task.formula_count in
				  	 if l = [] then (Queue.push 
					{task_num = t; curr_node_idx = n;  depth = curr_task.depth;
					 input_count = curr_task.input_count; formula_count = curr_task.formula_count+1;
					 solver_state = Solver.save_state(); var_env = var_env'} task_queue;)
				  	else print_string ("\tTask " ^ (string_of_int t) ^ " unsat by: " ^ (String.concat "," (List.map string_of_int l)) ^ "\n")
				| _ -> failwith "malformed graph")
			| (n,No) -> (match (Array.get graph n) with
				  Cond_node {cond;node_num;then_node;else_node;in_nodes} -> 
				     Solver.clear(); Solver.restore_state(curr_task.solver_state);
				     let cond' = exp_subst (BasicVar "input") [VarExp (BasicVar ("input"^(string_of_int curr_task.input_count)))] cond in
				     let (var_env',p) = (let phi_subst = (phi_subst_exp curr_task.var_env cond') in (fst phi_subst, Not (Exp_pred (snd phi_subst)))) in
				  	 let t = if (e = (List.hd in_edges)) then (curr_task.task_num) else (get_task_count()) in
				  	 let l = try_pred p t curr_task.formula_count in
				  	 if l = [] then (Queue.push 
					{task_num = t; curr_node_idx = n;  depth = curr_task.depth;
					 input_count = curr_task.input_count; formula_count = curr_task.formula_count+1;
					 solver_state = Solver.save_state(); var_env = var_env'} task_queue;)
				  	else print_string ("\tTask " ^ (string_of_int t) ^ " unsat by: " ^ (String.concat "," (List.map string_of_int l)) ^ "\n")
				| _ -> failwith "malformed graph")
			| (n,_) -> Queue.push
				  	  	{task_num = if (e = (List.hd in_edges)) then (curr_task.task_num) else (get_task_count());
				  	  	 curr_node_idx = n; depth = curr_task.depth; input_count = curr_task.input_count;
				  	  	 formula_count = curr_task.formula_count; solver_state = curr_task.solver_state; var_env = curr_task.var_env} 
				  	  	 task_queue;)) in_edges in queued_smt_reverse_search graph input_alph task_queue))
	else (print_string ("Depth limit reached  " ^ (string_of_int curr_task.task_num) ^ "\n");
		 queued_smt_reverse_search graph input_alph task_queue)) 

let init_queued_smt_search graph input_alph depth start_node_idx =
	Solver.clear();
	reset_task_count(); 
	reset_formula_count();
	let task_queue = Queue.create() in
	Queue.push {task_num = get_task_count(); 
		curr_node_idx = start_node_idx; depth = depth; input_count = 0; formula_count = 1; 
		solver_state = Solver.save_state(); var_env = (Varmap.empty)} task_queue;
	queued_smt_reverse_search graph input_alph task_queue
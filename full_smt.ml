(* An attempt at using an SMT solver as a simplification engine *)
(* Currently limited to using Yices with a depth-first search *)

open Yices
open Abs_syn
open Eval_exp
open Graph

module Varmap = Map.Make(String)

let glbl_context = ref (mk_context())

type task = 
	{	task_num:int;
		curr_node_idx:int;
		depth:int;
		input_count:int; (* possibly redundant with depth *)
		placeholder_pred:pred;
		rollback:int;
		var_env:int Varmap.t
	}

(* Global counters *)
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

let (get_quot_count, see_quot_count, reset_quot_count) =
  let count = ref 1 in
  ((fun () -> (let c = !count in (count := c + 1; c))),
   (fun () -> (!count)),
   (fun () -> count := 1))

let get_quot_name = (fun () -> ("quot" ^ (string_of_int (get_quot_count()))))

let (get_rem_count, see_rem_count, reset_rem_count) =
  let count = ref 1 in
  ((fun () -> (let c = !count in (count := c + 1; c))),
   (fun () -> (!count)),
   (fun () -> count := 1))

let get_rem_name = (fun () -> ("rem" ^ (string_of_int (get_rem_count()))))

let (get_div_count, see_div_count, reset_div_count) =
  let count = ref 1 in
  ((fun () -> (let c = !count in (count := c + 1; c))),
   (fun () -> (!count)),
   (fun () -> count := 1))

let get_div_name = (fun () -> ("div" ^ (string_of_int (get_div_count()))))

let (get_mod_count, see_mod_count, reset_mod_count) =
  let count = ref 1 in
  ((fun () -> (let c = !count in (count := c + 1; c))),
   (fun () -> (!count)),
   (fun () -> count := 1))

let get_mod_name = (fun () -> ("mod" ^ (string_of_int (get_mod_count()))))

let (get_unnamed_count, see_unnamed_count, reset_unnamed_count) =
  let count = ref 1 in
  ((fun () -> (let c = !count in (count := c + 1; c))),
   (fun () -> (!count)),
   (fun () -> count := 1))

(* ...not the best thing to call this function *)
let get_unnamed_name = (fun () -> ("unnamed" ^ (string_of_int (get_unnamed_count()))))

let rec term_of_exp e =
		(match e with
		  VarExp v -> let v_decl = try 
		  							(get_var_decl_from_name !glbl_context (string_of_var v)) 
		  				with Failure _ -> (mk_var_decl !glbl_context (string_of_var v) (mk_type !glbl_context "int")) in
		  					mk_var_from_decl !glbl_context v_decl
		| ConstExp (IntConst i) -> mk_num !glbl_context i
		| ConstExp (BoolConst true) -> mk_true !glbl_context
		| ConstExp (BoolConst false) -> mk_false !glbl_context
		| MonOpAppExp (IntNegOp,x) -> mk_sub !glbl_context (Array.of_list [(mk_num !glbl_context 0);(term_of_exp x)])
		| BinOpAppExp (PlusOp,x,y) -> mk_sum !glbl_context (Array.of_list [(term_of_exp x);(term_of_exp y)])
		| BinOpAppExp (MinusOp,x,y) -> mk_sub !glbl_context (Array.of_list [(term_of_exp x);(term_of_exp y)])
		| BinOpAppExp (TimesOp,x,y) -> mk_mul !glbl_context (Array.of_list [(term_of_exp x);(term_of_exp y)])
		| TernExp (p,x,y) -> mk_ite !glbl_context (formula_of_pred (Exp_pred p)) (term_of_exp x) (term_of_exp y)
			(* "special" operations (i.e. not natively supported by OCamlyices *)
		| BinOpAppExp (DivOp,i,(ConstExp (IntConst j))) ->
			let ctx = !glbl_context in 
			let quot_term = term_of_exp (VarExp (BasicVar (get_quot_name()))) in
			let rem_term = term_of_exp (VarExp (BasicVar (get_rem_name()))) in
			let i_term = term_of_exp i in
			let j_term = mk_num ctx j in
			let zero_term = mk_num ctx 0 in
				(* ((q * j) + r = i) && (((i >= 0) && (r >= 0)) || ((i <= 0) && (r <= 0))) *)
			let exp = mk_and ctx (Array.of_list [(mk_eq ctx (mk_sum ctx (Array.of_list [(mk_mul ctx (Array.of_list [quot_term;j_term]));rem_term])) i_term);(mk_or ctx (Array.of_list [(mk_and ctx (Array.of_list [(mk_ge ctx i_term zero_term);(mk_ge ctx rem_term zero_term)]));(mk_and ctx (Array.of_list [(mk_le ctx i_term zero_term);(mk_le ctx rem_term zero_term)]))]))]) in
			(*pp_expr exp;*) exp
		| BinOpAppExp (ModOp,i,(ConstExp (IntConst j))) -> 
			let ctx = !glbl_context in
			let div_term = term_of_exp (VarExp (BasicVar (get_div_name()))) in
			let mod_term = term_of_exp (VarExp (BasicVar (get_mod_name()))) in
			let i_term = term_of_exp i in
			let j_term = mk_num ctx j in
			let zero_term = mk_num ctx 0 in
				(* ((d * j) + m = i) && (((j >= 0) && (m >= 0)) || ((j <= 0) && (m <= 0))) *)
			let exp = mk_and ctx (Array.of_list [(mk_eq ctx (mk_sum ctx (Array.of_list [(mk_mul ctx (Array.of_list [div_term;j_term]));mod_term])) i_term);(mk_or ctx (Array.of_list [(mk_and ctx (Array.of_list [(mk_ge ctx j_term zero_term);(mk_ge ctx mod_term zero_term)]));(mk_and ctx (Array.of_list [(mk_le ctx j_term zero_term);(mk_le ctx mod_term zero_term)]))]))]) in
			(*pp_expr exp;*) exp
		| BinOpAppExp (DivOp,i,j) -> failwith "term_of_exp error (nonlinear division operation)"
		| BinOpAppExp (ModOp,i,j) -> failwith "term_of_exp error (nonlinear modulo operation)"
		| _ -> failwith "term_of_exp error (other)"
		)

		and formula_of_pred p =
		(match p with
		  True -> mk_true !glbl_context
		| False -> mk_false !glbl_context
		| And (x,y) -> mk_and !glbl_context (Array.of_list [(formula_of_pred x);(formula_of_pred y)])
		| Or (x,y) -> mk_or !glbl_context (Array.of_list [(formula_of_pred x);(formula_of_pred y)])
		(*| Implies (x,y) -> F.make F.Imp [(formula_of_pred x);(formula_of_pred y)]*)
		| Not x -> mk_not !glbl_context (formula_of_pred x)
		| Exp_pred (ConstExp (BoolConst true)) -> mk_true !glbl_context
		| Exp_pred (ConstExp (BoolConst false)) -> mk_false !glbl_context
		| Exp_pred (BinOpAppExp(GreaterOp,x,y)) -> mk_gt !glbl_context (term_of_exp y) (term_of_exp x)
		| Exp_pred (BinOpAppExp(LEqOp,x,y)) -> mk_le !glbl_context (term_of_exp x) (term_of_exp y)
		| Exp_pred (BinOpAppExp(LessOp,x,y)) -> mk_lt !glbl_context (term_of_exp x) (term_of_exp y)
		| Exp_pred (BinOpAppExp(GEqOp,x,y)) -> mk_ge !glbl_context (term_of_exp y) (term_of_exp x)
			(* a special case of the modulo operation that is easily solved by Yices *)
		| Exp_pred (BinOpAppExp(EqOp,BinOpAppExp (ModOp,x,ConstExp y ),ConstExp (IntConst 0))) -> 
			mk_eq !glbl_context (term_of_exp x) (term_of_exp (BinOpAppExp (TimesOp,ConstExp y, VarExp (BasicVar (get_unnamed_name())))))
		| Exp_pred (BinOpAppExp(EqOp,x,y)) -> mk_eq !glbl_context (term_of_exp x) (term_of_exp y)
		| Exp_pred (BinOpAppExp(NEqOp,x,y)) -> mk_diseq !glbl_context (term_of_exp x) (term_of_exp y)
		| Exp_pred (BinOpAppExp(AndOp,x,y)) -> mk_and !glbl_context (Array.of_list [(formula_of_pred (Exp_pred x));(formula_of_pred (Exp_pred y))])
		| Exp_pred (BinOpAppExp(OrOp,x,y)) -> mk_or !glbl_context (Array.of_list [(formula_of_pred (Exp_pred x));(formula_of_pred (Exp_pred y))])
		| Exp_pred (MonOpAppExp(BoolNegOp,x)) -> mk_not !glbl_context (formula_of_pred (Exp_pred x))
		| Exp_pred (BinOpAppExp(DivOp,i,j)) -> term_of_exp (BinOpAppExp(DivOp,i,j))
		| Exp_pred (BinOpAppExp (ModOp,i,j)) -> term_of_exp (BinOpAppExp (ModOp,i,j))
		| _ -> failwith "formula_of_pred error"
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

let try_pred pred_exp t = 
let n = get_formula_count() in
(*print_string ("\tTask " ^ (string_of_int t) ^ " Formula " ^ (string_of_int n) ^ ": " ^ (string_of_pred pred_exp) ^ "\n"); flush_all();*)
let f = (formula_of_pred pred_exp) in
	assert_simple !glbl_context f;
	(inconsistent !glbl_context)

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

(* find the current name for a variable (without incrementing its count in var_env) *)
let get_phi_name var_env vstr =
  let count = (try (Varmap.find vstr var_env) with Not_found -> failwith "Bad lookup (get_phi_name)") in
  let app = ("\'" ^ (string_of_int count)) in
  String.concat "" [vstr;app]

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

let revert_solver_state curr_rollback_count task_stack =
if (not (Stack.is_empty task_stack)) 
	then 
		(for i = (Stack.top task_stack).rollback to (curr_rollback_count-1) do 
				(pop !glbl_context)
		done)

let print_inputs var_env len = 
	let model = (get_model !glbl_context) in
	for i = 0 to (len-1) do 
		let input_name = ("input" ^ (string_of_int i)) in
		let input_phi = get_phi_name var_env input_name in
		let input_decl = get_var_decl_from_name !glbl_context input_phi in
		let input_val = get_int_value model input_decl in
			print_string ("\t" ^ input_name ^ " = " ^ 
				(String.make 1 (char_of_int (64+(Int32.to_int input_val)))) ^ "\n") (* char_of_int 65 = 'A' *)
	done

let rec smt_reverse_search graph task_stack =
if (Stack.is_empty task_stack) then (print_string "unreachable\n"(*"no more tasks\n"*); flush stdout; []) else
	(let curr_task = Stack.pop task_stack in
	if (curr_task.depth > -1) then
		(match curr_task.placeholder_pred with
			  True -> (*print_string ("Now working on task " ^ (string_of_int curr_task.task_num) ^ " at node " ^ (string_of_int curr_task.curr_node_idx) ^ "\n");*)
			  	(let curr_node = (Array.get graph curr_task.curr_node_idx) in
				(match (get_in_nodes curr_node) with
				  [] -> (*print_string "\t\t Found valid path to start! \n";*)
				  		(match (check !glbl_context) with
				  			  True -> print_string "Reachable!\n"; (*display_model (get_model !glbl_context); flush stdout*) print_inputs curr_task.var_env curr_task.input_count
				  			| _ -> failwith "Bad sat check (this should not happen!)"); 
					  	(*revert_solver_state curr_task.rollback task_stack;
					  	smt_reverse_search graph task_stack;*)
					  	Stack.clear task_stack; [] (* switch this for the previous two lines to keep searching after having found a valid path *)
				| in_edges -> 
					let _ = List.map (fun e -> (match e with
					  (n,Seq) -> (match (Array.get graph n) with
					  	  Assign_node {var;asg_value;node_num;out_node;in_nodes} -> 
					  	     let lhs = exp_subst (BasicVar "input") [VarExp (BasicVar ("input"^(string_of_int curr_task.input_count)))] (VarExp var) in
					  	     let rhs = exp_subst (BasicVar "input") [VarExp (BasicVar ("input"^(string_of_int curr_task.input_count)))] (List.hd asg_value) in
					  	     let var_env' = fst (phi curr_task.var_env lhs) in (* this could be done better *)
					  	     let (var_env'',rhs') = phi_subst_exp var_env' rhs in
					  	     let var_env_final = if ((Varmap.find (string_of_var var) var_env') = (Varmap.find (string_of_var var) var_env')) then (fst (phi var_env'' lhs)) else var_env'' in
					  	     let lhs' = snd (phi_subst_exp curr_task.var_env lhs) in
					  	     let p = Exp_pred (BinOpAppExp (EqOp,lhs',rhs')) in
					  	     	(if (e = (List.hd (List.rev in_edges))) then 
					  	     		  	(let t = curr_task.task_num in
					  	     		  	push !glbl_context; 
					  	     		  	let inconsistent = try_pred p t in
					  	     		  		(match inconsistent with
					  	     		  			  true -> (*print_string ("\tTask " ^ (string_of_int t) ^ " unsat by Formula: " ^ (string_of_int (see_formula_count())) ^ "\n"); *)
							  	     		  			revert_solver_state curr_task.rollback task_stack;
							  	     		  			pop !glbl_context
					  	     		  			| false -> 
					  	     		  				(Stack.push 
														{task_num = t; curr_node_idx = n; depth = curr_task.depth;
														 input_count = curr_task.input_count; placeholder_pred = True; 
														 rollback = curr_task.rollback+1; var_env = var_env_final} task_stack;)))
					  	     		else 
					  	     			(let t = get_task_count() in (Stack.push 
											{task_num = t; curr_node_idx = n; depth = curr_task.depth;
											 input_count = curr_task.input_count; placeholder_pred = p; rollback = curr_task.rollback; 
											 var_env = var_env_final} task_stack;)))
						| Input_node _ -> (*print_string ("\t\t INPUT WITH LEN " ^ (string_of_int curr_task.input_count) ^ "\n");*) Stack.push
							{task_num = if (e = (List.hd (List.rev in_edges))) then (curr_task.task_num) else (get_task_count());
							 curr_node_idx = n; depth = curr_task.depth-1;
							 input_count = curr_task.input_count+1; placeholder_pred = True; rollback = curr_task.rollback;
							 var_env = curr_task.var_env} task_stack;
						| _ -> Stack.push 
							{task_num = if (e = (List.hd (List.rev in_edges))) then (curr_task.task_num) else (get_task_count());
							 curr_node_idx = n; depth = curr_task.depth;
							 input_count = curr_task.input_count; placeholder_pred = True; rollback = curr_task.rollback;
							 var_env = curr_task.var_env} task_stack;)
					| (n,Yes) -> (match (Array.get graph n) with
						  Cond_node {cond;node_num;then_node;else_node;in_nodes} -> 
						     let cond' = exp_subst (BasicVar "input") [VarExp (BasicVar ("input"^(string_of_int curr_task.input_count)))] cond in
					  	     let (var_env',p) = (let phi_subst = (phi_subst_exp curr_task.var_env cond') in (fst phi_subst, Exp_pred (snd phi_subst))) in
						  	 	(if (e = (List.hd (List.rev in_edges))) then 
					  	     		  	(let t = curr_task.task_num in
					  	     		  	push !glbl_context;
					  	     		  	let inconsistent = try_pred p t in
					  	     		  		(match inconsistent with
					  	     		  			  true -> (*print_string ("\tTask " ^ (string_of_int t) ^ " unsat by Formula: " ^ (string_of_int (see_formula_count())) ^ "\n");*)
							  	     		  			revert_solver_state curr_task.rollback task_stack;
							  	     		  			pop !glbl_context
					  	     		  			| false -> 
					  	     		  				(Stack.push 
														{task_num = t; curr_node_idx = n; depth = curr_task.depth;
														 input_count = curr_task.input_count; placeholder_pred = True; 
														 rollback = curr_task.rollback+1; var_env = var_env'} task_stack;)))
					  	     		else 
					  	     			(let t = get_task_count() in (Stack.push 
											{task_num = t; curr_node_idx = n; depth = curr_task.depth;
											 input_count = curr_task.input_count; placeholder_pred = p; rollback = curr_task.rollback; 
											 var_env = var_env'} task_stack;)))
						| _ -> failwith "malformed graph")
					| (n,No) -> (match (Array.get graph n) with
						  Cond_node {cond;node_num;then_node;else_node;in_nodes} -> 
						     let cond' = exp_subst (BasicVar "input") [VarExp (BasicVar ("input"^(string_of_int curr_task.input_count)))] cond in
						     let (var_env',p) = (let phi_subst = (phi_subst_exp curr_task.var_env cond') in (fst phi_subst, Not (Exp_pred (snd phi_subst)))) in
						  	 	(if (e = (List.hd (List.rev in_edges))) then 
					  	     		  	(let t = curr_task.task_num in
					  	     		  	push !glbl_context;
					  	     		  	let inconsistent = try_pred p t in
					  	     		  		(match inconsistent with
					  	     		  			  true -> (*print_string ("\tTask " ^ (string_of_int t) ^ " unsat by Formula: " ^ (string_of_int (see_formula_count())) ^ "\n");*)
							  	     		  			revert_solver_state curr_task.rollback task_stack;
							  	     		  			pop !glbl_context
					  	     		  			| false ->
					  	     		  				(Stack.push 
														{task_num = t; curr_node_idx = n; depth = curr_task.depth;
														 input_count = curr_task.input_count; placeholder_pred = True; 
														 rollback = curr_task.rollback+1; var_env = var_env'} task_stack;)))
					  	     		else 
					  	     			(let t = get_task_count() in (Stack.push 
											{task_num = t; curr_node_idx = n; depth = curr_task.depth;
											 input_count = curr_task.input_count; placeholder_pred = p; rollback = curr_task.rollback; 
											 var_env = var_env'} task_stack;)))
						| _ -> failwith "malformed graph")
					| (n,_) -> Stack.push
						  	  	{task_num = if (e = (List.hd (List.rev in_edges))) then (curr_task.task_num) else (get_task_count());
						  	  	 curr_node_idx = n; depth = curr_task.depth; input_count = curr_task.input_count;
						  	  	 placeholder_pred = True; rollback = curr_task.rollback; var_env = curr_task.var_env} 
						  	  	 task_stack;)) in_edges in smt_reverse_search graph task_stack))
			| _ -> (*print_string ("Checking placeholder and resuming task " ^ (string_of_int curr_task.task_num) ^ " at node " ^ (string_of_int curr_task.curr_node_idx) ^ "\n");*)
					(let t = curr_task.task_num in
					push !glbl_context;
  	     		  	let inconsistent = try_pred curr_task.placeholder_pred t in
  	     		  		(match inconsistent with
  	     		  			  true -> (*print_string ("\tTask " ^ (string_of_int t) ^ " unsat by (placeholder) Formula: " ^ (string_of_int (see_formula_count())) ^ "\n");*)
			  	     		  			revert_solver_state curr_task.rollback task_stack;
			  	     		  			pop !glbl_context;
  	     		  			  			smt_reverse_search graph task_stack
  	     		  			| false -> (Stack.push 
											{task_num = t; curr_node_idx = curr_task.curr_node_idx; depth = curr_task.depth;
											 input_count = curr_task.input_count; placeholder_pred = True; 
											 rollback = curr_task.rollback+1; var_env = curr_task.var_env} task_stack;
										smt_reverse_search graph task_stack))))
	else ((*print_string ("Depth limit reached for task " ^ (string_of_int curr_task.task_num) ^ "\n");*)
		revert_solver_state curr_task.rollback task_stack;
		smt_reverse_search graph task_stack)) 

let init_smt_search graph depth start_node_idx =
	reset_task_count(); 
	reset_formula_count();
	reset_quot_count();
	reset_rem_count();
	reset_div_count();
	reset_mod_count();
	reset_unnamed_count();
	reset !glbl_context;
	let task_stack = Stack.create() in
	Stack.push {task_num = get_task_count(); 
		curr_node_idx = start_node_idx; depth = depth; input_count = 0;
		placeholder_pred = True; rollback = 0; var_env = (Varmap.empty)} task_stack;
	smt_reverse_search graph task_stack


let init_smt_search_all_terminals graph depth =
	(let terminal_node_idxs = get_terminal_node_idx_list graph in
	(*List.iter (fun idx -> 
		let errno = (get_crash_val (Array.get graph idx)) in
		if  true then
			(print_string ("Now searching from error_" ^ (string_of_int errno) ^ "... "); flush_all();
			init_smt_search graph depth idx; ()) else ()) terminal_node_idxs)*)
	Parmap.pariter ~ncores:2 (fun idx -> 
		let errno = (get_crash_val (Array.get graph idx)) in
		if true then
			(print_string ("Now searching from error_" ^ (string_of_int errno) ^ "... "); flush_all();
			init_smt_search graph depth idx; ()) else ()) (Parmap.L terminal_node_idxs))

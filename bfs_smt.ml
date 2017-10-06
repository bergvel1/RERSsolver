(* Breadth-first search over control flow graphs, to be used with a blackboxed SMT solver *)
(* This algorithm collects preconditions to pass to an SMT solver once a search trace reaches the head of the graph. *)
(* In the future, I'd like to try calling the solver at every step of the search (effectively using the SMT solver as a simplification engine), *)
(* 	but not all SMT solvers can reliably save and restore their internal states. *)
open Graph
open Eval_exp
open Solverintf

module Solver = Make_AEZ()
(*module Solver = Make_Yices()*)

type task = 
	{	task_num:int;
		curr_node_idx:int;
		depth:int;
		condition:pred;
		input_count:int (* possibly redundant with depth *)
	}

let (get_task_count, see_task_count, reset_task_count) =
  let count = ref 1 in
  ((fun () -> (let c = !count in (count := c + 1; c))),
   (fun () -> (!count)),
   (fun () -> count := 1))

let rec smt_reverse_bfs graph task_queue =
if (Queue.is_empty task_queue) then print_string "no more tasks\n" else
	let curr_task = Queue.pop task_queue in
	(*print_string ("Now working on " ^ (string_of_task curr_task));*)
	if ((curr_task.depth > -1) && (curr_task.condition != False)) then
		(let curr_node = (Array.get graph curr_task.curr_node_idx) in
		(match (get_in_nodes curr_node) with
		  [] -> Solver.assume curr_task.condition; Solver.check(); smt_reverse_bfs graph task_queue
		| in_edges -> let _ = List.map (fun e -> (match e with
			  (n,Seq) -> (match (Array.get graph n) with
			  	  Assign_node {var;asg_value;node_num;out_node;in_nodes} -> Queue.push 
					{task_num = if (e = (List.hd in_edges)) then curr_task.task_num else get_task_count();
					 curr_node_idx = n; 
					 depth = curr_task.depth;
					 condition = (*shake_eval_pred*) (pred_subst var asg_value curr_task.condition);
					 input_count = curr_task.input_count} task_queue;
				| Input_node _ -> Queue.push
					{task_num = if (e = (List.hd in_edges)) then curr_task.task_num else get_task_count();
					 curr_node_idx = n;
					 depth = curr_task.depth-1;
					 condition = pred_subst (BasicVar "input") [VarExp (BasicVar ("input"^(string_of_int curr_task.input_count)))] curr_task.condition;
					 input_count = curr_task.input_count+1} task_queue;
				| _ -> Queue.push 
					{task_num = if (e = (List.hd in_edges)) then curr_task.task_num else get_task_count();
					 curr_node_idx = n; 
					 depth = curr_task.depth;
					 condition = curr_task.condition;
					 input_count = curr_task.input_count} task_queue;)
			| (n,Yes) -> (match (Array.get graph n) with
				  Cond_node {cond;node_num;then_node;else_node;in_nodes} -> Queue.push 
					{task_num = if (e = (List.hd in_edges)) then curr_task.task_num else get_task_count();
					 curr_node_idx = n; 
					 depth = curr_task.depth;
					 condition = (*shake_eval_pred*) (And ((Exp_pred cond),curr_task.condition));
					 input_count = curr_task.input_count} task_queue;
				| _ -> failwith "malformed graph")
			| (n,No) -> (match (Array.get graph n) with
				  Cond_node {cond;node_num;then_node;else_node;in_nodes} -> Queue.push 
					{task_num = if (e = (List.hd in_edges)) then curr_task.task_num else get_task_count();
					 curr_node_idx = n; 
					 depth = curr_task.depth;
					 condition = (*shake_eval_pred*) (And ((Not (Exp_pred cond)),curr_task.condition));
					 input_count = curr_task.input_count} task_queue;
				| _ -> failwith "malformed graph")
			| (n,_) -> Queue.push 
					{task_num = if (e = (List.hd in_edges)) then curr_task.task_num else get_task_count();
					 curr_node_idx = n; 
					 depth = curr_task.depth;
					 condition = curr_task.condition;
					 input_count = curr_task.input_count} task_queue;)) in_edges in smt_reverse_bfs graph task_queue))
	else (*print_string ("Terminated " ^ (string_of_task curr_task));*)
		smt_reverse_bfs graph  task_queue 

let init_smt_reverse_bfs graph depth start_node_idx =
	Solver.reset();
	reset_task_count(); 
	let task_queue = Queue.create() in
	Queue.push {task_num = get_task_count(); 
		curr_node_idx = start_node_idx; depth = depth; condition = True;
		input_count = 0} task_queue;
	smt_reverse_bfs graph task_queue

open Graph
open Eval_exp
open Fuzz

type task = 
	{	task_num:int;
		curr_node_idx:int;
		depth:int;
		condition:pred;
		input_count:int;
	}

let (get_task_count, see_task_count, reset_task_count) =
  let count = ref 1 in
  ((fun () -> (let c = !count in (count := c + 1; c))),
   (fun () -> (!count)),
   (fun () -> count := 1))

(* To be deleted... *)
(*let print_end_result p inputs = print_string ("Reached start node with condition: " ^ (string_of_pred p) ^ " and inputs: " ^ (String.concat "," (List.map string_of_int inputs)) ^ "\n")*)

let string_of_task t = "Task #" ^ (string_of_int t.task_num) ^ ", node_idx:" ^ 
	(string_of_int t.curr_node_idx) ^ ", depth:" ^ (string_of_int t.depth) ^
	", condition:" ^ (string_of_pred t.condition) ^ ".\n"

let rec subst_reverse_bfs graph input_alph task_queue =
if (Queue.is_empty task_queue) then print_string "no more tasks\n" else
	let curr_task = Queue.pop task_queue in
	(*print_string ("Now working on " ^ (string_of_task curr_task));*)
	if ((curr_task.depth > -1) && (curr_task.condition != False)) then
		(let curr_node = (Array.get graph curr_task.curr_node_idx) in
		(match (get_in_nodes curr_node) with
		  [] -> fuzz_sat_check input_alph curr_task.input_count curr_task.condition; subst_reverse_bfs graph input_alph task_queue
		| in_edges -> let _ = List.map (fun e -> (match e with
			  (n,Seq) -> (match (Array.get graph n) with
			  	  Input_node _ -> Queue.push
		  		  	{task_num = if (e = (List.hd in_edges)) then curr_task.task_num else get_task_count();
		  		  	 curr_node_idx = n;
		  		  	 depth = curr_task.depth-1;
		  		  	 condition = pred_subst (BasicVar "input") [VarExp (BasicVar ("input"^(string_of_int curr_task.input_count)))] curr_task.condition;
		  		  	 input_count = curr_task.input_count+1;} task_queue
			  	| Assign_node {var;asg_value;node_num;out_node;in_nodes} -> Queue.push 
					{task_num = if (e = (List.hd in_edges)) then curr_task.task_num else get_task_count();
					 curr_node_idx = n; 
					 depth = curr_task.depth;
					 condition = shake_eval_pred (pred_subst var asg_value curr_task.condition);
					 input_count = curr_task.input_count} task_queue;
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
					 condition = shake_eval_pred (And ((Exp_pred cond),curr_task.condition));
					 input_count = curr_task.input_count} task_queue;
				| _ -> failwith "malformed graph")
			| (n,No) -> (match (Array.get graph n) with
				  Cond_node {cond;node_num;then_node;else_node;in_nodes} -> Queue.push 
					{task_num = if (e = (List.hd in_edges)) then curr_task.task_num else get_task_count();
					 curr_node_idx = n; 
					 depth = curr_task.depth;
					 condition = shake_eval_pred (And ((Not (Exp_pred cond)),curr_task.condition));
					 input_count = curr_task.input_count} task_queue;
				| _ -> failwith "malformed graph")
			| (n,_) -> Queue.push 
					{task_num = if (e = (List.hd in_edges)) then curr_task.task_num else get_task_count();
					 curr_node_idx = n; 
					 depth = curr_task.depth;
					 condition = curr_task.condition;
					 input_count = curr_task.input_count} task_queue;)) in_edges in subst_reverse_bfs graph input_alph task_queue))
	else (*print_string ("Terminated " ^ (string_of_task curr_task));*)
		subst_reverse_bfs graph input_alph task_queue 

let init_subst_reverse_bfs graph input_alph depth start_node_idx =
	reset_task_count(); 
	let task_queue = Queue.create() in
	Queue.push {task_num = get_task_count(); 
		curr_node_idx = start_node_idx; depth = depth; 
		condition = True; input_count = 0;} task_queue;
	subst_reverse_bfs graph input_alph task_queue
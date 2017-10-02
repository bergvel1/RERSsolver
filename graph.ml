open Abs_syn
open Lex
open Parse

exception Bad_instruction of Abs_syn.inst
exception Bad_block of Abs_syn.block

type edge_label = Seq | Yes | No | CallFrom of string | ReturnTo of string

let string_of_edge_label l =
  match l with Seq -> "Seq"
    | Yes -> "T"
    | No -> "F"
    | CallFrom p -> p^"->"
    | ReturnTo p -> "->"^p

let string_of_edge start_num (end_num, edge_label) =
  (string_of_int start_num)^" -> "^(string_of_int end_num) ^
    " [label = \""^(string_of_edge_label edge_label)^"\"];\n"

let make_node_string node_num node_label shape out_edges =
  (List.fold_left
     (fun s -> fun out_edge -> s^(string_of_edge node_num out_edge))
     ((string_of_int node_num)^" [label=\""^node_label^"\" shape="^shape^
         " ordering=\"out\"];\n")
     out_edges)

(* Corresponds to program instructions *)
type node =
    Begin_node of {out_node : (int * edge_label)}
  | Assign_node of
      {var : Abs_syn.var; asg_value : Abs_syn.exp list;
       node_num : int; out_node : (int * edge_label);
       mutable in_nodes : (int * edge_label) list}
  | Cond_node of
      {cond : Abs_syn.exp; node_num :int; then_node : (int * edge_label);
       else_node : (int * edge_label);
       mutable in_nodes : (int * edge_label) list}
  | Output_node of
      {output_val : Abs_syn.exp; node_num :int;
       out_node : (int * edge_label);
       mutable in_nodes : (int * edge_label) list} 
  | Input_node of
  	  {node_num:int; out_node:(int * edge_label);
  	   mutable in_nodes : (int * edge_label) list}
  | Error_msg_node of
      {node_num :int; out_node : (int * edge_label);
       mutable in_nodes : (int * edge_label) list}
  | Terminal_node of
      {crash_val : int; node_num :int;
       mutable in_nodes : (int * edge_label) list}
(* temporary? *)
  | Function_entry_node of
      {node_num :int; fun_name : string; out_node : (int * edge_label);
       mutable in_nodes : (int * edge_label) list}
  | Function_exit_node of
      {node_num :int; mutable out_nodes : (int * edge_label) list;
       mutable in_nodes : (int * edge_label) list}
  | Dummy_node of {node_num :int; mutable in_nodes : (int * edge_label) list}
  | Call_node of {node_num:int; calling:string; out_node : (int * edge_label);
  	   mutable in_nodes: (int * edge_label) list}


let string_of_node node =
  match node
  with Begin_node {out_node} -> make_node_string 0 "Start" "oval" [out_node]
     | Assign_node {var; asg_value; node_num; out_node; in_nodes} ->
       let node_label =
         ((string_of_var var)^" = "^
          (match asg_value
           with
	       [] -> "???"
	     | (hd :: []) -> (string_of_exp hd)
	     | _ -> "{" ^
                 (String.concat "," (List.map string_of_exp asg_value)) ^ "}"))
       in
       make_node_string node_num node_label "box" [out_node]
     | Cond_node {cond; node_num; then_node; else_node; in_nodes} ->
       make_node_string node_num ("?"^(string_of_exp cond)) "diamond"
         [then_node; else_node]
     | Output_node {output_val; node_num; out_node; in_nodes} -> 
     	make_node_string node_num ("PRINT "^(string_of_exp output_val)) "circle"
     		[out_node]
     | Input_node {node_num; out_node; in_nodes} ->
     	make_node_string node_num "INPUT" "egg" [out_node]
     | Error_msg_node {node_num; out_node; in_nodes} ->
     	make_node_string node_num "Invalid input" "trapezium" [out_node]
     | Terminal_node {crash_val; node_num; in_nodes} -> 
     	make_node_string node_num ("VERIFIER_error "^(string_of_int crash_val))
     		"triangle" []
     | Function_entry_node {node_num; fun_name; out_node; in_nodes} ->
     	make_node_string node_num fun_name "plain" [out_node]
     | Function_exit_node {node_num; out_nodes; in_nodes} ->
     	make_node_string node_num "return" "plain" out_nodes
     | Dummy_node {node_num; in_nodes} -> 
     	make_node_string node_num "" "star" []
     | Call_node {node_num; calling; out_node; in_nodes;} ->
     	make_node_string node_num ("goto "^calling) "invhouse" [out_node]

let inst_num inst =
  (match inst
   with OutputInst (n,e) -> n
     | InputInst n -> n
     | ErrorMsgInst n -> n
     | CrashInst (n,i) -> n
     | IfInst (n,e,l) -> n
     | IfElseInst (n,e,l1,l2) -> n
     | WhileInst (n,e,l) -> n
     | ExpInst (n,e) -> n
 	 | ReturnInst n -> n)

let inst_list_num inst_list = 
  (match inst_list with [] -> failwith "No instructions\n"
    | inst :: _ -> inst_num inst)

(*
let blk_num blk =
  (match blk
   with GlobalInsts (l) -> inst_list_num l
     | FunctionBlock ((f,params),l) -> inst_list_num l)

let blk_list_num blk_list = 
  (match blk_list with [] -> failwith "No program\n"
    | blk :: _ -> blk_num blk)

let next_inst_num inst_list blk_list =
  (match inst_list
   with [] -> blk_list_num blk_list
     | (inst  :: _) -> inst_num inst)
*)
let update_in_nodes node new_in_node =
  (match node
   with Begin_node n -> failwith "Nothing should point to the beginning.\n"
     | Assign_node n -> (n.in_nodes <- new_in_node :: n.in_nodes)
     | Cond_node n -> (n.in_nodes <- new_in_node :: n.in_nodes)
     | Output_node n -> (n.in_nodes <- new_in_node :: n.in_nodes)
     | Input_node n -> (n.in_nodes <- new_in_node :: n.in_nodes)
     | Error_msg_node n -> (n.in_nodes <- new_in_node :: n.in_nodes)
     | Terminal_node n -> (n.in_nodes <- new_in_node :: n.in_nodes)
     | Function_entry_node n -> (n.in_nodes <- new_in_node :: n.in_nodes)
     | Function_exit_node n -> (n.in_nodes <- new_in_node :: n.in_nodes)
     | Dummy_node n -> (n.in_nodes <- new_in_node :: n.in_nodes)
     | Call_node n -> (n.in_nodes <- new_in_node :: n.in_nodes)
  )

let update_out_nodes node new_out_node =
	(match node
	 with Function_exit_node n -> (n.out_nodes <- new_out_node :: n.out_nodes)
		| _ -> failwith "Only Function_exit_nodes have mutable out_nodes")

let get_in_nodes node =
  (match node
   with Begin_node n -> []
     | Assign_node n -> n.in_nodes
     | Cond_node n -> n.in_nodes
     | Output_node n -> n.in_nodes
     | Input_node n -> n.in_nodes
     | Error_msg_node n -> n.in_nodes
     | Terminal_node n -> n.in_nodes
     | Function_entry_node n -> n.in_nodes
     | Function_exit_node n -> n.in_nodes
     | Dummy_node n -> n.in_nodes
     | Call_node n -> n.in_nodes
  )

let get_node_number node =
  (match node
   with Begin_node n -> 0
     | Assign_node n -> n.node_num
     | Cond_node n -> n.node_num
     | Output_node n -> n.node_num
     | Input_node n -> n.node_num
     | Error_msg_node n -> n.node_num
     | Terminal_node n -> n.node_num
     | Function_entry_node n -> n.node_num
     | Function_exit_node n -> n.node_num
     | Dummy_node n -> n.node_num  (* Or should this be some kind of error? *)
     | Call_node n -> n.node_num
  )

 let get_asg_value node =
   (match node
    with Assign_node n -> n.asg_value
	  | _ -> failwith "Not an assignment node")

(* could be more efficient if created during graph construction *)
let get_terminal_node_idx_list graph = 
List.concat (List.map (fun n -> (match n with
						  Terminal_node {crash_val;node_num;in_nodes} -> if (crash_val < 0) then [] else [node_num]
						| _ -> [])) (Array.to_list graph))

let get_input_alph graph = 
let inputsnode = (List.find (fun n -> (match n with
					  Assign_node {var; asg_value; node_num; out_node; in_nodes} -> 
				  	    if (String.equal "inputs" (string_of_var var)) then true else false
				  	| _ -> false)) (Array.to_list graph)) in
	List.concat (List.map (fun n -> (match n with
					  ConstExp (IntConst i) -> [i]
					| _ -> [])) (get_asg_value inputsnode))


let mk_graph blk_list =
  let (fun_defs, gen_code_blks) =
    List.partition
      (function FunctionBlock _ -> true | GlobalInsts _ -> false)
      blk_list in
  let fun_env =
    List.fold_left
      (fun env ->
        (function FunctionBlock ((f,args), body) -> (match f with
        		  "main" -> (let newbody = (match body with
        		  	  (WhileInst (n,e,l))::[retI] -> l@[ExpInst (n,CallExp("main",[]))]@[retI]
        		  	| _ -> failwith "malformed main()") in (f,newbody)::env)
        		| _ -> (f,body)::env)
          | _ -> failwith "impossible"))
      []
      fun_defs
  in 
  let gen_code_list =
    List.fold_right
      (function GlobalInsts l -> fun insts -> l @ insts
        | _ -> failwith "impossible")
      gen_code_blks
      [ExpInst (get_node_count(), CallExp("main",[])); ReturnInst (get_node_count())]
  in
  let first_node = inst_list_num gen_code_list in
  let graph =
    Array.init (see_node_count())
      (fun n -> Dummy_node {node_num = n; in_nodes = []}) in
  let _ = (Array.set graph 0 (Begin_node {out_node = (first_node, Seq)});
           update_in_nodes (Array.get graph first_node) (0, Seq)) in
  let rec put_nodes inst_list return_stack fun_def_env =
    (match inst_list with [] -> graph
      | inst :: insts ->
      	(*print_string ((Abs_syn.string_of_inst inst) ^ "\n\n");*) (* debug *)
      	(match (Array.get graph (inst_num inst))
        with Dummy_node _ -> (match inst
	         with OutputInst (n,e) ->
	           (let next_node_num = inst_list_num insts in
	           (Array.set graph n
	             (Output_node
	                {node_num = n; output_val = e;
	                 in_nodes = get_in_nodes (Array.get graph n);
	                 out_node = (next_node_num,Seq)}));
	           (update_in_nodes (Array.get graph next_node_num) (n, Seq));
	           put_nodes insts return_stack fun_def_env)
	           | ErrorMsgInst n ->
	             (let next_node_num = inst_list_num insts in
	              (Array.set graph n
	                 (Error_msg_node
	                    {node_num = n;
	                     in_nodes = get_in_nodes (Array.get graph n);
	                     out_node = (next_node_num,Seq)}));
	              (update_in_nodes (Array.get graph next_node_num) (n, Seq));
	              put_nodes insts return_stack fun_def_env)
	           | CrashInst (n,i) ->
	             (Array.set graph n
	               (Terminal_node
	                  {node_num = n; crash_val = i;
	                   in_nodes = get_in_nodes (Array.get graph n)});
	             put_nodes insts return_stack fun_def_env) (* Do we need this? *)
	           | IfInst (n,e,l) ->
	             (let next_node_num = inst_list_num insts in
	              let then_node_num =
	                (match l with [] -> next_node_num | _ -> inst_list_num l) in
	              (Array.set graph n
	                (Cond_node
	                   {node_num = n; cond = e; 
	                    in_nodes = get_in_nodes (Array.get graph n);
	                    then_node = (then_node_num, Yes);
	                    else_node = (next_node_num, No)});
	              (update_in_nodes (Array.get graph then_node_num) (n, Yes));
	              (update_in_nodes (Array.get graph next_node_num) (n, No));
	              (* Set out_node of last node in then branch to next_node_num *)
	              put_nodes (l@insts) return_stack fun_def_env))
	            | IfElseInst (n,e,l1,l2) ->
	              (let next_node_num = inst_list_num insts in
	               let then_node_num =
	           		 (match l1 with [] -> next_node_num | _ -> inst_list_num l1) in
	           	   let else_node_num =
	           		 (match l2 with [] -> next_node_num | _ -> inst_list_num l2) in
	           	   (Array.set graph n
	           	     (Cond_node
	           	        {node_num = n; cond = e;
	           	    	 in_nodes = get_in_nodes (Array.get graph n);
	           	    	 then_node = (then_node_num, Yes);
	           	    	 else_node = (else_node_num, No)}));
	           	 	 (update_in_nodes (Array.get graph then_node_num) (n,Yes));
	           	 	 (update_in_nodes (Array.get graph else_node_num) (n,No)));
	           	 	 put_nodes (l1@l2@insts) return_stack fun_def_env;
	           	 	 (*put_nodes (insts@l2) return_stack fun_def_env;*)
	           	 	 (* Set out_node of last node in then branch to next_node_num *)
	           	 	 (* Set out_node of last node in else branch to next_node_num *)
	           	| InputInst n ->
	           		(let next_node_num = inst_list_num insts in
	           			(Array.set graph n
	           				(Input_node
	           					{node_num = n;
	           					 in_nodes = get_in_nodes (Array.get graph n);
	           					 out_node = (next_node_num,Seq)}));
	           			(update_in_nodes (Array.get graph next_node_num) (n,Seq));
	           		put_nodes insts return_stack fun_def_env)
	           	| WhileInst (n,e,l) -> put_nodes ((l @ [ExpInst (n,CallExp("main",[]))]) @ insts) return_stack fun_def_env (* this should not be used *)
	           	| ExpInst (n,e) ->
	           		(match e with
	           			  AssignExp (v,exps) ->
	           			  	(let next_node_num = inst_list_num insts in 
	           			  		(Array.set graph n
	           			  			(Assign_node
	           			  				{var = v; asg_value = exps;
	           			  				 node_num = n;
	           			  				 in_nodes = get_in_nodes (Array.get graph n);
	           			  				 out_node = (next_node_num,Seq)}));
	           			  		(update_in_nodes (Array.get graph next_node_num) (n,Seq));
	           			  	put_nodes insts return_stack fun_def_env)
	           			| CallExp (f,exps) -> 
	           				(let current_function = List.hd return_stack in
	           				(let next_node_num = inst_list_num insts in
	           				(let target_insts = (List.assoc f fun_def_env) in 
		           			(let target_node = Array.get graph (inst_list_num target_insts) in
		           			(let return_node_num = inst_num (List.hd (List.rev target_insts)) in 
	           					(Array.set graph n
           						(Call_node
           							{calling = f; node_num = n;
           							 in_nodes = get_in_nodes (Array.get graph n);
           							 out_node = ((get_node_number target_node), CallFrom(current_function))}));
	           					update_in_nodes target_node (n,CallFrom(current_function));
	           					put_nodes target_insts (f::return_stack) fun_def_env;
	           					update_out_nodes (Array.get graph return_node_num) (next_node_num,ReturnTo(current_function));
	           					update_in_nodes (Array.get graph next_node_num) (return_node_num,ReturnTo(current_function));
	           					put_nodes insts return_stack fun_def_env))))) 
	           			| _ -> put_nodes insts return_stack fun_def_env) (* no-op *)
	           	| ReturnInst n ->
	           		(Array.set graph n
	               		(Function_exit_node
	                  		{node_num = n; out_nodes = [];
	                   		 in_nodes = get_in_nodes (Array.get graph n)}));
	             	put_nodes insts return_stack fun_def_env (* Do we need this? *)               
        )
		| _ -> put_nodes insts return_stack fun_def_env)
    )
  in 
  put_nodes gen_code_list ["global"] fun_env

let mk_graph_from_file filename = 
	reset_node_count();
	let c = open_in filename in
	let lexbuf = Lexing.from_channel c in
	let blocks = Parse.main Lex.token lexbuf in
	mk_graph blocks

let string_of_graph problem_name graph =
  (Array.fold_left
     (fun s -> fun node -> s ^ (string_of_node node))
     ("digraph "^problem_name^"\n"^"{\n")
     graph) ^ "}\n"



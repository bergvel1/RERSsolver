(* Some helpful functions for checking satisfiability via fuzzing *)
open Eval_exp


(* https://rosettacode.org/wiki/Combinations_with_repetitions#OCaml *)
let rec combs_with_rep k xxs =
  match k, xxs with
  | 0,  _ -> [[]]
  | _, [] -> []
  | k, x::xs ->
      List.map (fun ys -> x::ys) (combs_with_rep (k-1) xxs)
      @ combs_with_rep k xs

(* https://rosettacode.org/wiki/Permutations#OCaml *)
let rec permutations l =
   let n = List.length l in
   if n = 1 then [l] else
   let rec sub e = function
      | [] -> failwith "sub"
      | h :: t -> if h = e then t else h :: sub e t in
   let rec aux k =
      let e = List.nth l k in
      let subperms = permutations (sub e l) in
      let t = List.map (fun a -> e::a) subperms in
      if k < n-1 then List.rev_append t (aux (k+1)) else t in
   aux 0;; 

(* todo: find a way to generate permutations without duplicates from the start *)
let rec remove_duplicates l =
(match l with
  [] -> []
| h::t -> h::(remove_duplicates (List.filter (fun x -> x <> h) t)))

let (get_curr_idx, see_curr_idx, reset_curr_idx) =
	let count = ref 0 in 
	((fun () -> (let c = !count in (count := c + 1; c))),
     (fun () -> (!count)),
     (fun () -> count := 0))

let try_input_list inputs p = 
	reset_curr_idx();
	let p_subst = List.fold_left (fun pred i -> pred_subst (BasicVar ("input" ^ (string_of_int (get_curr_idx())))) [ConstExp (IntConst i)] pred) p inputs in
		(match (shake_eval_pred p_subst) with
			  True -> print_string ("\t\tSat with inputs: " ^ (String.concat "," (List.map string_of_int (List.rev inputs))) ^ "\n")
			| other -> (* print_string ("Unsat: " ^ (string_of_pred other)) ^ "\n" *) ())

let fuzz_sat_check input_alph len p =
	let input_sequences = remove_duplicates (List.flatten (List.map permutations (combs_with_rep len input_alph))) in
		List.iter (fun i -> try_input_list i p) input_sequences 
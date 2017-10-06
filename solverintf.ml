(* Functors to create solver modules *)
open Eval_exp
open Abs_syn

module type SolverInterface =
	sig
		val reset: unit -> unit
		val assume: pred -> unit
		val check: unit -> unit
		val print_model: unit -> unit
		val try_assume: pred -> bool
	end

module Make_AEZ() : SolverInterface = 
	struct
		open Aez
		open Smt
		module InternalSolver = Make (struct end)
		module T = Term
		module F = Formula

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

		let reset () = reset_formula_count(); InternalSolver.clear()
		let assume p = (*print_string ("Assume: " ^ (string_of_pred p) ^ "... ");*)
					   let n = get_formula_count() in 
					   let f = formula_of_pred p in 
					   	try
					   	 InternalSolver.assume ~id:n f
					   	with Unsat _ -> () (*print_string "Unsat (assume) \n"*)
		let check () = try (InternalSolver.check(); print_string "Sat!\n") with Unsat _ -> () (*print_string "Unsat (check)\n"*)
		let print_model () = print_string "AEZ cannot print models.\n"
		let try_assume p = true
	end

module Make_Yices() : SolverInterface = 
	struct
		open Yices

		let context = ref (mk_context())
		(* should we add a map of var_decls? *)

		let rec term_of_exp e =
		(match e with
		  VarExp v -> let v_decl = try 
		  							(get_var_decl_from_name !context (string_of_var v)) 
		  				with Failure _ -> (mk_var_decl !context (string_of_var v) (mk_type !context "int")) in
		  					mk_var_from_decl !context v_decl
		| ConstExp (IntConst i) -> mk_num !context i
		| ConstExp (BoolConst true) -> mk_true !context
		| ConstExp (BoolConst false) -> mk_false !context
		| MonOpAppExp (IntNegOp,x) -> mk_sub !context (Array.of_list [(mk_num !context 0);(term_of_exp x)])
		| BinOpAppExp (PlusOp,x,y) -> mk_sum !context (Array.of_list [(term_of_exp x);(term_of_exp y)])
		| BinOpAppExp (MinusOp,x,y) -> mk_sub !context (Array.of_list [(term_of_exp x);(term_of_exp y)])
		| BinOpAppExp (TimesOp,x,y) -> mk_mul !context (Array.of_list [(term_of_exp x);(term_of_exp y)])
		(*| BinOpAppExp (DivOp,x,y) -> T.make_arith T.Div (term_of_exp x) (term_of_exp y)*)
		| TernExp (p,x,y) -> mk_ite !context (formula_of_pred (Exp_pred p)) (term_of_exp x) (term_of_exp y)
		| _ -> failwith "term_of_exp"
		)

		and formula_of_pred p =
		(match p with
		  True -> mk_true !context
		| False -> mk_false !context
		| And (x,y) -> mk_and !context (Array.of_list [(formula_of_pred x);(formula_of_pred y)])
		| Or (x,y) -> mk_or !context (Array.of_list [(formula_of_pred x);(formula_of_pred y)])
		(*| Implies (x,y) -> F.make F.Imp [(formula_of_pred x);(formula_of_pred y)]*)
		| Not x -> mk_not !context (formula_of_pred x)
		| Exp_pred (ConstExp (BoolConst true)) -> mk_true !context
		| Exp_pred (ConstExp (BoolConst false)) -> mk_false !context
		| Exp_pred (BinOpAppExp(GreaterOp,x,y)) -> mk_gt !context (term_of_exp y) (term_of_exp x)
		| Exp_pred (BinOpAppExp(LEqOp,x,y)) -> mk_le !context (term_of_exp x) (term_of_exp y)
		| Exp_pred (BinOpAppExp(LessOp,x,y)) -> mk_lt !context (term_of_exp x) (term_of_exp y)
		| Exp_pred (BinOpAppExp(GEqOp,x,y)) -> mk_ge !context (term_of_exp y) (term_of_exp x)
		| Exp_pred (BinOpAppExp(EqOp,x,y)) -> mk_eq !context (term_of_exp x) (term_of_exp y)
		| Exp_pred (BinOpAppExp(NEqOp,x,y)) -> mk_diseq !context (term_of_exp x) (term_of_exp y)
		| Exp_pred (BinOpAppExp(AndOp,x,y)) -> mk_and !context (Array.of_list [(formula_of_pred (Exp_pred x));(formula_of_pred (Exp_pred y))])
		| Exp_pred (BinOpAppExp(OrOp,x,y)) -> mk_or !context (Array.of_list [(formula_of_pred (Exp_pred x));(formula_of_pred (Exp_pred y))])
		| Exp_pred (MonOpAppExp(BoolNegOp,x)) -> mk_not !context (formula_of_pred (Exp_pred x))	
		| _ -> failwith "formula_of_pred"
		)

		let try_assume p = let f = (formula_of_pred p) in
						   let res = (push !context;
						 	assert_simple !context f;
						 	match (check !context) with
							  	  False -> false
								| _ -> true) in
						   	pop !context; res

		let reset () = del_context !context; context := mk_context()
		let assume p = let f = (formula_of_pred p) in ((*pp_expr f;*) assert_simple !context f)
		let check () = (match (check !context) with
			  True -> (*print_string "True with model: \n"; *)display_model (get_model !context)
			| False -> (*print_string "False\n"*) ()
			| Undef -> (*print_string "Undef with model: \n";*) display_model (get_model !context)); reset()
		let print_model () = print_string "use check() instead\n"

		
	end


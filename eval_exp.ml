open Abs_syn

type pred = 
  True
| False
| Exp_pred of Abs_syn.exp
| And of pred*pred
| Not of pred
| Or of pred*pred
| Implies of pred*pred

let eval_aexp_aux e =
(match e with
  ConstExp (IntConst c) -> ConstExp (IntConst c)
| VarExp v -> VarExp v
| MonOpAppExp (IntNegOp,ConstExp (IntConst c)) -> ConstExp (IntConst (0-c))
| MonOpAppExp (IntNegOp,x) -> MonOpAppExp (IntNegOp,x)
| BinOpAppExp (DivOp,ConstExp (IntConst x), ConstExp (IntConst y)) -> ConstExp (IntConst (x/y))
| BinOpAppExp (DivOp,x, ConstExp (IntConst 1)) -> x
| BinOpAppExp (DivOp,x,y) -> BinOpAppExp (DivOp,x,y)
| BinOpAppExp (MinusOp,ConstExp (IntConst x),ConstExp (IntConst y)) -> ConstExp (IntConst (x-y))
| BinOpAppExp (MinusOp,x,ConstExp (IntConst 0)) -> x
| BinOpAppExp (MinusOp,(BinOpAppExp (MinusOp,x,ConstExp (IntConst y))),ConstExp (IntConst z)) -> BinOpAppExp (MinusOp,x,ConstExp (IntConst (y+z)))
| BinOpAppExp (MinusOp,(BinOpAppExp (PlusOp,x,ConstExp (IntConst y))),ConstExp (IntConst z)) ->
if y=z then x
	else if y>z then BinOpAppExp (PlusOp,x,ConstExp (IntConst (y-z)))
				else BinOpAppExp (MinusOp,x,ConstExp (IntConst (z-y)))
| BinOpAppExp (MinusOp,x,y) -> BinOpAppExp (MinusOp,x,y)
| BinOpAppExp (PlusOp,ConstExp (IntConst x),ConstExp (IntConst y)) -> ConstExp (IntConst (x+y))
| BinOpAppExp (PlusOp,x,ConstExp (IntConst 0)) -> x
| BinOpAppExp (PlusOp,(BinOpAppExp (PlusOp,x,ConstExp (IntConst y))),ConstExp (IntConst z)) -> BinOpAppExp (PlusOp,x,ConstExp (IntConst (y+z)))
| BinOpAppExp (PlusOp,ConstExp (IntConst x),(BinOpAppExp (PlusOp,ConstExp (IntConst y),z))) -> BinOpAppExp (PlusOp,z,ConstExp (IntConst (x+y)))
| BinOpAppExp (PlusOp,ConstExp (IntConst x),(BinOpAppExp (PlusOp,y,ConstExp (IntConst z)))) -> BinOpAppExp (PlusOp,y,ConstExp (IntConst (x+z)))
| BinOpAppExp (PlusOp,ConstExp (IntConst x),(BinOpAppExp (MinusOp,y,ConstExp (IntConst z)))) ->
if x=z then y
	else if x>z then BinOpAppExp (PlusOp,y,ConstExp (IntConst (x-z)))
				else BinOpAppExp (MinusOp,y,ConstExp (IntConst (z-x)))
| BinOpAppExp (PlusOp,(BinOpAppExp (MinusOp,y,ConstExp (IntConst z))),ConstExp (IntConst x)) ->
if x=z then y
	else if x>z then BinOpAppExp (PlusOp,y,ConstExp (IntConst (x-z)))
				else BinOpAppExp (MinusOp,y,ConstExp (IntConst (z-x)))
| BinOpAppExp (PlusOp,x,y) -> BinOpAppExp (PlusOp,x,y)
| BinOpAppExp (TimesOp,x,ConstExp (IntConst 0)) -> ConstExp (IntConst 0)
| BinOpAppExp (TimesOp,x,ConstExp (IntConst 1)) -> x
| BinOpAppExp (TimesOp,(BinOpAppExp (TimesOp,x,ConstExp (IntConst y))),ConstExp (IntConst z)) -> BinOpAppExp (TimesOp,x,ConstExp (IntConst (y*z)))
| BinOpAppExp (TimesOp,ConstExp (IntConst x),(BinOpAppExp (TimesOp,ConstExp (IntConst y),z))) -> BinOpAppExp (TimesOp,z,ConstExp (IntConst (x*y)))
| BinOpAppExp (TimesOp,ConstExp (IntConst x),(BinOpAppExp (TimesOp,y,ConstExp (IntConst z)))) -> BinOpAppExp (TimesOp,y,ConstExp (IntConst (x*z)))
| BinOpAppExp (TimesOp,x,y) -> BinOpAppExp (TimesOp,x,y)
| TernExp (ConstExp (BoolConst true),x,y) -> x
| TernExp (ConstExp (BoolConst false),x,y) -> y
| TernExp (p,x,y) -> TernExp (p,x,y)
| _ -> failwith "eval_aexp_aux error")

let rec eval_aexp e =
(match e with
  ConstExp (IntConst c) -> ConstExp (IntConst c)
| VarExp v -> VarExp v(* todo: array indexing *)
| MonOpAppExp (IntNegOp,e) -> eval_aexp_aux (MonOpAppExp (IntNegOp,eval_aexp e))
| BinOpAppExp (DivOp,x,y) -> eval_aexp_aux (BinOpAppExp (DivOp,eval_aexp x,eval_aexp y))
| BinOpAppExp (MinusOp,x,y) -> eval_aexp_aux (BinOpAppExp (MinusOp,eval_aexp x,eval_aexp y))
| BinOpAppExp (PlusOp,x,y) -> eval_aexp_aux (BinOpAppExp (PlusOp,eval_aexp x,eval_aexp y))
| BinOpAppExp (TimesOp,x,y) -> eval_aexp_aux (BinOpAppExp (TimesOp,eval_aexp x,eval_aexp y))
| TernExp (p,x,y) -> eval_aexp_aux (TernExp (eval_bexp p,eval_aexp x,eval_aexp y))
| BinOpAppExp (AndOp,x,y) -> eval_bexp (BinOpAppExp (AndOp,x,y))
| BinOpAppExp (OrOp,x,y) -> eval_bexp (BinOpAppExp (OrOp,x,y))
| MonOpAppExp (BoolNegOp,e) -> eval_bexp (MonOpAppExp (BoolNegOp,e))
| ConstExp (BoolConst true) -> eval_bexp (ConstExp (BoolConst true))
| ConstExp (BoolConst false) -> eval_bexp (ConstExp (BoolConst false))
| _ -> failwith "eval_aexp error")

and eval_bexp_aux e =
(match e with
  BinOpAppExp(GreaterOp,ConstExp (IntConst x),ConstExp (IntConst y)) -> if x>y then (ConstExp (BoolConst true)) else (ConstExp (BoolConst false))
| BinOpAppExp(LessOp,ConstExp (IntConst x),ConstExp (IntConst y)) -> if x<y then (ConstExp (BoolConst true)) else (ConstExp (BoolConst false))
| BinOpAppExp(GEqOp,ConstExp (IntConst x),ConstExp (IntConst y)) -> if x>=y then (ConstExp (BoolConst true)) else (ConstExp (BoolConst false))
| BinOpAppExp(LEqOp,ConstExp (IntConst x),ConstExp (IntConst y)) -> if x<=y then (ConstExp (BoolConst true)) else (ConstExp (BoolConst false))
| BinOpAppExp(EqOp,ConstExp (IntConst x),ConstExp (IntConst y)) -> if x=y then (ConstExp (BoolConst true)) else (ConstExp (BoolConst false))
| BinOpAppExp(NEqOp,ConstExp (IntConst x),ConstExp (IntConst y)) -> if x=y then (ConstExp (BoolConst false)) else (ConstExp (BoolConst true))
| BinOpAppExp(GreaterOp,x,y) -> (BinOpAppExp(GreaterOp,x,y))
| BinOpAppExp(LessOp,x,y) -> (BinOpAppExp(LessOp,x,y))
| BinOpAppExp(GEqOp,x,y) -> (BinOpAppExp(GEqOp,x,y))
| BinOpAppExp(LEqOp,x,y) -> (BinOpAppExp(LEqOp,x,y))
| BinOpAppExp(EqOp,ConstExp (IntConst x),y) -> (BinOpAppExp(EqOp,y,ConstExp (IntConst x)))
| BinOpAppExp(EqOp,x,y) -> if x=y then (ConstExp (BoolConst true)) else (BinOpAppExp(EqOp,x,y))
| BinOpAppExp(NEqOp,x,y) -> (BinOpAppExp(NEqOp,x,y))
| BinOpAppExp(AndOp,ConstExp (BoolConst true),x) -> x
| BinOpAppExp(AndOp,x,ConstExp (BoolConst true)) -> x
| BinOpAppExp(AndOp,ConstExp (BoolConst false),x) -> ConstExp (BoolConst false)
| BinOpAppExp(AndOp,x,ConstExp (BoolConst false)) -> ConstExp (BoolConst false)
| BinOpAppExp(AndOp,x,y) -> if x=y then x else BinOpAppExp(AndOp,x,y)
| BinOpAppExp(OrOp,ConstExp (BoolConst true),x) -> ConstExp (BoolConst true)
| BinOpAppExp(OrOp,x,ConstExp (BoolConst true)) -> ConstExp (BoolConst true)
| BinOpAppExp(OrOp,ConstExp (BoolConst false),x) -> x
| BinOpAppExp(OrOp,x,ConstExp (BoolConst false)) -> x
| BinOpAppExp(OrOp,x,y) -> if x=y then x else BinOpAppExp(OrOp,x,y)
| MonOpAppExp(BoolNegOp,ConstExp (BoolConst true)) -> ConstExp (BoolConst false)
| MonOpAppExp(BoolNegOp,ConstExp (BoolConst false)) -> ConstExp (BoolConst true)
| MonOpAppExp(BoolNegOp,(BinOpAppExp (AndOp,x,y))) -> BinOpAppExp (OrOp,(eval_bexp_aux (MonOpAppExp (BoolNegOp,x))),(eval_bexp_aux (MonOpAppExp (BoolNegOp,y))))
| MonOpAppExp(BoolNegOp,(BinOpAppExp (OrOp,x,y))) -> BinOpAppExp (AndOp,(eval_bexp_aux (MonOpAppExp (BoolNegOp,x))),(eval_bexp_aux (MonOpAppExp (BoolNegOp,y))))
| MonOpAppExp(BoolNegOp,x) -> MonOpAppExp(BoolNegOp,x) (* can more be done here? *)
| ConstExp (BoolConst true) -> ConstExp (BoolConst true)
| ConstExp (BoolConst false) -> ConstExp (BoolConst false)
| _ -> failwith "not a boolean expression")

and eval_bexp e =
(match e with
  BinOpAppExp(GreaterOp,e1,e2) -> eval_bexp_aux (BinOpAppExp(GreaterOp,(eval_aexp e1),(eval_aexp e2)))
| BinOpAppExp(LEqOp,e1,e2) -> eval_bexp_aux (BinOpAppExp(LEqOp,(eval_aexp e1),(eval_aexp e2)))
| BinOpAppExp(LessOp,e1,e2) -> eval_bexp_aux (BinOpAppExp(LessOp,(eval_aexp e1),(eval_aexp e2)))
| BinOpAppExp(GEqOp,e1,e2) -> eval_bexp_aux (BinOpAppExp(GEqOp,(eval_aexp e1),(eval_aexp e2)))
| BinOpAppExp(EqOp,e1,e2) -> eval_bexp_aux (BinOpAppExp(EqOp,(eval_aexp e1),(eval_aexp e2)))
| BinOpAppExp(NEqOp,e1,e2) -> eval_bexp_aux (BinOpAppExp(NEqOp,(eval_aexp e1),(eval_aexp e2)))
| BinOpAppExp(AndOp,x,y) -> eval_bexp_aux (BinOpAppExp (AndOp,(eval_bexp x),(eval_bexp y)))
| BinOpAppExp(OrOp,x,y) -> eval_bexp_aux (BinOpAppExp (OrOp,(eval_bexp x),(eval_bexp y)))
| MonOpAppExp(BoolNegOp,e) -> eval_bexp_aux (MonOpAppExp (BoolNegOp,(eval_bexp e)))
| ConstExp (BoolConst true) -> ConstExp (BoolConst true)
| ConstExp (BoolConst false) -> ConstExp (BoolConst false)
| _ -> failwith "not a boolean expression")

(* to make types check *)
and pred_to_exp p =
(match p with
  True -> ConstExp (BoolConst true)
| False -> ConstExp (BoolConst false)
| Exp_pred e -> e
| And (p1,p2) -> BinOpAppExp (AndOp,(pred_to_exp p1),(pred_to_exp p2))
| Not p1 -> MonOpAppExp (BoolNegOp,(pred_to_exp p1))
| Or (p1,p2) -> BinOpAppExp (OrOp,(pred_to_exp p1),(pred_to_exp p2))
| Implies (p1,p2) -> MonOpAppExp (BoolNegOp,(BinOpAppExp (AndOp,(pred_to_exp p1),(MonOpAppExp (BoolNegOp,(pred_to_exp p2)))))))

let rec negate_bexp e =
(match e with
  BinOpAppExp(GreaterOp,e1,e2) -> BinOpAppExp(LEqOp,e1,e2)
| BinOpAppExp(LEqOp,e1,e2) -> BinOpAppExp(GreaterOp,e1,e2)
| BinOpAppExp(LessOp,e1,e2) -> BinOpAppExp(GEqOp,e1,e2)
| BinOpAppExp(GEqOp,e1,e2) -> BinOpAppExp(LessOp,e1,e2)
| BinOpAppExp(EqOp,e1,e2) -> BinOpAppExp(NEqOp,e1,e2)
| BinOpAppExp(NEqOp,e1,e2) -> BinOpAppExp(EqOp,e1,e2)
| BinOpAppExp(AndOp,e1,e2) -> MonOpAppExp (BoolNegOp,(BinOpAppExp(AndOp,e1,e2))) (* might need to fix this *)
| BinOpAppExp(OrOp,e1,e2) -> MonOpAppExp (BoolNegOp,(BinOpAppExp(OrOp,e1,e2))) (* ditto *)
| MonOpAppExp(BoolNegOp,e) -> e
| ConstExp (BoolConst true) -> ConstExp (BoolConst false)
| ConstExp (BoolConst false) -> ConstExp (BoolConst true)
| _ -> failwith "can't negate nonboolean expressions")

let rec eval_pred_aux p = 
(match p with
  True -> True
| False -> False
| And (True,x) -> x
| And (x,True) -> x
| And (False,x) -> False
| And (x,False) -> False
| And (x,y) -> if (x = y) then x else (And (x,y))
| Or (True,x) -> True
| Or (x,True) -> True
| Or (False,x) -> x
| Or (x,False) -> x
| Or (x,y) -> if (x=y) then x else (Or (x,y))
| Implies (x,True) -> True
| Implies (x,False) -> Not x 
| Implies (True,x) -> x
| Implies (False,x) -> True
| Implies (x,y) -> Implies (x,y)
| Not (True) -> False
| Not (False) -> True
| Not (Not(x)) -> x
| Not (And(x,y)) -> Or(eval_pred_aux (Not x), eval_pred_aux (Not y))
| Not (Or(x,y)) -> And(eval_pred_aux (Not x), eval_pred_aux (Not y))
| Not (Implies(x,y)) -> And(x,Not(eval_pred_aux y))
| Not (Exp_pred x) -> Exp_pred (negate_bexp x)
| Exp_pred (ConstExp (BoolConst true)) -> True
| Exp_pred (ConstExp (BoolConst false)) -> False
| Exp_pred x -> Exp_pred (eval_bexp x))

let rec eval_pred p =
(match p with 
  True -> True
| False -> False
| Exp_pred e -> eval_pred_aux (Exp_pred e)
| And (p1,p2) -> eval_pred_aux (And ((eval_pred p1),(eval_pred p2)))
| Not p -> eval_pred_aux (Not (eval_pred p))
| Or (p1,p2) -> eval_pred_aux (Or ((eval_pred p1),(eval_pred p2)))
| Implies (p1,p2) -> eval_pred_aux (Implies ((eval_pred p1),(eval_pred p2))))

(* perform evaluation/simplification until result "settles" *)
let rec shake_eval_pred p = 
let pass_one = eval_pred p in 
let pass_two = eval_pred pass_one in
if (pass_one = pass_two) then pass_one else shake_eval_pred pass_two

let rec string_of_pred p =
(match p with
  True -> "True"
| False -> "False"
| And (p1,p2) -> "And (" ^ (string_of_pred p1) ^ "," ^ (string_of_pred p2) ^ ")"
| Or (p1,p2) -> "Or (" ^ (string_of_pred p1) ^ "," ^ (string_of_pred p2) ^ ")"
| Implies (p1,p2) -> "Implies (" ^ (string_of_pred p1) ^ "," ^ (string_of_pred p2) ^ ")"
| Not p1 -> "Not (" ^ (string_of_pred p1) ^ ")"
| Exp_pred e -> string_of_exp e)

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
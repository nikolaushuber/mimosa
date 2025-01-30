(* type value =
  | VUndef
  | VUnit
  | VInt of int
  | VBool of bool
  | VFloat of float
  | VTuple of value list
  | VOption of value option
  | VLambda of pat * pat * equation list

and extern_func = value -> value

and expr =
  | EUnit
  | EInt of int
  | EBool of bool
  | EFloat of float
  | EVar of string
  | EUnOp of unop * expr
  | EBinOp of binop * expr * expr
  | EEither of expr * expr
  | ETuple of expr list
  | EIf of expr * expr * expr
  | EApp of expr * expr
  | EArrow of expr * expr
  | EFby of expr * expr
  | EPre of expr
  | ENone
  | ESome of expr
  | ELam of pat * pat * equation list
  | EExtern of extern_func * expr

and unop = Not | Neg | IsSome
and binop = And | Or | Implies | Add | Sub | Mul | Div | Eq | Neq | Gt | Geq | Lt | Leq

and pat = PUnit | PAny | PVar of string | PTuple of pat list

and equation = pat * expr

let rec eval env = function
  | EUnit -> VUnit
  | EInt i -> VInt i
  | EBool b -> VBool b
  | EFloat f -> VFloat f
  | EVar v -> List.assoc v env
  | EUnOp (op, e) -> eval_unop op (eval env e)
  | EBinOp (op, e1, e2) -> eval_binop op (eval env e1) (eval env e2)
  | EEither (e1, e2) -> (match eval env e1 with
    | VOption (Some v) -> v
    | VOption None -> eval env e2
    | _ -> assert false
  )
  | ETuple es -> VTuple (List.map (eval env) es)
  | EIf (c, t, e) -> (match eval env c with
    | VBool true -> eval env t
    | VBool false -> eval env e
    | _ -> assert false
  )
  | EArrow (e1, _) -> eval env e1
  | EFby (e1, _) -> eval env e1
  | EPre _ -> VUndef
  | ENone -> VOption None
  | ESome e -> VOption (Some (eval env e))
  | ELam (p1, p2, b) -> VLambda (p1, p2, b)
  | EApp (e1, e2) -> (match eval env e1 with
    | VLambda (p1, p2, b) ->
      let arg = eval env e2 in
      let env = env_bind [] p1 arg in
      let env' = eval_eqs env b in
      env_proj env' p2
    | _ -> assert false
  )
  | EExtern (func, e) -> func (eval env e)

and eval_unop op v = match op, v with
  | Not, VBool b -> VBool (Bool.not b)
  | Neg, VInt i -> VInt (- i)
  | Neg, VFloat f -> VFloat (-. f)
  | IsSome, VOption (Some _) -> VBool true
  | IsSome, VOption None -> VBool false
  | _ -> assert false

and eval_binop op v1 v2 = match op, v1, v2 with
  | And, VBool b1, VBool b2 -> VBool (b1 && b2)
  | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
  | Implies, VBool b1, VBool b2 -> VBool ((Bool.not b1) || b2)
  | Add, VInt i1, VInt i2 -> VInt (i1 + i2)
  | Sub, VInt i1, VInt i2 -> VInt (i1 - i2)
  | Mul, VInt i1, VInt i2 -> VInt (i1 * i2)
  | Div, VInt i1, VInt i2 -> VInt (i1 / i2)
  | Add, VFloat f1, VFloat f2 -> VFloat (f1 +. f2)
  | Sub, VFloat f1, VFloat f2 -> VFloat (f1 -. f2)
  | Mul, VFloat f1, VFloat f2 -> VFloat (f1 *. f2)
  | Div, VFloat f1, VFloat f2 -> VFloat (f1 /. f2)
  | Eq, e1, e2 -> VBool (e1 = e2)
  | Neq, e1, e2 -> VBool (e1 <> e2)
  | Gt, e1, e2 -> VBool (e1 > e2)
  | Geq, e1, e2 -> VBool (e1 >= e2)
  | Lt, e1, e2 -> VBool (e1 < e2)
  | Leq, e1, e2 -> VBool (e1 <= e2)
  | _ -> assert false

and env_bind env p v = match p, v with
  | PAny, _ -> env
  | PUnit, VUnit -> env
  | PVar x, _ -> (x, v) :: env
  | PTuple ps, VTuple vs ->
    assert (List.length ps = List.length vs);
    List.fold_left2 env_bind env ps vs 
  | _ -> assert false

and eval_eqs env eqs =
  let aux env p e =
    

and env_proj env = function
  | PVar x -> List.assoc x env
  | PUnit -> VUnit
  | PAny -> VUndef
  | PTuple ps -> VTuple (List.map (env_proj env) ps) *)

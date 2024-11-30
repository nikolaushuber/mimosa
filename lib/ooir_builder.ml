open Ooir

let assign name e = Assign (name, e)
let state_assign name e = StateAssign (name, e)
let tuple_constr name es = TupleConstr (name, es)
let tuple_destr names e = TupleDestr (names, e)
let reset func self = Reset (func, self)
let return expr = Return expr
let if_ c t e = If (c, t, e)
let step_app ret f args self = StepApp (ret, f, args, self)
let either e1 e2 = Either (e1, e2)
let expr expr_desc expr_ty = { expr_desc; expr_ty }
let evar id ty = expr (Var id) ty
let estate_var name ty = expr (StateVar name) ty
let econst c ty = expr (Const c) ty
let eint i = econst (Ttree.CInt i) TInt
let ebool b = econst (Ttree.CBool b) TBool
let ereal r = econst (Ttree.CReal r) TReal
let eglobal_const id ty = expr (GlobalConst id) ty
let enone ty = expr None ty
let esome e ty = expr (Some e) ty
let eunop op e ty = expr (UnOp (op, e)) ty
let ebinop op e1 e2 ty = expr (BinOp (op, e1, e2)) ty
let param name ty = (name, ty)

let machine name inputs locals memory instances reset def =
  Machine { name; inputs; locals; memory; instances; reset; def }

let package name items = Package (name, items)

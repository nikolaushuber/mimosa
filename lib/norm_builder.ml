open Norm
open Type

let pattern pat_desc pat_ty = Ttree.{ pat_desc; pat_ty }
let pany = pattern PAny
let punit = pattern PUnit TUnit
let pvar name = pattern (PVar name)
let ptuple pl = pattern (PTuple pl)
let base base_expr_desc base_expr_ty = { base_expr_desc; base_expr_ty }
let base_const c ty = base (EConst c) ty
let base_var v ty = base (EVar v) ty
let base_global_var v ty = base (EGlobalVar v) ty
let base_unop op e ty = base (EUnOp (op, e)) ty
let base_binop op e1 e2 ty = base (EBinOp (op, e1, e2)) ty
let base_none ty = base ENone ty
let base_some e ty = base (ESome e) ty
let expr expr_desc expr_ty = { expr_desc; expr_ty }
let base_expr be = expr (EBase be) be.base_expr_ty
let econst c ty = expr (EBase (base_const c ty)) ty
let evar id ty = expr (EBase (base_var id ty)) ty
let eglobalvar id ty = expr (EBase (base_global_var id ty)) ty
let eunop op e ty = expr (EBase (base_unop op e ty)) ty
let ebinop op e1 e2 ty = expr (EBase (base_binop op e1 e2 ty)) ty
let eeither e1 e2 = expr (EEither (e1, e2))
let etuple es ty = expr (ETuple es) ty
let eif c t e = expr (EIf (c, t, e))
let eapp f args = expr (EApp (f, args))
let efby e1 e2 = expr (EFby (e1, e2))
let enone ty = expr (EBase (base_none ty)) ty
let esome e ty = expr (EBase (base_some e ty)) ty
let block vb e : block = (vb, e)
let step name inp ret_ty e state = (name, inp, ret_ty, e, state)
let proto name inp ret_ty = (name, inp, ret_ty)

let package pack_name pack_protos pack_steps =
  { pack_name; pack_protos; pack_steps }

open Ttree
open Type

let mk_pat pat_desc pat_ty = { pat_desc; pat_ty }
let pany ty = mk_pat PAny ty
let punit = mk_pat PAny TUnit
let pvar var ty = mk_pat (PVar var) ty

let ptuple pats =
  let tys = List.map (fun p -> p.pat_ty) pats in
  mk_pat (PTuple pats) (TTuple tys)

let mk_expr expr_desc expr_ty = { expr_desc; expr_ty }
let evar id ty = mk_expr (EVar id) ty
let eint i = mk_expr (EConst (CInt i)) TInt
let efloat f = mk_expr (EConst (CFloat f)) TFloat
let ebool b = mk_expr (EConst (CBool b)) TBool
let eunit () = mk_expr (EConst CUnit) TUnit
let eunop op e ty = mk_expr (EUnOp (op, e)) ty
let ebinop op e1 e2 ty = mk_expr (EBinOp (op, e1, e2)) ty

let eeither e1 e2 =
  let ty = e2.expr_ty in
  mk_expr (EEither (e1, e2)) ty

let etuple es =
  let tys = List.map (fun e -> e.expr_ty) es in
  mk_expr (ETuple es) (TTuple tys)

let eif c t e =
  let ty = t.expr_ty in
  mk_expr (EIf (c, t, e)) ty

let eapply f e ty = mk_expr (EApp (f, e)) ty
let ematch _ _ = failwith "not yet implemented"

let earrow e1 e2 =
  let ty = e2.expr_ty in
  mk_expr (EArrow (e1, e2)) ty

let efby e1 e2 =
  let ty = e2.expr_ty in
  mk_expr (EFby (e1, e2)) ty

let epre e =
  let ty = e.expr_ty in
  mk_expr (EPre e) ty

let enone ty = mk_expr ENone ty

let esome e =
  let ty = e.expr_ty in
  mk_expr (ESome e) (TOption ty)

let step step_name step_input step_output step_def =
  { step_name; step_input; step_output; step_def }

let proto proto_name proto_input proto_output =
  { proto_name; proto_input; proto_output }

let channel channel_name channel_ty channel_elems =
  { channel_name; channel_ty; channel_elems }

let port port_name port_opt = { port_name; port_opt }

let node node_name node_implements node_inputs node_outputs node_period =
  { node_name; node_implements; node_inputs; node_outputs; node_period }

let package protos steps channels nodes = { protos; steps; channels; nodes }

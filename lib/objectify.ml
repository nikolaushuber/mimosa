open Norm
open Ooir_builder

let counter = ref 0

let new_var ?(prefix = "tmp") _ =
  let id = !counter in
  incr counter;
  Format.asprintf "%s_%d" prefix id

let trans_const c =
  let open Ttree in
  let open Type in
  match c with
  | CUnit -> econst c TUnit
  | CInt _ -> econst c TInt
  | CBool _ -> econst c TBool
  | CReal _ -> econst c TReal

let trans_expr m e =
  match e.base_expr_desc with
  | EConst c -> trans_const c
  | EGlobalVar id -> eglobal_const id e.base_expr_ty
  | EVar v when List.mem_assoc v m -> estate_var v e.base_expr_ty
  | EVar v -> evar v e.base_expr_ty
  | EUnOp (op, e') -> eunop op e' e.base_expr_ty
  | EBinOp (op, e1, e2) -> ebinop op e1 e2 e.base_expr_ty
  | ENone -> enone e.base_expr_ty
  | ESome e' -> esome e' e.base_expr_ty

let rec trans_eq (m, l, si, j, s) (lhs, rhs) =
  match (lhs.pat_desc, rhs.expr_desc) with
  | PVar v, ETuple es ->
      (m, (v, lhs.pat_ty) :: l, si, j, s @ [ tuple_constr v es ])
  | PVar v, EIf (c, t, e) ->
      let eqs1, e1 = t in
      let m', l', si', j', s_t =
        List.fold_left trans_eq (m, l, si, j, []) eqs1
      in
      let t' = s_t @ [ assign v (trans_expr m' e1) ] in
      let eqs2, e2 = e in
      let m'', l'', si'', j'', s_e =
        List.fold_left trans_eq (m', l', si', j', []) eqs2
      in
      let e' = s_e @ [ assign v (trans_expr m'' e2) ] in
      let if_instr = if_ c t' e' in
      (m'', (v, lhs.pat_ty) :: l'', si'', j'', s @ [ if_instr ])
  | PVar v, EFby (e1, (eqs, e2)) ->
      let first = new_var ~prefix:"first" () in
      let m', l', si', j', s_e2 =
        List.fold_left trans_eq (m, l, si, j, [])
          (eqs @ [ (lhs, Norm_builder.base_expr e2) ])
      in
      let if_instr = if_ first [ assign v (evar e1 rhs.expr_ty) ] s_e2 in
      ( (first, Type.TBool) :: m',
        l',
        state_assign first (ebool true) :: si',
        j',
        s @ [ if_instr; state_assign first (ebool false) ] )
  | ((PVar _ | PUnit | PAny) as eq_lhs), EApp (f, a) ->
      let o = new_var ~prefix:"X" () in
      let lhs =
        match eq_lhs with
        | PVar v -> Some v
        | PUnit | PAny -> None
        | PTuple _ -> assert false
      in
      let s' = s @ [ step_app lhs f [ a ] o ] in
      (m, l, reset f o :: si, (o, f) :: j, s')
  | PTuple ps, ETuple es ->
      let tys =
        match lhs.pat_ty with
        | TTuple tys -> tys
        | _ -> assert false
      in
      assert (List.length ps = List.length es);
      let eqs =
        List.map2
          (fun (p, e) ty -> assign p (evar e ty))
          (List.combine ps es) tys
      in
      (m, List.combine ps tys @ l, si, j, s @ eqs)
  | PTuple ps, _ ->
      let var = new_var () in
      let m', l', si', j', s' =
        trans_eq (m, l, si, j, s) (Norm_builder.pvar var lhs.pat_ty, rhs)
      in
      (m', l', si', j', s' @ [ tuple_destr ps var ])
  | PVar v, EBase e ->
      (m, (v, lhs.pat_ty) :: l, si, j, s @ [ assign v (trans_expr m e) ])
  | _ -> assert false

let trans_param pat =
  match pat.pat_desc with
  | PAny | PUnit ->
      let var = new_var ~prefix:"unused" () in
      [ param var pat.pat_ty ]
  | PVar v -> [ param v pat.pat_ty ]
  | PTuple vs ->
      let tys =
        match pat.pat_ty with
        | TTuple tys -> tys
        | _ -> assert false
      in
      List.map2 param vs tys

let trans_step (name, input, ret_ty, def, state) =
  counter := state;
  let eqs, ret = def in
  let m, l, si, j, s = List.fold_left trans_eq ([], [], [], [], []) eqs in
  let ret_var = new_var ~prefix:"return_val" () in
  let inputs = trans_param input in
  let self = new_var ~prefix:"self" () in
  let s' = s @ [ assign ret_var (trans_expr m ret); return ret_var ] in
  machine name inputs ((ret_var, ret_ty) :: l) m j si ret_ty self s'

let trans_item = function
  | Step s -> trans_step s

let f (Package (name, items)) =
  let items' = List.map trans_item items in
  package name items'

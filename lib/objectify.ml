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
  | CFloat _ -> econst c TFloat

let trans_expr m e =
  match e.base_expr_desc with
  | EConst c -> trans_const c
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
      let tmp = new_var () in
      let m', l', si', j', s_e2 =
        List.fold_left trans_eq (m, l, si, j, [])
          (eqs @ [ (lhs, Norm_builder.base_expr e2) ])
      in
      let cond_instr = assign tmp (estate_var first TBool) in
      let if_instr = if_ tmp [ assign v (evar e1 rhs.expr_ty) ] s_e2 in
      ( (first, Type.TBool) :: m',
        (tmp, TBool) :: l',
        state_assign first (ebool true) :: si',
        j',
        s @ [ cond_instr; if_instr; state_assign first (ebool false) ] )
  | PVar v, EEither (e1, (eqs, e2)) ->
      let m', l', si', j', s_e2 =
        List.fold_left trans_eq (m, l, si, j, [])
          (eqs @ [ (lhs, Norm_builder.base_expr e2) ])
      in
      (m', l', si', j', s @ [ either v e1 s_e2 ])
  | ((PVar _ | PUnit | PAny) as eq_lhs), EApp (f, a) ->
      let o = new_var ~prefix:"x" () in
      let lhs, l' =
        match eq_lhs with
        | PVar v -> (Some v, (v, lhs.pat_ty) :: l)
        | PUnit | PAny -> (None, l)
        | PTuple _ -> assert false
      in
      let s' = s @ [ step_app lhs f [ a ] o ] in
      (m, l', reset f o :: si, (o, f) :: j, s')
  | PTuple ps, ETuple es ->
      let tys = List.map (fun p -> p.pat_ty) ps in
      let eqs = List.map2 Norm_builder.evar es tys in
      List.fold_left trans_eq (m, l, si, j, s) (List.combine ps eqs)
  | PTuple ps, _ ->
      let var = new_var () in
      let tys = List.map (fun p -> p.pat_ty) ps in
      let m', l', si', j', s' =
        trans_eq (m, l, si, j, s) (Norm_builder.pvar var lhs.pat_ty, rhs)
      in
      let eqs, lhs' =
        List.fold_left_map
          (fun acc p ->
            match p.pat_desc with
            | PAny | PUnit -> (acc, "unused")
            | PVar v -> (acc, v)
            | PTuple _ ->
                let tmp = new_var () in
                (acc @ [ (p, Norm_builder.evar tmp p.pat_ty) ], tmp))
          [] ps
      in
      let acc' =
        (m', List.combine lhs' tys @ l', si', j', s' @ [ tuple_destr lhs' var ])
      in
      List.fold_left trans_eq acc' eqs
  | PVar v, EBase e ->
      (m, (v, lhs.pat_ty) :: l, si, j, s @ [ assign v (trans_expr m e) ])
  | _ -> assert false

let trans_step (name, input, ret_ty, def, state) =
  counter := state;
  let eqs, ret = def in
  let m, l, si, j, s = List.fold_left trans_eq ([], [], [], [], []) eqs in
  let ret_var = new_var ~prefix:"return_val" () in
  let self = new_var ~prefix:"self" () in
  let s' = s @ [ assign ret_var (trans_expr m ret); return ret_var ] in
  machine name input ((ret_var, ret_ty) :: l) m j si ret_ty self s'

let trans_proto (name, input, ret_ty) = proto name input.pat_ty ret_ty
let f p = package (List.map trans_proto p.protos) (List.map trans_step p.steps)

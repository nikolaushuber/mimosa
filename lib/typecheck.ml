open Ptree
open Type
open Reserr
open Ttree_builder

let lookup_id genv lenv id state =
  let lookup_name env name state =
    match Env.find_opt name env with
    | Some s ->
        let t, state' = Scheme.instantiate s state in
        (Subst.empty, t, state') |> ok
    | None ->
        let err = Error.Unbound_value name in
        let loc = id.Location.loc in
        error (err, loc)
  in
  match id.Location.txt with
  | Lident.Lident name -> lookup_name lenv name state
  | Ldot (pack, name) -> (
      match String.Map.find_opt pack genv with
      | Some env -> lookup_name env name state
      | None ->
          let err = Error.Unbound_package pack in
          let loc = id.loc in
          error (err, loc))

let rec ty_expr genv lenv expr state =
  let* s, ty, e', state' =
    match expr.pexpr_desc with
    | Pexpr_ident id -> ty_ident genv lenv id state
    | Pexpr_constant c -> ty_const c state
    | Pexpr_unop (op, e) -> ty_unop genv lenv op e state
    | Pexpr_binop (op, e1, e2) -> ty_binop genv lenv op e1 e2 state
    | Pexpr_either (e1, e2) -> ty_either genv lenv e1 e2 state
    | Pexpr_tuple es -> ty_tuple genv lenv es state
    | Pexpr_ite (c, t, e) -> ty_ite genv lenv c t e state
    | Pexpr_apply (f, e) -> ty_apply genv lenv f e state
    | Pexpr_arrow (e1, e2) -> ty_arrow genv lenv e1 e2 state
    | Pexpr_fby (e1, e2) -> ty_fby genv lenv e1 e2 state
    | Pexpr_pre e -> ty_pre genv lenv e state
    | Pexpr_match _ -> failwith "not yet implemented"
    | Pexpr_none -> ty_none state
    | Pexpr_some e -> ty_some genv lenv e state
  in
  (s, ty, e', state') |> ok

and ty_ident genv lenv id state =
  let* s, ty, state' = lookup_id genv lenv id state in
  (s, ty, evar id.Location.txt ty, state') |> ok

and ty_const c state =
  match c with
  | Pconst_int i -> (Subst.empty, TInt, eint i, state) |> ok
  | Pconst_real r -> (Subst.empty, TReal, ereal r, state) |> ok
  | Pconst_bool b -> (Subst.empty, TBool, ebool b, state) |> ok
  | Pconst_unit -> (Subst.empty, TUnit, eunit (), state) |> ok

and ty_unop genv lenv op e state =
  let* s1, ty, e', state' = ty_expr genv lenv e state in
  match op.punop_desc with
  | Punop_not ->
      let* s2 = unify ~loc:e.pexpr_loc TBool ty in
      (Subst.compose s2 s1, TBool, eunop Ttree.Not e' TBool, state') |> ok
  | Punop_neg ->
      let* s2 = unify ~loc:e.pexpr_loc TInt ty in
      (Subst.compose s2 s1, TInt, eunop Ttree.Neg e' TInt, state') |> ok
  | Punop_rneg ->
      let* s2 = unify ~loc:e.pexpr_loc TInt ty in
      (Subst.compose s2 s1, TReal, eunop Ttree.RNeg e' TReal, state') |> ok
  | Punop_is_some ->
      let n, state'' = State.next state' in
      let* s2 = unify ~loc:e.pexpr_loc (TOption (TVar n)) ty in
      ( Subst.compose s2 s1,
        TOption ty,
        eunop Ttree.IsSome e' (TOption ty),
        state'' )
      |> ok

and ty_binop genv lenv op e1 e2 state =
  let trans_op op =
    let open Ttree in
    match op.pbinop_desc with
    | Pbinop_and -> And
    | Pbinop_or -> Or
    | Pbinop_implies -> Implies
    | Pbinop_add -> Add
    | Pbinop_sub -> Sub
    | Pbinop_mul -> Mul
    | Pbinop_div -> Div
    | Pbinop_radd -> RAdd
    | Pbinop_rsub -> RSub
    | Pbinop_rmul -> RMul
    | Pbinop_rdiv -> RDiv
    | Pbinop_eq -> Eq
    | Pbinop_neq -> Neq
    | Pbinop_lt -> Lt
    | Pbinop_leq -> Leq
    | Pbinop_gt -> Gt
    | Pbinop_geq -> Geq
    | Pbinop_rlt -> RLt
    | Pbinop_rleq -> RLeq
    | Pbinop_rgt -> RGt
    | Pbinop_rgeq -> RGeq
  in
  let* s1, t1, e1', state' = ty_expr genv lenv e1 state in
  let* s2, t2, e2', state'' = ty_expr genv (Env.apply s1 lenv) e2 state' in
  match op.pbinop_desc with
  | Pbinop_and | Pbinop_or | Pbinop_implies ->
      let* s3 = unify ~loc:e1.pexpr_loc TBool t1
      and* s4 = unify ~loc:e2.pexpr_loc TBool t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s4; s3; s2; s1 ], TBool, e', state'') |> ok
  | Pbinop_add | Pbinop_sub | Pbinop_mul | Pbinop_div ->
      let* s3 = unify ~loc:e1.pexpr_loc TInt t1
      and* s4 = unify ~loc:e2.pexpr_loc TInt t2 in
      let e' = ebinop (trans_op op) e1' e2' TInt in
      (Subst.compose_list [ s4; s3; s2; s1 ], TInt, e', state'') |> ok
  | Pbinop_radd | Pbinop_rsub | Pbinop_rmul | Pbinop_rdiv ->
      let* s3 = unify ~loc:e1.pexpr_loc TReal t1
      and* s4 = unify ~loc:e2.pexpr_loc TReal t2 in
      let e' = ebinop (trans_op op) e1' e2' TReal in
      (Subst.compose_list [ s4; s3; s2; s1 ], TReal, e', state'') |> ok
  | Pbinop_eq | Pbinop_neq ->
      let* s3 = unify ~loc:e1.pexpr_loc t1 t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s3; s2; s1 ], TBool, e', state'') |> ok
  | Pbinop_lt | Pbinop_leq | Pbinop_gt | Pbinop_geq ->
      let* s3 = unify ~loc:e1.pexpr_loc TInt t1
      and* s4 = unify ~loc:e2.pexpr_loc TInt t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s4; s3; s2; s1 ], TBool, e', state'') |> ok
  | Pbinop_rlt | Pbinop_rleq | Pbinop_rgt | Pbinop_rgeq ->
      let* s3 = unify ~loc:e1.pexpr_loc TReal t1
      and* s4 = unify ~loc:e2.pexpr_loc TReal t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s4; s3; s2; s1 ], TBool, e', state'') |> ok

and ty_either genv lenv e1 e2 state =
  let* s1, ty1, e1', state' = ty_expr genv lenv e1 state in
  let* s2, ty2, e2', state'' = ty_expr genv (Env.apply s1 lenv) e2 state' in
  let n, state''' = State.next state'' in
  let* s3 = unify ~loc:e1.pexpr_loc (TOption (TVar n)) ty1 in
  let* s4 = unify ~loc:e2.pexpr_loc (apply s3 (TVar n)) ty2 in
  let s = Subst.compose_list [ s4; s3; s2; s1 ] in
  let ty = apply s (TVar n) in
  let e' = eeither e1' e2' in
  (s, ty, e', state''') |> ok

and ty_tuple genv lenv es state =
  let* s, tys, es', _, state' =
    fold_left
      (fun (sacc, tys, es, lenv, state) e ->
        let* s, ty, e', state' = ty_expr genv lenv e state in
        (Subst.compose s sacc, ty :: tys, e' :: es, Env.apply s lenv, state')
        |> ok)
      (Subst.empty, [], [], lenv, state)
      es
  in
  let tys = List.rev_map (apply s) tys in
  let e' = etuple es' in
  (s, TTuple tys, e', state') |> ok

and ty_ite genv lenv c t e state =
  let* s1, tc, c', state' = ty_expr genv lenv c state in
  let lenv' = Env.apply s1 lenv in
  let* s2, tt, t', state'' = ty_expr genv lenv' t state' in
  let lenv'' = Env.apply s2 lenv' in
  let* s3, te, e', state''' = ty_expr genv lenv'' e state'' in
  let* s4 = unify ~loc:c.pexpr_loc TBool tc in
  let* s5 = unify ~loc:e.pexpr_loc tt te in
  let s = Subst.compose_list [ s5; s4; s3; s2; s1 ] in
  let exp' = eif c' t' e' in
  (s, apply s tt, exp', state''') |> ok

and ty_apply genv lenv f e state =
  let* s1, ty_f, f', state' = ty_expr genv lenv f state in
  let* s2, ty_e, e', state'' = ty_expr genv (Env.apply s1 lenv) e state' in
  let n, state''' = State.next state'' in
  let* s3 = unify ~loc:e.pexpr_loc (apply s2 ty_f) (TFunc (ty_e, TVar n)) in
  let ty = apply s3 (TVar n) in
  let exp' = eapply f' e' ty in
  (Subst.compose_list [ s3; s2; s1 ], ty, exp', state''') |> ok

and ty_pre genv lenv e state =
  let* s, ty, e', state' = ty_expr genv lenv e state in
  let e'' = epre e' in
  (s, ty, e'', state') |> ok

and ty_arrow genv lenv e1 e2 state =
  let* s1, ty1, e1', state' = ty_expr genv lenv e1 state in
  let* s2, ty2, e2', state'' = ty_expr genv (Env.apply s1 lenv) e2 state' in
  let* s3 = unify ~loc:e2.pexpr_loc ty1 ty2 in
  let s = Subst.compose_list [ s3; s2; s1 ] in
  let ty = apply s ty2 in
  let e' = earrow e1' e2' in
  (s, ty, e', state'') |> ok

and ty_fby genv lenv e1 e2 state =
  let* s1, ty1, e1', state' = ty_expr genv lenv e1 state in
  let* s2, ty2, e2', state'' = ty_expr genv (Env.apply s1 lenv) e2 state' in
  let* s3 = unify ~loc:e2.pexpr_loc ty1 ty2 in
  let s = Subst.compose_list [ s3; s2; s1 ] in
  let ty = apply s ty2 in
  let e' = efby e1' e2' in
  (s, ty, e', state'') |> ok

and ty_none state =
  let n, state' = State.next state in
  let ty = TOption (TVar n) in
  (Subst.empty, ty, enone ty, state') |> ok

and ty_some genv lenv e state =
  let* s, ty, e', state' = ty_expr genv lenv e state in
  let exp' = esome e' in
  (s, TOption ty, exp', state') |> ok

let rec ty_core_type lenv map c state =
  match c.ptype_desc with
  | Ptype_var s -> (
      match String.Map.find_opt s map with
      | Some t -> (map, t, state)
      | None ->
          let n, state' = State.next state in
          (String.Map.add s (TVar n) map, TVar n, state'))
  | Ptype_tuple tys ->
      let map', tys, state' =
        List.fold_left
          (fun (map, tys, state) ty ->
            let map', ty, state' = ty_core_type lenv map ty state in
            (map', ty :: tys, state'))
          (map, [], state) tys
      in
      (map', TTuple (List.rev tys), state')
  | Ptype_option t ->
      let map', ty, state' = ty_core_type lenv map t state in
      (map', TOption ty, state')
  | Ptype_int -> (map, TInt, state)
  | Ptype_bool -> (map, TBool, state)
  | Ptype_real -> (map, TReal, state)

let rec ty_pattern lenv map p state =
  let get_ty p state =
    match p.ppat_ty with
    | Some t -> ty_core_type lenv map t state
    | None ->
        let n, state' = State.next state in
        (map, TVar n, state')
  in
  match p.ppat_desc with
  | Ppat_any ->
      let map', ty, state' = get_ty p state in
      (lenv, map', ty, pany ty, state')
  | Ppat_unit -> (lenv, map, TUnit, punit, state)
  | Ppat_var v ->
      let ty, lenv', state' =
        match Env.find_opt v.txt lenv with
        | Some s ->
            let ty, state' = Scheme.instantiate s state in
            (ty, lenv, state')
        | None ->
            let n, state' = State.next state in
            let ty = TVar n in
            let lenv' = Env.add lenv v.txt (Int.Set.empty, ty) in
            (ty, lenv', state')
      in
      (lenv', map, ty, pvar v.txt ty, state')
  | Ppat_tuple ps ->
      let lenv', map', tys, pats, state' =
        List.fold_left
          (fun (lenv, map, tys, pats, state) p ->
            let lenv', map', ty, p', state' = ty_pattern lenv map p state in
            (lenv', map', ty :: tys, p' :: pats, state'))
          (lenv, map, [], [], state) ps
      in
      (lenv', map', TTuple (List.rev tys), ptuple (List.rev pats), state')

let ty_eq genv lenv map eq state =
  let lhs, rhs = eq in
  let* s1, rhs_ty, rhs', state' = ty_expr genv lenv rhs state in
  let lenv', map', lhs_ty, lhs', state'' =
    ty_pattern (Env.apply s1 lenv) map lhs state'
  in
  let* s2 = unify ~loc:rhs.pexpr_loc lhs_ty rhs_ty in
  let eq' = (lhs', rhs') in
  let s = Subst.compose s2 s1 in
  (s, Env.apply s2 lenv', map', eq', state'') |> ok

let rec apply_subst_expr s expr =
  let open Ttree in
  let expr_desc =
    match expr.expr_desc with
    | EVar id -> EVar id
    | EConst c -> EConst c
    | EUnOp (op, e) -> EUnOp (op, apply_subst_expr s e)
    | EBinOp (op, e1, e2) ->
        EBinOp (op, apply_subst_expr s e1, apply_subst_expr s e2)
    | EEither (e1, e2) -> EEither (apply_subst_expr s e1, apply_subst_expr s e2)
    | ETuple es -> ETuple (List.map (apply_subst_expr s) es)
    | EIf (c, t, e) ->
        EIf (apply_subst_expr s c, apply_subst_expr s t, apply_subst_expr s e)
    | EApp (e1, e2) -> EApp (apply_subst_expr s e1, apply_subst_expr s e2)
    | EMatch _ -> failwith "not yet implemented"
    | EArrow (e1, e2) -> EArrow (apply_subst_expr s e1, apply_subst_expr s e2)
    | EFby (e1, e2) -> EFby (apply_subst_expr s e1, apply_subst_expr s e2)
    | EPre e -> EPre (apply_subst_expr s e)
    | ENone -> ENone
    | ESome e -> ESome (apply_subst_expr s e)
  in
  let expr_ty = apply s expr.expr_ty in
  { expr_desc; expr_ty }

let rec apply_subst_pat s pat =
  let open Ttree in
  let pat_desc =
    match pat.pat_desc with
    | PAny -> PAny
    | PUnit -> PUnit
    | PVar s -> PVar s
    | PTuple ps -> PTuple (List.map (apply_subst_pat s) ps)
  in
  let pat_ty = apply s pat.pat_ty in
  { pat_desc; pat_ty }

let apply_subst_eq s (lhs, rhs) = (apply_subst_pat s lhs, apply_subst_expr s rhs)

let ty_step genv lenv step =
  let lenv', map, ty1, p1, state' =
    ty_pattern lenv String.Map.empty step.pstep_input (State.init ())
  in
  let lenv'', map', ty2, p2, state'' =
    ty_pattern lenv' map step.pstep_output state'
  in

  let rec aux lenv map state = function
    | [] -> (Subst.empty, []) |> ok
    | eq :: eqs ->
        let* s, lenv', map', eq', state' = ty_eq genv lenv map eq state in
        let* sr, eqs' = aux (Env.apply s lenv') map' state' eqs in
        (Subst.compose sr s, eq' :: eqs') |> ok
  in

  let* s, eqs = aux lenv'' map' state'' step.pstep_def in

  let ty = TFunc (apply s ty1, apply s ty2) in
  let s_min = minimise ty in
  let ty_min = apply s_min ty in
  let s = Subst.compose s_min s in
  let def = List.map (apply_subst_eq s) eqs in

  let name = step.pstep_name.txt in
  let input = apply_subst_pat s p1 in
  let output = apply_subst_pat s p2 in

  let lenv' = Env.add lenv name (generalize Env.empty ty_min) in

  (lenv', Ttree_builder.step name input output def) |> ok

let ty_item genv lenv item =
  match item.ppack_item with
  | Ppack_step s -> ty_step genv lenv s
  | Ppack_node _ -> failwith "not yet implemented"
  | Ppack_link _ -> failwith "not yet implemented"

let ty_pack genv pack =
  let name = pack.ppack_name.txt in
  let items = pack.ppack_items in
  let* lenv, items =
    fold_left
      (fun (lenv, items) item ->
        let* lenv', item' = ty_item genv lenv item in
        (lenv', item' :: items) |> ok)
      (Env.empty, []) items
  in
  let genv = String.Map.add name lenv genv in
  let pack = package name (List.rev items) in
  (genv, pack) |> ok

let f (d : Ptree.t list) : Ttree.t list Reserr.t =
  let* _, d' = fold_left_map ty_pack String.Map.empty d in
  d' |> ok

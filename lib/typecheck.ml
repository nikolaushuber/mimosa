open Ptree
open Type
open Reserr
open Ttree_builder

let state = ref 0

let next_state () =
  let n = !state in
  incr state;
  n

let lookup_id genv lenv id =
  let lookup_name env name =
    match Env.find_opt name env with
    | Some s ->
        let t = Scheme.instantiate s state in
        (Subst.empty, t) |> ok
    | None ->
        let err = Error.Unbound_value name in
        let loc = id.Location.loc in
        error (err, loc)
  in
  match id.Location.txt with
  | Lident.Lident name -> lookup_name lenv name
  | Ldot (pack, name) -> (
      match String.Map.find_opt pack genv with
      | Some env -> lookup_name env name
      | None ->
          let err = Error.Unbound_package pack in
          let loc = id.loc in
          error (err, loc))

let rec ty_expr genv lenv expr =
  let* s, ty, e' =
    match expr.pexpr_desc with
    | Pexpr_ident id -> ty_ident genv lenv id
    | Pexpr_constant c -> ty_const c
    | Pexpr_unop (op, e) -> ty_unop genv lenv op e
    | Pexpr_binop (op, e1, e2) -> ty_binop genv lenv op e1 e2
    | Pexpr_either (e1, e2) -> ty_either genv lenv e1 e2
    | Pexpr_tuple es -> ty_tuple genv lenv es
    | Pexpr_ite (c, t, e) -> ty_ite genv lenv c t e
    | Pexpr_apply (f, e) -> ty_apply genv lenv f e
    | Pexpr_arrow (e1, e2) -> ty_arrow genv lenv e1 e2
    | Pexpr_fby (e1, e2) -> ty_fby genv lenv e1 e2
    | Pexpr_pre e -> ty_pre genv lenv e
    | Pexpr_none -> ty_none ()
    | Pexpr_some e -> ty_some genv lenv e
  in
  (s, ty, e') |> ok

and ty_ident genv lenv id =
  let* s, ty = lookup_id genv lenv id in
  (s, ty, evar id.Location.txt ty) |> ok

and ty_const c =
  match c with
  | Pconst_int i -> (Subst.empty, TInt, eint i) |> ok
  | Pconst_real r -> (Subst.empty, TReal, ereal r) |> ok
  | Pconst_bool b -> (Subst.empty, TBool, ebool b) |> ok
  | Pconst_unit -> (Subst.empty, TUnit, eunit ()) |> ok

and ty_unop genv lenv op e =
  let* s1, ty, e' = ty_expr genv lenv e in
  match op.punop_desc with
  | Punop_not ->
      let* s2 = unify ~loc:e.pexpr_loc TBool ty in
      (Subst.compose s2 s1, TBool, eunop Ttree.Not e' TBool) |> ok
  | Punop_neg ->
      let* s2 = unify ~loc:e.pexpr_loc TInt ty in
      (Subst.compose s2 s1, TInt, eunop Ttree.Neg e' TInt) |> ok
  | Punop_rneg ->
      let* s2 = unify ~loc:e.pexpr_loc TInt ty in
      (Subst.compose s2 s1, TReal, eunop Ttree.RNeg e' TReal) |> ok
  | Punop_is_some ->
      let n = next_state () in
      let* s2 = unify ~loc:e.pexpr_loc (TOption (TVar n)) ty in
      (Subst.compose s2 s1, TOption ty, eunop Ttree.IsSome e' (TOption ty))
      |> ok

and ty_binop genv lenv op e1 e2 =
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
  let* s1, t1, e1' = ty_expr genv lenv e1 in
  let* s2, t2, e2' = ty_expr genv (Env.apply s1 lenv) e2 in
  match op.pbinop_desc with
  | Pbinop_and | Pbinop_or | Pbinop_implies ->
      let* s3 = unify ~loc:e1.pexpr_loc TBool t1
      and* s4 = unify ~loc:e2.pexpr_loc TBool t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s4; s3; s2; s1 ], TBool, e') |> ok
  | Pbinop_add | Pbinop_sub | Pbinop_mul | Pbinop_div ->
      let* s3 = unify ~loc:e1.pexpr_loc TInt t1
      and* s4 = unify ~loc:e2.pexpr_loc TInt t2 in
      let e' = ebinop (trans_op op) e1' e2' TInt in
      (Subst.compose_list [ s4; s3; s2; s1 ], TInt, e') |> ok
  | Pbinop_radd | Pbinop_rsub | Pbinop_rmul | Pbinop_rdiv ->
      let* s3 = unify ~loc:e1.pexpr_loc TReal t1
      and* s4 = unify ~loc:e2.pexpr_loc TReal t2 in
      let e' = ebinop (trans_op op) e1' e2' TReal in
      (Subst.compose_list [ s4; s3; s2; s1 ], TReal, e') |> ok
  | Pbinop_eq | Pbinop_neq ->
      let* s3 = unify ~loc:e1.pexpr_loc t1 t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s3; s2; s1 ], TBool, e') |> ok
  | Pbinop_lt | Pbinop_leq | Pbinop_gt | Pbinop_geq ->
      let* s3 = unify ~loc:e1.pexpr_loc TInt t1
      and* s4 = unify ~loc:e2.pexpr_loc TInt t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s4; s3; s2; s1 ], TBool, e') |> ok
  | Pbinop_rlt | Pbinop_rleq | Pbinop_rgt | Pbinop_rgeq ->
      let* s3 = unify ~loc:e1.pexpr_loc TReal t1
      and* s4 = unify ~loc:e2.pexpr_loc TReal t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s4; s3; s2; s1 ], TBool, e') |> ok

and ty_either genv lenv e1 e2 =
  let* s1, ty1, e1' = ty_expr genv lenv e1 in
  let* s2, ty2, e2' = ty_expr genv (Env.apply s1 lenv) e2 in
  let n = next_state () in
  let* s3 = unify ~loc:e1.pexpr_loc (TOption (TVar n)) ty1 in
  let* s4 = unify ~loc:e2.pexpr_loc (apply s3 (TVar n)) ty2 in
  let s = Subst.compose_list [ s4; s3; s2; s1 ] in
  let ty = apply s (TVar n) in
  let e' = eeither e1' e2' in
  (s, ty, e') |> ok

and ty_tuple genv lenv es =
  let* s, tys, es', _ =
    fold_left
      (fun (sacc, tys, es, lenv) e ->
        let* s, ty, e' = ty_expr genv lenv e in
        (Subst.compose s sacc, ty :: tys, e' :: es, Env.apply s lenv) |> ok)
      (Subst.empty, [], [], lenv)
      es
  in
  let tys = List.rev_map (apply s) tys in
  let e' = etuple es' in
  (s, TTuple tys, e') |> ok

and ty_ite genv lenv c t e =
  let* s1, tc, c' = ty_expr genv lenv c in
  let lenv' = Env.apply s1 lenv in
  let* s2, tt, t' = ty_expr genv lenv' t in
  let lenv'' = Env.apply s2 lenv' in
  let* s3, te, e' = ty_expr genv lenv'' e in
  let* s4 = unify ~loc:c.pexpr_loc TBool tc in
  let* s5 = unify ~loc:e.pexpr_loc tt te in
  let s = Subst.compose_list [ s5; s4; s3; s2; s1 ] in
  let exp' = eif c' t' e' in
  (s, apply s tt, exp') |> ok

and ty_apply genv lenv f e =
  let* s1, ty_f, f' = ty_expr genv lenv f in
  let* s2, ty_e, e' = ty_expr genv (Env.apply s1 lenv) e in
  let n = next_state () in
  let* s3 = unify ~loc:e.pexpr_loc (apply s2 ty_f) (TFunc (ty_e, TVar n)) in
  let ty = apply s3 (TVar n) in
  let exp' = eapply f' e' ty in
  (Subst.compose_list [ s3; s2; s1 ], ty, exp') |> ok

and ty_pre genv lenv e =
  let* s, ty, e' = ty_expr genv lenv e in
  let e'' = epre e' in
  (s, ty, e'') |> ok

and ty_arrow genv lenv e1 e2 =
  let* s1, ty1, e1' = ty_expr genv lenv e1 in
  let* s2, ty2, e2' = ty_expr genv (Env.apply s1 lenv) e2 in
  let* s3 = unify ~loc:e2.pexpr_loc ty1 ty2 in
  let s = Subst.compose_list [ s3; s2; s1 ] in
  let ty = apply s ty2 in
  let e' = earrow e1' e2' in
  (s, ty, e') |> ok

and ty_fby genv lenv e1 e2 =
  let* s1, ty1, e1' = ty_expr genv lenv e1 in
  let* s2, ty2, e2' = ty_expr genv (Env.apply s1 lenv) e2 in
  let* s3 = unify ~loc:e2.pexpr_loc ty1 ty2 in
  let s = Subst.compose_list [ s3; s2; s1 ] in
  let ty = apply s ty2 in
  let e' = efby e1' e2' in
  (s, ty, e') |> ok

and ty_none () =
  let n = next_state () in
  let ty = TOption (TVar n) in
  (Subst.empty, ty, enone ty) |> ok

and ty_some genv lenv e =
  let* s, ty, e' = ty_expr genv lenv e in
  let exp' = esome e' in
  (s, TOption ty, exp') |> ok

let rec ty_core_type lenv map c =
  match c.ptype_desc with
  | Ptype_var s -> (
      match String.Map.find_opt s map with
      | Some t -> (map, t)
      | None ->
          let n = next_state () in
          (String.Map.add s (TVar n) map, TVar n))
  | Ptype_tuple tys ->
      let map', tys =
        List.fold_left
          (fun (map, tys) ty ->
            let map', ty = ty_core_type lenv map ty in
            (map', ty :: tys))
          (map, []) tys
      in
      (map', TTuple (List.rev tys))
  | Ptype_option t ->
      let map', ty = ty_core_type lenv map t in
      (map', TOption ty)
  | Ptype_int -> (map, TInt)
  | Ptype_bool -> (map, TBool)
  | Ptype_real -> (map, TReal)

let rec ty_pattern ?(proto = false) lenv map p =
  let get_ty map p =
    match p.ppat_ty with
    | Some t -> ty_core_type lenv map t |> ok
    | None ->
        if proto then
          let err = Error.Missing_type_in_proto in
          let loc = p.ppat_loc in
          error (err, loc)
        else
          let n = next_state () in
          (map, TVar n) |> ok
  in
  match p.ppat_desc with
  | Ppat_any ->
      let* map', ty = get_ty map p in
      (lenv, map', ty, pany ty) |> ok
  | Ppat_unit -> (lenv, map, TUnit, punit) |> ok
  | Ppat_var v ->
      if proto then
        let* map', ty = get_ty map p in
        (lenv, map', ty, pvar v.txt ty) |> ok
      else
        let ty, lenv' =
          match Env.find_opt v.txt lenv with
          | Some s ->
              let ty = Scheme.instantiate s state in
              (ty, lenv)
          | None ->
              let n = next_state () in
              let ty = TVar n in
              let lenv' = Env.add lenv v.txt (Int.Set.empty, ty) in
              (ty, lenv')
        in
        let* map', spec_ty = get_ty map p in
        let* s = unify ~loc:p.ppat_loc spec_ty ty in
        let ty' = apply s ty in
        let lenv'' = Env.apply s lenv' in
        (lenv'', map', ty', pvar v.txt ty') |> ok
  | Ppat_tuple ps ->
      (* Because of the way we parse tuple patterns the type of the overall
         pattern will always be None, i.e. the user cannot specify
           a, b : int * int
         but only
           (a : int, b : int)
      *)
      let* lenv', map', tys, pats =
        fold_left
          (fun (lenv, map, tys, pats) p ->
            let* lenv', map', ty, p' = ty_pattern ~proto lenv map p in
            (lenv', map', ty :: tys, p' :: pats) |> ok)
          (lenv, map, [], []) ps
      in
      (lenv', map', TTuple (List.rev tys), ptuple (List.rev pats)) |> ok

let ty_eq genv lenv map eq =
  let lhs, rhs = eq in
  let* s1, rhs_ty, rhs' = ty_expr genv lenv rhs in
  let* lenv', map', lhs_ty, lhs' = ty_pattern (Env.apply s1 lenv) map lhs in
  let* s2 = unify ~loc:rhs.pexpr_loc lhs_ty rhs_ty in
  let eq' = (lhs', rhs') in
  let s = Subst.compose s2 s1 in
  (s, Env.apply s2 lenv', map', eq') |> ok

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
  state := 0;
  let* lenv', map, ty1, p1 =
    ty_pattern lenv String.Map.empty step.pstep_input
  in
  let* lenv'', map', ty2, p2 = ty_pattern lenv' map step.pstep_output in

  let rec aux lenv map = function
    | [] -> (Subst.empty, []) |> ok
    | eq :: eqs ->
        let* s, lenv', map', eq' = ty_eq genv lenv map eq in
        let* sr, eqs' = aux (Env.apply s lenv') map' eqs in
        (Subst.compose sr s, eq' :: eqs') |> ok
  in

  let* s, eqs = aux lenv'' map' step.pstep_def in

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

let ty_proto lenv p =
  let* _, _, ty_in, p_in =
    ty_pattern ~proto:true Env.empty String.Map.empty p.pproto_input
  in
  let* _, _, ty_out, p_out =
    ty_pattern ~proto:true Env.empty String.Map.empty p.pproto_output
  in
  let name = p.pproto_name.txt in

  let lenv' = Env.add lenv name (Int.Set.empty, TFunc (ty_in, ty_out)) in

  (lenv', Ttree_builder.proto name p_in p_out) |> ok

let ty_item genv (lenv, acc_protos, acc_steps) item =
  match item.ppack_item with
  | Ppack_step s ->
      let* lenv', step = ty_step genv lenv s in
      (lenv', acc_protos, step :: acc_steps) |> ok
  | Ppack_proto p ->
      let* lenv', proto = ty_proto lenv p in
      (lenv', proto :: acc_protos, acc_steps) |> ok
  | Ppack_node _ -> failwith "not yet implemented"
  | Ppack_link _ -> failwith "not yet implemented"

let deps_of_id acc = function
  | Lident.Lident _ -> acc
  | Ldot (p, _) -> String.Set.add p acc

let rec deps_of_expr acc e =
  match e.pexpr_desc with
  | Pexpr_ident id -> deps_of_id acc id.txt
  | Pexpr_none | Pexpr_constant _ -> acc
  | Pexpr_pre e | Pexpr_some e | Pexpr_unop (_, e) -> deps_of_expr acc e
  | Pexpr_binop (_, e1, e2)
  | Pexpr_either (e1, e2)
  | Pexpr_fby (e1, e2)
  | Pexpr_apply (e1, e2)
  | Pexpr_arrow (e1, e2) ->
      deps_of_expr (deps_of_expr acc e1) e2
  | Pexpr_tuple es -> List.fold_left deps_of_expr acc es
  | Pexpr_ite (e1, e2, e3) ->
      deps_of_expr (deps_of_expr (deps_of_expr acc e1) e2) e3

let deps_of_eq acc (_, e) = deps_of_expr acc e
let deps_of_step s = List.fold_left deps_of_eq String.Set.empty s.pstep_def

let deps_of_item acc item =
  match item.ppack_item with
  | Ppack_step s -> String.Set.union acc (deps_of_step s)
  | _ -> acc

let deps_of_pack p =
  List.fold_left deps_of_item String.Set.empty p.ppack_items
  |> String.Set.to_list

let ty_pack genv pack =
  let name = pack.ppack_name.txt in
  let items = pack.ppack_items in
  let* lenv, protos, steps =
    fold_left (ty_item genv) (Env.empty, [], []) items
  in
  let genv = String.Map.add name lenv genv in
  let deps = deps_of_pack pack in
  let pack = package name deps protos steps [] [] in
  (genv, pack) |> ok

let f (d : Ptree.t list) : Ttree.t list Reserr.t =
  let* _, d' = fold_left_map ty_pack String.Map.empty d in
  d' |> ok

(* Typechecking
   Invariants which must hold at this point:
    - Equations are topologically ordered
    - No causality cycles between equations
    - No mutually recursive dependencies between steps
    - Each symbol is only defined once
    - Each output is defined by an equation
*)

open Ordering
open Ptree
open Type
open Reserr
open Ttree_builder

let state = ref 0

let next_state () =
  let n = !state in
  incr state;
  n

let rec ty_expr env expr =
  match expr.expr_desc with
  | Expr_ident id -> ty_ident expr.expr_loc env id
  | Expr_constant c -> ty_const c
  | Expr_unop (op, e) -> ty_unop env op e
  | Expr_binop (op, e1, e2) -> ty_binop env op e1 e2
  | Expr_either (e1, e2) -> ty_either env e1 e2
  | Expr_tuple es -> ty_tuple env es
  | Expr_ite (c, t, e) -> ty_ite env c t e
  | Expr_apply (f, e) -> ty_apply env f e
  | Expr_arrow (e1, e2) -> ty_arrow env e1 e2
  | Expr_fby (e1, e2) -> ty_fby env e1 e2
  | Expr_pre e -> ty_pre env e
  | Expr_none -> ty_none ()
  | Expr_some e -> ty_some env e

and ty_ident loc env id =
  let lookup_id loc env name =
    match Env.find_opt name env with
    | Some s ->
        let t = Scheme.instantiate s state in
        (Subst.empty, t) |> ok
    | None ->
        let err = Error.Unbound_value name in
        error (err, loc)
  in
  let* s, ty = lookup_id loc env id in
  (s, ty, evar id ty) |> ok

and ty_const c =
  match c with
  | Const_int i -> (Subst.empty, TInt, eint i) |> ok
  | Const_float f -> (Subst.empty, TFloat, efloat f) |> ok
  | Const_bool b -> (Subst.empty, TBool, ebool b) |> ok
  | Const_unit -> (Subst.empty, TUnit, eunit ()) |> ok

and ty_unop env op e =
  let* s1, ty, e' = ty_expr env e in
  match op.unop_desc with
  | Unop_not ->
      let* s2 = unify ~loc:e.expr_loc TBool ty in
      (Subst.compose s2 s1, TBool, eunop Ttree.Not e' TBool) |> ok
  | Unop_neg ->
      let* s2 = unify ~loc:e.expr_loc TInt ty in
      (Subst.compose s2 s1, TInt, eunop Ttree.Neg e' TInt) |> ok
  | Unop_fneg ->
      let* s2 = unify ~loc:e.expr_loc TInt ty in
      (Subst.compose s2 s1, TFloat, eunop Ttree.FNeg e' TFloat) |> ok
  | Unop_is_some ->
      let n = next_state () in
      let* s2 = unify ~loc:e.expr_loc (TOption (TVar n)) ty in
      (Subst.compose s2 s1, TBool, eunop Ttree.IsSome e' TBool) |> ok

and ty_binop env op e1 e2 =
  let trans_op op =
    let open Ttree in
    match op.binop_desc with
    | Binop_and -> And
    | Binop_or -> Or
    | Binop_implies -> Implies
    | Binop_add -> Add
    | Binop_sub -> Sub
    | Binop_mul -> Mul
    | Binop_div -> Div
    | Binop_fadd -> FAdd
    | Binop_fsub -> FSub
    | Binop_fmul -> FMul
    | Binop_fdiv -> FDiv
    | Binop_eq -> Eq
    | Binop_neq -> Neq
    | Binop_lt -> Lt
    | Binop_leq -> Leq
    | Binop_gt -> Gt
    | Binop_geq -> Geq
    | Binop_flt -> FLt
    | Binop_fleq -> FLeq
    | Binop_fgt -> FGt
    | Binop_fgeq -> FGeq
  in
  let* s1, t1, e1' = ty_expr env e1 in
  let* s2, t2, e2' = ty_expr (Env.apply s1 env) e2 in
  match op.binop_desc with
  | Binop_and | Binop_or | Binop_implies ->
      let* s3 = unify ~loc:e1.expr_loc TBool t1
      and* s4 = unify ~loc:e2.expr_loc TBool t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s4; s3; s2; s1 ], TBool, e') |> ok
  | Binop_add | Binop_sub | Binop_mul | Binop_div ->
      let* s3 = unify ~loc:e1.expr_loc TInt t1
      and* s4 = unify ~loc:e2.expr_loc TInt t2 in
      let e' = ebinop (trans_op op) e1' e2' TInt in
      (Subst.compose_list [ s4; s3; s2; s1 ], TInt, e') |> ok
  | Binop_fadd | Binop_fsub | Binop_fmul | Binop_fdiv ->
      let* s3 = unify ~loc:e1.expr_loc TFloat t1
      and* s4 = unify ~loc:e2.expr_loc TFloat t2 in
      let e' = ebinop (trans_op op) e1' e2' TFloat in
      (Subst.compose_list [ s4; s3; s2; s1 ], TFloat, e') |> ok
  | Binop_eq | Binop_neq ->
      let* s3 = unify ~loc:e1.expr_loc t1 t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s3; s2; s1 ], TBool, e') |> ok
  | Binop_lt | Binop_leq | Binop_gt | Binop_geq ->
      let* s3 = unify ~loc:e1.expr_loc TInt t1
      and* s4 = unify ~loc:e2.expr_loc TInt t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s4; s3; s2; s1 ], TBool, e') |> ok
  | Binop_flt | Binop_fleq | Binop_fgt | Binop_fgeq ->
      let* s3 = unify ~loc:e1.expr_loc TFloat t1
      and* s4 = unify ~loc:e2.expr_loc TFloat t2 in
      let e' = ebinop (trans_op op) e1' e2' TBool in
      (Subst.compose_list [ s4; s3; s2; s1 ], TBool, e') |> ok

and ty_either env e1 e2 =
  let* s1, ty1, e1' = ty_expr env e1 in
  let* s2, ty2, e2' = ty_expr (Env.apply s1 env) e2 in
  let n = next_state () in
  let* s3 = unify ~loc:e1.expr_loc (TOption (TVar n)) ty1 in
  let* s4 = unify ~loc:e2.expr_loc (apply s3 (TVar n)) ty2 in
  let s = Subst.compose_list [ s4; s3; s2; s1 ] in
  let ty = apply s (TVar n) in
  let e' = eeither e1' e2' in
  (s, ty, e') |> ok

and ty_tuple env es =
  let* s, tys, es', _ =
    fold_left
      (fun (sacc, tys, es, env) e ->
        let* s, ty, e' = ty_expr env e in
        (Subst.compose s sacc, ty :: tys, e' :: es, Env.apply s env) |> ok)
      (Subst.empty, [], [], env) es
  in
  let tys = List.rev_map (apply s) tys in
  let e' = etuple es' in
  (s, TTuple tys, e') |> ok

and ty_ite env c t e =
  let* s1, tc, c' = ty_expr env c in
  let env' = Env.apply s1 env in
  let* s2, tt, t' = ty_expr env' t in
  let env'' = Env.apply s2 env' in
  let* s3, te, e' = ty_expr env'' e in
  let* s4 = unify ~loc:c.expr_loc TBool tc in
  let* s5 = unify ~loc:e.expr_loc tt te in
  let s = Subst.compose_list [ s5; s4; s3; s2; s1 ] in
  let exp' = eif c' t' e' in
  (s, apply s tt, exp') |> ok

and ty_apply env f e =
  let* s1, ty_f, f' = ty_expr env f in
  let* s2, ty_e, e' = ty_expr (Env.apply s1 env) e in
  let n = next_state () in
  let* s3 = unify ~loc:e.expr_loc (apply s2 ty_f) (TFunc (ty_e, TVar n)) in
  let ty = apply s3 (TVar n) in
  let exp' = eapply f' e' ty in
  (Subst.compose_list [ s3; s2; s1 ], ty, exp') |> ok

and ty_pre env e =
  let* s, ty, e' = ty_expr env e in
  let e'' = epre e' in
  (s, ty, e'') |> ok

and ty_arrow env e1 e2 =
  let* s1, ty1, e1' = ty_expr env e1 in
  let* s2, ty2, e2' = ty_expr (Env.apply s1 env) e2 in
  let* s3 = unify ~loc:e2.expr_loc ty1 ty2 in
  let s = Subst.compose_list [ s3; s2; s1 ] in
  let ty = apply s ty2 in
  let e' = earrow e1' e2' in
  (s, ty, e') |> ok

and ty_fby env e1 e2 =
  let* s1, ty1, e1' = ty_expr env e1 in
  let* s2, ty2, e2' = ty_expr (Env.apply s1 env) e2 in
  let* s3 = unify ~loc:e2.expr_loc ty1 ty2 in
  let s = Subst.compose_list [ s3; s2; s1 ] in
  let ty = apply s ty2 in
  let e' = efby e1' e2' in
  (s, ty, e') |> ok

and ty_none () =
  let n = next_state () in
  let ty = TOption (TVar n) in
  (Subst.empty, ty, enone ty) |> ok

and ty_some env e =
  let* s, ty, e' = ty_expr env e in
  let exp' = esome e' in
  (s, TOption ty, exp') |> ok

let rec ty_core_type map c =
  match c.type_desc with
  | Type_var s -> (
      match String.Map.find_opt s map with
      | Some t -> (map, t)
      | None ->
          let n = next_state () in
          (String.Map.add s (TVar n) map, TVar n))
  | Type_tuple tys ->
      let map', tys =
        List.fold_left
          (fun (map, tys) ty ->
            let map', ty = ty_core_type map ty in
            (map', ty :: tys))
          (map, []) tys
      in
      (map', TTuple (List.rev tys))
  | Type_option t ->
      let map', ty = ty_core_type map t in
      (map', TOption ty)
  | Type_int -> (map, TInt)
  | Type_bool -> (map, TBool)
  | Type_float -> (map, TFloat)

let rec ty_pattern ?(proto = false) env map p =
  let get_ty map p =
    match p.pat_ty with
    | Some t -> ty_core_type map t |> ok
    | None ->
        if proto then
          let err = Error.Missing_type_in_proto in
          let loc = p.pat_loc in
          error (err, loc)
        else
          let n = next_state () in
          (map, TVar n) |> ok
  in
  match p.pat_desc with
  | Pat_any ->
      let* map', ty = get_ty map p in
      (env, map', ty, pany ty) |> ok
  | Pat_unit -> (env, map, TUnit, punit) |> ok
  | Pat_var v ->
      if proto then
        let* map', ty = get_ty map p in
        (env, map', ty, pvar v.txt ty) |> ok
      else
        let ty, env' =
          match Env.find_opt v.txt env with
          | Some s ->
              let ty = Scheme.instantiate s state in
              (ty, env)
          | None ->
              let n = next_state () in
              let ty = TVar n in
              let env' = Env.add env v.txt (Int.Set.empty, ty) in
              (ty, env')
        in
        let* map', spec_ty = get_ty map p in
        let* s = unify ~loc:p.pat_loc spec_ty ty in
        let ty' = apply s ty in
        let env'' = Env.apply s env' in
        (env'', map', ty', pvar v.txt ty') |> ok
  | Pat_tuple ps ->
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
          (env, map, [], []) ps
      in
      (lenv', map', TTuple (List.rev tys), ptuple (List.rev pats)) |> ok

let ty_eq env (lhs_ty, p, e) =
  let* s1, rhs_ty, e' = ty_expr env e in
  let* s2 = unify ~loc:e.expr_loc lhs_ty rhs_ty in
  let s = Subst.compose s2 s1 in
  let lenv'' = Env.apply s2 env in
  (s, lenv'', (p, e')) |> ok

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

let ty_step env step =
  state := 0;

  let* env', map, ty_inp, pat_inp =
    ty_pattern env String.Map.empty step.step_input
  in
  let* env'', map', ty_out, pat_out = ty_pattern env' map step.step_output in

  let* (env''', map''), eqs =
    fold_left_map
      (fun (env, map) (lhs, rhs) ->
        let* env', map', ty_lhs, pat_lhs = ty_pattern env map lhs in
        ((env', map'), (ty_lhs, pat_lhs, rhs)) |> ok)
      (env'', map') step.step_def
  in

  let rec aux env map = function
    | [] -> (Subst.empty, []) |> ok
    | eq :: eqs ->
        let* s, env', eq' = ty_eq env eq in
        let* sr, eqs' = aux (Env.apply s env') map eqs in
        (Subst.compose sr s, eq' :: eqs') |> ok
  in

  let* s, eqs = aux env''' map'' eqs in

  let ty = TFunc (apply s ty_inp, apply s ty_out) in
  let s_min = minimise ty in
  let ty_min = apply s_min ty in
  let s = Subst.compose s_min s in
  let def = List.map (apply_subst_eq s) eqs in

  let name = step.step_name.txt in
  let input = apply_subst_pat s pat_inp in
  let output = apply_subst_pat s pat_out in

  let env' = Env.add env name (generalize Env.empty ty_min) in

  (env', Ttree_builder.step name input output def) |> ok

let ty_proto env p =
  let* _, _, ty_in, p_in =
    ty_pattern ~proto:true Env.empty String.Map.empty p.proto_input
  in
  let* _, _, ty_out, p_out =
    ty_pattern ~proto:true Env.empty String.Map.empty p.proto_output
  in
  let name = p.proto_name.txt in

  let env' = Env.add env name (Int.Set.empty, TFunc (ty_in, ty_out)) in

  (env', Ttree_builder.proto name p_in p_out) |> ok

let ty_channel env l =
  let rec ty_core_type c =
    match c.type_desc with
    | Type_var _ ->
        let err = Error.Typevar_in_link in
        let loc = c.type_loc in
        error (err, loc)
    | Type_tuple tys ->
        let* ty = map ty_core_type tys in
        TTuple ty |> ok
    | Type_option t ->
        let* ty = ty_core_type t in
        TOption ty |> ok
    | Type_int -> TInt |> ok
    | Type_bool -> TBool |> ok
    | Type_float -> TFloat |> ok
  in
  let name = l.channel_name in
  let* ty' = ty_core_type l.channel_type in
  let env' = String.Map.add name ty' env in
  let* elems =
    map
      (fun e ->
        let* _, _, e' = ty_expr Env.empty e in
        e' |> ok)
      l.channel_elems
  in
  (env', channel name ty' elems) |> ok

let ty_port env p =
  let port_name = p.port_name.txt in
  let port_opt = p.port_opt in
  (env, port port_name port_opt) |> ok

let ty_node env n =
  let node_name = n.node_name.txt in
  let node_implements = n.node_implements.txt in
  let* _, node_inputs = fold_left_map ty_port String.Map.empty n.node_inputs in
  let* _, node_outputs =
    fold_left_map ty_port String.Map.empty n.node_outputs
  in
  let node_period = (n.node_period.period_time, n.node_period.period_unit) in
  (env, node node_name node_implements node_inputs node_outputs node_period)
  |> ok

let ty_pack pack =
  let* env, protos = fold_left_map ty_proto Env.empty pack.protos in
  let* _, steps = fold_left_map ty_step env pack.steps in
  let* _, channels = fold_left_map ty_channel String.Map.empty pack.channels in
  let* _, nodes = fold_left_map ty_node String.Map.empty pack.nodes in
  let pack = package protos steps channels nodes in
  pack |> ok

let f (d : Ordering.t) : Ttree.t Reserr.t = ty_pack d

open Reserr
open Ttree

module Cache = struct
  type t = string Type.Map.t String.Map.t

  let empty : t = String.Map.empty
  let mem = String.Map.mem
  let add = String.Map.add
  let find_opt = String.Map.find_opt

  let get_name cache name ty =
    match find_opt name cache with
    | None -> (name, add name (Type.Map.singleton ty name) cache)
    | Some map -> (
        match Type.Map.find_opt ty map with
        | None ->
            let n = Type.Map.cardinal map in
            let new_name = Format.asprintf "%s_%d" name n in
            (new_name, add name (Type.Map.add ty new_name map) cache)
        | Some n -> (n, cache))

  let pp : t Fmt.t =
    String.Map.pp
      ~sep:(fun ppf _ -> Format.pp_print_string ppf " => ")
      (Type.Map.pp Fmt.string)
end

let rec cache_expr ((gcache, lcache) as acc) expr =
  let acc', desc' =
    match expr.expr_desc with
    | EVar v -> (
        match v with
        | Lident f ->
            let v', lcache' = Cache.get_name lcache f expr.expr_ty in
            ((gcache, lcache'), EVar (Lident v'))
        | Ldot (p, v) -> (
            match Cache.find_opt p gcache with
            | None ->
                let v', cache' = Cache.get_name Cache.empty v expr.expr_ty in
                ((String.Map.add p cache' gcache, lcache), EVar (Ldot (p, v')))
            | Some cache ->
                let v', cache' = Cache.get_name cache v expr.expr_ty in
                ((String.Map.add p cache' gcache, lcache), EVar (Ldot (p, v'))))
        )
    | EConst c -> (acc, EConst c)
    | ENone -> (acc, ENone)
    | EPre e ->
        let acc', e' = cache_expr acc e in
        (acc', EPre e')
    | ESome e ->
        let acc', e' = cache_expr acc e in
        (acc', ESome e')
    | EUnOp (op, e) ->
        let acc', e' = cache_expr acc e in
        (acc', EUnOp (op, e'))
    | EBinOp (op, e1, e2) ->
        let acc', e1' = cache_expr acc e1 in
        let acc'', e2' = cache_expr acc' e2 in
        (acc'', EBinOp (op, e1', e2'))
    | EEither (e1, e2) ->
        let acc', e1' = cache_expr acc e1 in
        let acc'', e2' = cache_expr acc' e2 in
        (acc'', EEither (e1', e2'))
    | EFby (e1, e2) ->
        let acc', e1' = cache_expr acc e1 in
        let acc'', e2' = cache_expr acc' e2 in
        (acc'', EFby (e1', e2'))
    | EArrow (e1, e2) ->
        let acc', e1' = cache_expr acc e1 in
        let acc'', e2' = cache_expr acc' e2 in
        (acc'', EArrow (e1', e2'))
    | EIf (c, t, e) ->
        let acc', c' = cache_expr acc c in
        let acc'', t' = cache_expr acc' t in
        let acc''', e' = cache_expr acc'' e in
        (acc''', EIf (c', t', e'))
    | EMatch _ -> failwith "not yet implemented"
    | ETuple es ->
        let acc', es' = List.fold_left_map cache_expr acc es in
        (acc', ETuple es')
    | EApp (e1, e2) ->
        let acc', e1' = cache_expr acc e1 in
        let acc'', e2' = cache_expr acc' e2 in
        (acc'', EApp (e1', e2'))
  in
  (acc', { expr with expr_desc = desc' })

let cache_step acc step =
  let acc', defs' =
    List.fold_left_map
      (fun acc (lhs, rhs) ->
        let acc', rhs' = cache_expr acc rhs in
        (acc', (lhs, rhs')))
      acc step.step_def
  in
  (acc', { step with step_def = defs' })

let subst_bool t : Type.Subst.t =
  let open Type in
  let rec aux acc = function
    | TUnit | TInt | TBool | TReal -> acc
    | TOption t -> aux acc t
    | TTuple ts -> List.fold_left aux acc ts
    | TVar n -> Subst.compose (Int.Map.singleton n TBool) acc
    | TFunc (t1, t2) ->
        let acc' = aux acc t1 in
        aux acc' t2
  in
  aux Subst.empty t

let specialise_step name ty step =
  let open Type in
  let arg_ty = step.step_input.pat_ty in
  let ret_ty = step.step_output.pat_ty in
  let ty' = TFunc (arg_ty, ret_ty) in
  let bool_subst = subst_bool ty in
  let* s = unify ty' ty in
  let s' = Subst.compose bool_subst s in
  let input' = Typecheck.apply_subst_pat s' step.step_input in
  let output' = Typecheck.apply_subst_pat s' step.step_output in
  let def' =
    List.map
      (fun (lhs, rhs) ->
        (Typecheck.apply_subst_pat s' lhs, Typecheck.apply_subst_expr s' rhs))
      step.step_def
  in
  {
    step_name = name;
    step_input = input';
    step_output = output';
    step_def = def';
  }
  |> ok

let duplicate_step (lcache : Cache.t) step =
  match Cache.find_opt step.step_name lcache with
  | None ->
      let name = step.step_name in
      let ty = Type.TFunc (step.step_input.pat_ty, step.step_output.pat_ty) in
      [ specialise_step name ty step ] |> sequence
  | Some map ->
      let tys = Type.Map.to_list map in
      List.map (fun (ty, name) -> specialise_step name ty step) tys |> sequence

let trans_step step ((_, lcache) as acc) =
  let* steps = duplicate_step lcache step in
  let acc', steps' = List.fold_left_map cache_step acc steps in
  (List.map (fun s -> Step s) steps', acc') |> ok

let trans_item item acc =
  match item with
  | Step s -> trans_step s acc

let trans_pack pack gcache =
  let lcache =
    match String.Map.find_opt pack.pack_name gcache with
    | Some cache -> cache
    | None -> Cache.empty
  in
  let cache = (gcache, lcache) in
  let* items', (_, lcache') = fold_right_map trans_item pack.pack_items cache in
  let gcache' = String.Map.add pack.pack_name lcache' gcache in
  ({ pack with pack_items = List.concat (List.rev items') }, gcache') |> ok

let f (d : t list) : t list Reserr.t =
  let* d', _ = fold_right_map trans_pack d String.Map.empty in
  d' |> ok

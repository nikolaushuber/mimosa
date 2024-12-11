(** Translate typed AST to k-normal form *)

open Ttree
open Norm_builder

let counter = ref 0

let new_var ?(prefix = "tmp") _ =
  let id = !counter in
  incr counter;
  Format.asprintf "%s_%d" prefix id

let rec dummy_nil =
  let open Type in
  let open Ttree_builder in
  function
  | TInt -> eint 0
  | TUnit -> eunit ()
  | TBool -> ebool false
  | TReal -> ereal 0.0
  | TOption t -> enone t
  | TTuple tys -> etuple (List.map dummy_nil tys)
  | TFunc _ | TVar _ -> assert false

let trans_name map s =
  match List.assoc_opt s map with
  | None -> s
  | Some s -> s

let rec trans_expr map e : Norm.block =
  match e.expr_desc with
  | EVar (Lident v) -> ([], base_var (trans_name map v) e.expr_ty)
  | EVar v -> ([], base_global_var v e.expr_ty)
  | EConst c -> ([], base_const c e.expr_ty)
  | EUnOp (op, e1) ->
      let eqs', e1' = trans_expr map e1 in
      let x = new_var () in
      (eqs' @ [ (pvar x e.expr_ty, base_expr e1') ], base_unop op x e.expr_ty)
  | EBinOp (op, e1, e2) ->
      let eqs1, e1' = trans_expr map e1 in
      let eqs2, e2' = trans_expr map e2 in
      let x = new_var () in
      let y = new_var () in
      let eqs =
        eqs1
        @ eqs2
        @ [
            (pvar x e1.expr_ty, base_expr e1');
            (pvar y e2.expr_ty, base_expr e2');
          ]
      in
      (eqs, base_binop op x y e.expr_ty)
  | EEither (e1, e2) ->
      let eqs1, e1' = trans_expr map e1 in
      let block2 = trans_expr map e2 in
      let x = new_var () in
      let y = new_var () in
      let eqs =
        eqs1
        @ [
            (pvar x e1.expr_ty, base_expr e1');
            (pvar y e.expr_ty, eeither x block2 e.expr_ty);
          ]
      in
      (eqs, base_var x e.expr_ty)
  | EIf (c, t, e) ->
      let eqs, c' = trans_expr map c in
      let x = new_var () in
      let t' = trans_expr map t in
      let e' = trans_expr map e in
      let ret = new_var () in
      let eqs' =
        eqs
        @ [
            (pvar x c.expr_ty, base_expr c');
            (pvar ret t.expr_ty, eif x t' e' t.expr_ty);
          ]
      in
      (eqs', base_var ret t.expr_ty)
  | EApp (f, arg) ->
      let f_id =
        (* For now we don't have anonymous functions *)
        match f.expr_desc with
        | EVar id -> id
        | _ -> assert false
      in
      let eqs, arg' = trans_expr map arg in
      let x = new_var () in
      let ret = new_var () in
      let eqs' =
        eqs
        @ [
            (pvar x arg.expr_ty, base_expr arg');
            (pvar ret e.expr_ty, eapp f_id x e.expr_ty);
          ]
      in
      (eqs', base_var ret e.expr_ty)
  | EPre e -> trans_expr map (Ttree_builder.earrow (dummy_nil e.expr_ty) e)
  | ENone -> ([], base_none e.expr_ty)
  | ESome e1 ->
      let eqs, e1' = trans_expr map e1 in
      let x = new_var () in
      let eqs' = eqs @ [ (pvar x e1.expr_ty, base_expr e1') ] in
      (eqs', base_some x e.expr_ty)
  | EFby (e1, e2) ->
      let eqs, e1' = trans_expr map e1 in
      let x = new_var () in
      let ret = new_var () in
      let eqs' =
        eqs
        @ [
            (pvar x e1.expr_ty, base_expr e1');
            (pvar ret e1.expr_ty, efby x (trans_expr map e2) e.expr_ty);
          ]
      in
      (eqs', base_var ret e1.expr_ty)
  | EArrow (e1, e2) ->
      let ty = e1.expr_ty in
      let eqs1, e1' = trans_expr map e1 in
      let eqs2, e2' = trans_expr map e2 in
      let x = new_var () in
      let y = new_var () in
      let ret = new_var () in
      let eqs' =
        eqs1
        @ eqs2
        @ [
            (pvar x ty, base_expr e1');
            (pvar y ty, base_expr e2');
            (pvar ret ty, efby x ([], base_var y ty) ty);
          ]
      in
      (eqs', base_var ret ty)
  | EMatch _ -> failwith "not yet implemented"
  | ETuple es ->
      let eqs', es' =
        List.fold_left_map
          (fun acc e ->
            let eqs, e' = trans_expr map e in
            let x = new_var () in
            let eqs' = acc @ eqs @ [ (pvar x e.expr_ty, base_expr e') ] in
            (eqs', x))
          [] es
      in
      let ret = new_var () in
      let eqs'' = eqs' @ [ (pvar ret e.expr_ty, etuple es' e.expr_ty) ] in
      (eqs'', base_var ret e.expr_ty)

let trans_pattern map pat =
  match pat.pat_desc with
  | PAny -> (map, fun e -> [ (pany pat.pat_ty, e) ])
  | PUnit -> (map, fun e -> [ (punit, e) ])
  | PVar v ->
      let v' = new_var ~prefix:v () in
      ((v, v') :: map, fun e -> [ (pvar v' pat.pat_ty, e) ])
  | PTuple ps ->
      let rec aux (map, acc, eqs) p =
        match p.pat_desc with
        | PAny | PUnit ->
            let x = new_var ~prefix:"unused" () in
            (map, acc @ [ x ], eqs)
        | PVar v ->
            let v' = new_var ~prefix:v () in
            ((v, v') :: map, acc @ [ v ], eqs)
        | PTuple ps -> List.fold_left aux (map, acc, eqs) ps
      in
      let map', r, eqs = List.fold_left aux (map, [], []) ps in
      (map', fun e -> (ptuple r pat.pat_ty, e) :: eqs)

let rec trans_output map pat =
  match pat.pat_desc with
  | PAny -> assert false
  | PVar v ->
      ( [],
        base_var
          (match List.assoc_opt v map with
          | Some v' -> v'
          | None -> v)
          pat.pat_ty )
  | PUnit -> ([], base_const Ttree.CUnit pat.pat_ty)
  | PTuple ps ->
      let eqs, names =
        List.fold_left_map
          (fun acc p ->
            let eqs, e' = trans_output map p in
            let x = new_var () in
            let eqs' = acc @ eqs @ [ (pvar x p.pat_ty, base_expr e') ] in
            (eqs', x))
          [] ps
      in
      let ret = new_var () in
      let eqs' = eqs @ [ (pvar ret pat.pat_ty, etuple names pat.pat_ty) ] in
      (eqs', base_var ret pat.pat_ty)

let trans_equation map (lhs, rhs) =
  let map', lhs' = trans_pattern map lhs in
  let eqs, rhs' = trans_expr map' rhs in

  (map', eqs @ lhs' (base_expr rhs'))

let trans_step { step_name; step_input; step_output; step_def } =
  counter := 0;
  let in_var = new_var ~prefix:"input" () in
  let map, input_cont = trans_pattern [] step_input in
  let input_eqs = input_cont (evar in_var step_input.pat_ty) in
  let map', def' = List.fold_left_map trans_equation map step_def in
  let eqs', output = trans_output map' step_output in
  let body = (input_eqs @ List.flatten def' @ eqs', output) in
  step step_name
    (pvar in_var step_input.pat_ty)
    step_output.pat_ty body !counter

let trans_item = function
  | Step s -> trans_step s

let f pack =
  let items' = List.map trans_item pack.pack_items in
  package pack.pack_name items'

(** Translate typed AST to k-normal form *)

open Ttree
open Norm_builder

let counter = ref 0

let new_var ?(prefix = "tmp") _ =
  let id = !counter in
  incr counter;
  Format.asprintf "%s_%d" prefix id

let insert_let (e, t) k =
  let open Norm in
  match e with
  | EVar (Lident s) -> k s
  | _ ->
      let x = new_var t in
      let e', t' = k x in
      (ELet (pvar t x, e, e'), t')

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

let get_id map =
  let open Lident in
  function
  | Lident s -> (
      match List.assoc_opt s map with
      | None -> Lident s
      | Some s -> Lident s)
  | Ldot (p, s) -> Ldot (p, s)

let rec trans_expr map e =
  match e.expr_desc with
  | EVar v -> (evar (get_id map v), e.expr_ty)
  | EConst c -> (econst c, e.expr_ty)
  | EUnOp (op, e) ->
      insert_let (trans_expr map e) (fun x -> (eunop op x, e.expr_ty))
  | EBinOp (op, e1, e2) ->
      insert_let (trans_expr map e1) (fun x ->
          insert_let (trans_expr map e2) (fun y -> (ebinop op x y, e.expr_ty)))
  | EEither (e1, e2) ->
      insert_let (trans_expr map e1) (fun x ->
          let e2', _ = trans_expr map e2 in
          (eeither x e2', e.expr_ty))
  | EIf (c, t, e) ->
      insert_let (trans_expr map c) (fun x ->
          let t', _ = trans_expr map t in
          let e', _ = trans_expr map e in
          (eif x t' e', e.expr_ty))
  | EApp (f, args) ->
      let f_id =
        match f.expr_desc with
        | EVar id -> id
        | _ -> assert false
      in
      insert_let (trans_expr map args) (fun x -> (eapp f_id x, e.expr_ty))
  | EPre e -> trans_expr map (Ttree_builder.earrow (dummy_nil e.expr_ty) e)
  | ENone -> (enone, e.expr_ty)
  | ESome e -> insert_let (trans_expr map e) (fun x -> (esome x, e.expr_ty))
  | EFby (e1, e2) ->
      insert_let (trans_expr map e1) (fun x ->
          let e2', _ = trans_expr map e2 in
          (efby x e2', e.expr_ty))
  | EArrow (e1, e2) ->
      insert_let (trans_expr map e1) (fun x ->
          insert_let (trans_expr map e2) (fun y ->
              (efby x (evar (Lident.Lident y)), e.expr_ty)))
  | EMatch _ -> failwith "not yet implemented"
  | ETuple es ->
      let rec bind xs ts = function
        | [] -> (etuple xs, Type.TTuple ts)
        | e :: es ->
            let e', t' = trans_expr map e in
            insert_let (e', t') (fun x -> bind (xs @ [ x ]) (ts @ [ t' ]) es)
      in
      bind [] [] es

let rec trans_pattern map pat =
  match pat.pat_desc with
  | PAny -> (map, pany pat.pat_ty)
  | PUnit -> (map, punit)
  | PVar v ->
      let v' = new_var ~prefix:v () in
      ((v, v') :: map, pvar pat.pat_ty v')
  | PTuple ps ->
      let map', ps' = List.fold_left_map trans_pattern map ps in
      (map', ptuple ps')

let rec trans_output map pat =
  match pat.pat_desc with
  | PAny -> assert false
  | PVar v ->
      ( evar
          (match List.assoc_opt v map with
          | Some v' -> Lident.Lident v'
          | None -> Lident.Lident v),
        pat.pat_ty )
  | PUnit -> (econst Ttree.CUnit, pat.pat_ty)
  | PTuple ps ->
      let rec bind xs ts = function
        | [] -> (etuple xs, Type.TTuple ts)
        | p :: ps ->
            let e', t' = trans_output map p in
            insert_let (e', t') (fun x -> bind (xs @ [ x ]) (ts @ [ t' ]) ps)
      in
      bind [] [] ps

let trans_equation map (lhs, rhs) =
  let map', lhs' = trans_pattern map lhs in
  let rhs', _ = trans_expr map' rhs in
  (map', (lhs', rhs'))

let trans_step { step_name; step_input; step_output; step_def } =
  counter := 0;
  let map, input = trans_pattern [] step_input in
  let map', def' = List.fold_left_map trans_equation map step_def in
  let output, _ = trans_output map' step_output in
  let body = List.fold_right (fun (lhs, rhs) e -> elet lhs rhs e) def' output in
  step step_name input body

let trans_item = function
  | Step s -> trans_step s

let f pack =
  let items' = List.map trans_item pack.pack_items in
  package pack.pack_name items'

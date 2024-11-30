open Norm
open Fmt

let pp_pattern ppf pat =
  match pat.pat_desc with
  | PAny -> string ppf "_"
  | PUnit -> string ppf "()"
  | PVar v -> pf ppf "%s : %a" v Type.pp pat.pat_ty
  | PTuple ps -> (list ~sep:comma string |> box) ppf ps

let pp_base_expr ppf e =
  match e.base_expr_desc with
  | EConst c -> Ttree_printer.pp_const ppf c
  | EVar v -> string ppf v
  | EGlobalVar id -> Lident.pp ppf id
  | EUnOp (op, e) -> pf ppf "@[<hov>%a@;<1 2>%s@]" Ttree_printer.pp_unop op e
  | EBinOp (op, e1, e2) ->
      pf ppf "@[<hov>%s@;<1 2>%a@;<1 2>%s@]" e1 Ttree_printer.pp_binop op e2
  | ENone -> string ppf "None"
  | ESome e -> pf ppf "@[Some@;<1 2>%s@]" e

let rec pp_expr ppf e =
  match e.expr_desc with
  | EBase e -> pp_base_expr ppf e
  | EEither (e1, e2) ->
      pf ppf "@[either@;<1 2>%s@;<1 0>or@;1 2>%a@]" e1 pp_block e2
  | EIf (c, t, e) ->
      pf ppf "@[if@;<1 2>%s@;<1 0>then@;<1 2>%a@;<1 0>else@;<1 2>%a@]" c
        pp_block t pp_block e
  | EApp (f, e) -> pf ppf "@[%a@;<1 2>(%a)@]" Lident.pp f string e
  | EFby (e1, e2) -> pf ppf "@[<b>%s@ fby@ %a@]" e1 pp_block e2
  | ETuple es -> pf ppf "@[<hov>%a@]" (list ~sep:comma string) es

and pp_block ppf (l, e) =
  match l with
  | [] -> pp_base_expr ppf e
  | eqs ->
      pf ppf "@;<0 2>@[<v>%a@;<0 0>in@;<0 0>%a@]"
        (list ~sep:cut
           (pair ~sep:(fun ppf _ -> pf ppf " = ") pp_pattern pp_expr))
        eqs pp_base_expr e

let pp_step ppf (name, input, e, _) =
  pf ppf "@[<v>@[<hov>step@;<1 2>%s@;<1 2>(%a)@;<1 0>=@]%a@]" name pp_pattern
    input pp_block e

let pp_item ppf = function
  | Step s -> pp_step ppf s

let pp ppf (Package (name, items)) =
  pf ppf "@[<v>@[package@;<1 2>%s@]@;@;%a@]@." name
    (list ~sep:(cut ++ cut) pp_item)
    items

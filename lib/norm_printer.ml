open Norm
open Fmt

let rec pp_pattern ppf = function
  | PAny ty -> pf ppf "_ : %a" Type.pp ty
  | PUnit -> string ppf "()"
  | PVar (ty, v) -> pf ppf "%s : %a" v Type.pp ty
  | PTuple ps -> (list ~sep:comma pp_pattern |> box) ppf ps

let rec pp_expr ppf = function
  | EConst c -> Ttree_printer.pp_const ppf c
  | EVar id -> Lident.pp ppf id
  | EUnOp (op, e) -> pf ppf "@[<hov>%a@;<1 2>%s@]" Ttree_printer.pp_unop op e
  | EBinOp (op, e1, e2) ->
      pf ppf "@[<hov>%s@;<1 2>%a@;<1 2>%s)@]" e1 Ttree_printer.pp_binop op e2
  | EEither (e1, e2) ->
      pf ppf "@[either@;<1 2>%s@;<1 0>or@;1 2>%a@]" e1 pp_expr e2
  | ETuple es -> pf ppf "@[<hov>%a@]" (list ~sep:comma string) es
  | EIf (c, t, e) ->
      pf ppf "@[if@;<1 2>%s@;<1 0>then@;<1 2>%a@;<1 0>else@;<1 2>%a@]" c pp_expr
        t pp_expr e
  | EApp (f, e) -> pf ppf "@[%a@;<1 2>%s@]" Lident.pp f e
  | EFby (e1, e2) -> pf ppf "@[<b>%s@ fby@ %a@]" e1 pp_expr e2
  | ENone -> string ppf "None"
  | ESome e -> pf ppf "@[Some@;<1 2>%s@]" e
  | ELet (p, (ELet _ as e1), e2) ->
      pf ppf "@[<v>@[<hov>let@;<1 2>%a@;<1 0>=@]@;<1 2>%a@;in@;%a@]" pp_pattern
        p pp_expr e1 pp_expr e2
  | ELet (p, e1, e2) ->
      pf ppf "@[<v>@[<hov>let@;<1 2>%a@;<1 0>=@;<1 2>%a@;<1 0>in@]@;%a@]"
        pp_pattern p pp_expr e1 pp_expr e2

let pp_step ppf (name, input, e) =
  pf ppf "@[<v>@[<hov>step@;<1 2>%s@;<1 2>(%a)@;<1 0>=@]@;<0 2>%a@]" name
    pp_pattern input pp_expr e

let pp_item ppf = function
  | Step s -> pp_step ppf s

let pp ppf (Package (name, items)) =
  pf ppf "@[<v>@[package@;<1 2>%s@]@;@;%a@]@." name
    (list ~sep:(cut ++ cut) pp_item)
    items

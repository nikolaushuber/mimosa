open Norm
open Fmt

let colon ppf _ = pf ppf " : "
let pp_pattern = Ttree_printer.pp_pat

let pp_base_expr ppf e =
  match e.base_expr_desc with
  | EConst c -> Ttree_printer.pp_const ppf c
  | EVar v -> string ppf v
  | EUnOp (op, e) -> pf ppf "%a@ %s" Ttree_printer.pp_unop op e
  | EBinOp (op, e1, e2) ->
      pf ppf "@[<2>%s@;%a@;%s@]" e1 Ttree_printer.pp_binop op e2
  | ENone -> string ppf "None"
  | ESome e -> pf ppf "@[Some@;%s@]" e

let rec pp_expr ppf e =
  match e.expr_desc with
  | EBase e -> pp_base_expr ppf e
  | EEither (e1, e2) ->
      let fmt : (_, _, _) format =
        "@[<hv0>@[<2>either@ %s@]@ @[<2>or@ %a@]@]"
      in
      pf ppf fmt e1 pp_block e2
  | EIf (c, t, e) ->
      let fmt : (_, _, _) format =
        "@[<hv0>@[<2>if@ %s@]@;@[<2>then@ %a@]@;@[<2>else@;%a@]@]"
      in
      pf ppf fmt c pp_block t pp_block e
  | EApp (f, e) -> pf ppf "@[<2>%a@;%a@]" string f string e
  | EFby (e1, e2) -> pf ppf "@[<2>%s@;fby@;%a@]" e1 pp_block e2
  | ETuple es ->
      pf ppf "@[<hov2>(%a)@]" (list ~sep:(fun ppf _ -> pf ppf ",@;") string) es

and pp_eq ppf (lhs, rhs) =
  let pp_rhs ppf rhs = pf ppf "=@;%a" pp_expr rhs in
  pf ppf "%a@ %a" pp_pattern lhs pp_rhs rhs

and pp_eqs ppf l =
  let pp_eq ppf x = pf ppf "@[<2>%a@]" pp_eq x in
  match l with
  | [] -> ()
  | [ x ] -> pp_eq ppf x
  | x :: xs -> pf ppf "@[<v>%a@,%a@]" pp_eq x (list ~sep:sp pp_eq) xs

and pp_block ppf (l, e) =
  match l with
  | [] -> pp_base_expr ppf e
  | l -> pf ppf "@[<v 2>%a@;<1 -2>in %a@]" pp_eqs l pp_base_expr e

let pp_step ppf (name, input, _, e, _) =
  pf ppf "@[<2>step@ %s@ (%a)@ %a@]@\n" name pp_pattern input pp_block e

let pp_proto ppf (name, input, _) =
  pf ppf "@[<2>extern step@ %s@ (%a)@]@\n" name pp_pattern input

let pp ppf p =
  pf ppf "@[<v>%a@;%a@]@." (list pp_proto) p.protos (list pp_step) p.steps

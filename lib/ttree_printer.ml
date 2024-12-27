open Ttree
open Fmt

let colon : unit Fmt.t = any "@ :@ "

let rec pp_pat : pat Fmt.t =
 fun ppf pat ->
  let pp outermost ppf pat =
    match pat.pat_desc with
    | PAny -> pf ppf "_ : %a" Type.pp pat.pat_ty
    | PUnit -> pf ppf "() : %a" Type.pp pat.pat_ty
    | PVar v -> pf ppf "%s : %a" v Type.pp pat.pat_ty
    | PTuple ps ->
        let aux = list ~sep:comma pp_pat in
        (if outermost then aux else aux |> parens) ppf ps
  in
  (pp true) ppf pat

let pp_const ppf = function
  | CUnit -> string ppf "()"
  | CInt i -> int ppf i
  | CReal r -> float ppf r
  | CBool b -> bool ppf b

let pp_unop ppf = function
  | Not -> string ppf "~"
  | Neg -> string ppf "-"
  | RNeg -> string ppf "-."
  | IsSome -> string ppf "?"

let pp_binop ppf = function
  | And -> string ppf "&&"
  | Or -> string ppf "||"
  | Implies -> string ppf "=>"
  | Add -> string ppf "+"
  | Sub -> string ppf "-"
  | Mul -> string ppf "*"
  | Div -> string ppf "/"
  | RAdd -> string ppf "+."
  | RSub -> string ppf "-."
  | RMul -> string ppf "*."
  | RDiv -> string ppf "/."
  | Eq -> string ppf "=="
  | Neq -> string ppf "!="
  | Lt -> string ppf "<"
  | Leq -> string ppf "<="
  | Gt -> string ppf ">"
  | Geq -> string ppf ">="
  | RLt -> string ppf "<."
  | RLeq -> string ppf "<=."
  | RGt -> string ppf ">."
  | RGeq -> string ppf ">=."

let rec pp_expr ppf expr =
  match expr.expr_desc with
  | EVar id -> Lident.pp ppf id
  | EConst c -> pp_const ppf c
  | EUnOp (op, e) -> pf ppf "@[%a%a@]" pp_unop op pp_expr e
  | EBinOp (op, e1, e2) ->
      pf ppf "@[%a@ %a %a@]" pp_expr e1 pp_binop op pp_expr e2
  | EEither (e1, e2) -> pf ppf "@[either@ %a@ or@ %a@]" pp_expr e1 pp_expr e2
  | ETuple es -> (list ~sep:comma pp_expr |> parens) ppf es
  | EIf (c, t, e) ->
      pf ppf "@[if@;<1 2>%a@;<1 0>then@;<1 2>%a@;<1 0>else@;<1 2>%a@;<0 0>@]"
        pp_expr c pp_expr t pp_expr e
  | EApp (e1, e2) -> pf ppf "%a@ (%a)" pp_expr e1 pp_expr e2
  | EMatch _ -> failwith "not yet implemented"
  | EArrow (e1, e2) -> pf ppf "@[%a@ -> %a@]" pp_expr e1 pp_expr e2
  | EFby (e1, e2) -> pf ppf "@[%a@ fby %a@]" pp_expr e1 pp_expr e2
  | EPre e -> pf ppf "pre %a" pp_expr e
  | ENone -> string ppf "None"
  | ESome e -> pf ppf "Some %a" pp_expr e

let pp_eq ppf (lhs, rhs) = pf ppf "@[<2>%a@ =@ %a@]" pp_pat lhs pp_expr rhs

let pp_step ppf step =
  pf ppf "step %s %a --> %a@;{@;<0 2>@[<v>%a@]@;}" step.step_name
    (parens pp_pat) step.step_input (parens pp_pat) step.step_output
    (list ~sep:(any ";@;") pp_eq)
    step.step_def

let pp_proto ppf p =
  pf ppf "step %s %a --> %a" p.proto_name (parens pp_pat) p.proto_input
    (parens pp_pat) p.proto_output

let pp ppf pack =
  pf ppf "@[<v>package %s@;@;%a@;%a@]" pack.pack_name
    (list ~sep:(cut ++ cut) pp_proto)
    pack.pack_protos
    (list ~sep:(cut ++ cut) pp_step)
    pack.pack_steps

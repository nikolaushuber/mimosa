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
  | CFloat f -> float ppf f
  | CBool b -> bool ppf b

let pp_unop ppf = function
  | Not -> string ppf "~"
  | Neg -> string ppf "-"
  | FNeg -> string ppf "-."
  | IsSome -> string ppf "?"

let pp_binop ppf = function
  | And -> string ppf "&&"
  | Or -> string ppf "||"
  | Implies -> string ppf "=>"
  | Add -> string ppf "+"
  | Sub -> string ppf "-"
  | Mul -> string ppf "*"
  | Div -> string ppf "/"
  | FAdd -> string ppf "+."
  | FSub -> string ppf "-."
  | FMul -> string ppf "*."
  | FDiv -> string ppf "/."
  | Eq -> string ppf "=="
  | Neq -> string ppf "!="
  | Lt -> string ppf "<"
  | Leq -> string ppf "<="
  | Gt -> string ppf ">"
  | Geq -> string ppf ">="
  | FLt -> string ppf "<."
  | FLeq -> string ppf "<=."
  | FGt -> string ppf ">."
  | FGeq -> string ppf ">=."

let rec pp_expr ppf expr =
  match expr.expr_desc with
  | EVar id -> string ppf id
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

let pp_channel ppf c =
  pf ppf "channel %s : %a = %a" c.channel_name Type.pp c.channel_ty
    (braces (list ~sep:semi pp_expr))
    c.channel_elems

let pp_port ppf { port_name; port_async } =
  if port_async then pf ppf "@[async@;%s@]" port_name else string ppf port_name

let pp_period ppf (time, unit) =
  let pp_unit ppf =
    let open Ptree in
    function
    | Ms -> string ppf "ms"
  in
  pf ppf "@[%d@;%a@]" time pp_unit unit

let pp_node ppf n =
  pf ppf "@[<hv0>@[<hov>node@ %s@]@;implements@;%s@;%a@ -->@ %a@;every@;%a@]"
    n.node_name n.node_implements
    (parens (list ~sep:comma pp_port))
    n.node_inputs
    (parens (list ~sep:comma pp_port))
    n.node_outputs pp_period n.node_period

let pp ppf { protos; steps; channels; nodes } =
  pf ppf "@[<v>%a@;%a@;%a@;%a@]@." (list pp_proto) protos (list pp_step) steps
    (list pp_channel) channels (list pp_node) nodes

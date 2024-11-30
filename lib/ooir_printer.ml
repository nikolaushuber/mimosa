open Ooir
open Fmt

let pp_expr ppf e =
  match e.expr_desc with
  | Var s -> string ppf s
  | StateVar s -> string ppf s
  | Const c -> Ttree_printer.pp_const ppf c
  | GlobalConst id -> Lident.pp ppf id
  | None -> string ppf "None"
  | Some s -> pf ppf "Some %s" s
  | UnOp (op, e) -> pf ppf "%a %s" Ttree_printer.pp_unop op e
  | BinOp (op, e1, e2) -> pf ppf "%s %a %s" e1 Ttree_printer.pp_binop op e2

let rec pp_instr ppf = function
  | Assign (lhs, rhs) -> pf ppf "%s = %a" lhs pp_expr rhs
  | StateAssign (lhs, rhs) -> pf ppf "%s := %a" lhs pp_expr rhs
  | TupleConstr (lhs, rhs) -> pf ppf "%s = %a" lhs (list ~sep:comma string) rhs
  | TupleDestr (lhs, rhs) -> pf ppf "%a = %s" (list ~sep:comma string) lhs rhs
  | Reset (f, self) -> pf ppf "%a.reset( %s )" Lident.pp f self
  | Return s -> pf ppf "return %s" s
  | If (c, t, e) ->
      pf ppf "@[<v>if %s@;<1 2>{%a@;<1 0>}@;<1 2>else@;<1 0>{%a@;<1 0>}@]" c
        (list ~sep:(fun ppf _ -> pf ppf ";@;<1 2>") pp_instr)
        t
        (list ~sep:(fun ppf _ -> pf ppf ";@;<1 2>") pp_instr)
        e
  | StepApp (None, f, args, self) ->
      pf ppf "%a (%a, %s)" Lident.pp f (list ~sep:comma string) args self
  | StepApp (Some v, f, args, self) ->
      pf ppf "%s = %a (%a, %s)" v Lident.pp f (list ~sep:comma string) args self
  | Either _ -> failwith "not yet implemented"

let pp_machine ppf m =
  pf ppf
    "@[<v>@[machine %s@]@;\
     memory: %a@;\
     instances: %a@;\
     reset: %a@;\
     inputs: %a@;\
     body: %a@]" m.name
    (list ~sep:comma (pair string Type.pp))
    m.memory
    (list ~sep:comma (pair string Lident.pp))
    m.instances
    (list ~sep:(fun ppf _ -> pf ppf ";@;<1 2>") pp_instr)
    m.reset
    (list ~sep:comma (pair string Type.pp))
    m.inputs
    (list ~sep:(fun ppf _ -> pf ppf ";@;<1 2>") pp_instr)
    m.def

let pp_item ppf = function
  | Machine m -> pp_machine ppf m

let pp ppf (Package (name, items)) =
  pf ppf "@[<v>@[package %s@]@;@;%a@]" name
    (list ~sep:(cut ++ cut) pp_item)
    items

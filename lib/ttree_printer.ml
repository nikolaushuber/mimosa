open Sexplib.Sexp
open Ttree

let rec sexp_of_ty =
  let open Typing.Type in
  function
  | TUnit -> Atom "unit"
  | TInt -> Atom "int"
  | TBool -> Atom "bool"
  | TReal -> Atom "real"
  | TTuple ts -> List (List.map sexp_of_ty ts)
  | TOption t -> List [ sexp_of_ty t; Atom "opt" ]
  | TFunc (t1, t2) -> List [ sexp_of_ty t1; Atom "->"; sexp_of_ty t2 ]
  | TVar i -> Atom (string_of_int i)

let rec sexp_of_pattern pat =
  let ty = sexp_of_ty pat.pat_ty in
  match pat.pat_desc with
  | PAny -> List [ Atom "_"; Atom ":"; ty ]
  | PUnit -> List [ Atom "()"; Atom ":"; ty ]
  | PVar v -> List [ Atom v; Atom ":"; ty ]
  | PTuple ps -> List (List.map sexp_of_pattern ps)

let sexp_of_const = function
  | CUnit -> Atom "()"
  | CInt i -> Atom (string_of_int i)
  | CReal r -> Atom (string_of_float r)
  | CBool b -> Atom (string_of_bool b)

let sexp_of_unop = function
  | Not -> Atom "~"
  | Neg -> Atom "-"
  | RNeg -> Atom "-."
  | IsSome -> Atom "?"

let sexp_of_binop = function
  | And -> Atom "&&"
  | Or -> Atom "||"
  | Implies -> Atom "=>"
  | Add -> Atom "+"
  | Sub -> Atom "-"
  | Mul -> Atom "*"
  | Div -> Atom "/"
  | RAdd -> Atom "+."
  | RSub -> Atom "-."
  | RMul -> Atom "*."
  | RDiv -> Atom "/."
  | Eq -> Atom "=="
  | Neq -> Atom "!="
  | Lt -> Atom "<"
  | Leq -> Atom "<="
  | Gt -> Atom ">"
  | Geq -> Atom ">="
  | RLt -> Atom "<."
  | RLeq -> Atom "<=."
  | RGt -> Atom ">."
  | RGeq -> Atom ">=."

let rec sexp_of_expr expr =
  let ty = sexp_of_ty expr.expr_ty in
  let desc =
    match expr.expr_desc with
    | EVar id -> (
        match id with
        | Lident.Lident s -> Atom s
        | Ldot (p, s) -> List [ Atom p; Atom s ])
    | EConst c -> sexp_of_const c
    | EUnOp (op, e) -> List [ sexp_of_unop op; sexp_of_expr e ]
    | EBinOp (op, e1, e2) ->
        List [ sexp_of_binop op; sexp_of_expr e1; sexp_of_expr e2 ]
    | EEither (e1, e2) ->
        List [ Atom "either"; sexp_of_expr e1; sexp_of_expr e2 ]
    | ETuple es -> List (List.map sexp_of_expr es)
    | EIf (c, t, e) ->
        List [ Atom "ite"; sexp_of_expr c; sexp_of_expr t; sexp_of_expr e ]
    | EApp (f, e) ->
        let f' =
          match f with
          | Lident.Lident s -> Atom s
          | Ldot (p, s) -> List [ Atom p; Atom s ]
        in
        List [ f'; sexp_of_expr e ]
    | EMatch _ -> failwith "not yet implemented"
    | EArrow (e1, e2) -> List [ Atom "->"; sexp_of_expr e1; sexp_of_expr e2 ]
    | EFby (e1, e2) -> List [ Atom "fby"; sexp_of_expr e1; sexp_of_expr e2 ]
    | EPre e -> List [ Atom "pre"; sexp_of_expr e ]
    | ENone -> Atom "None"
    | ESome e -> List [ Atom "Some"; sexp_of_expr e ]
  in
  match expr.expr_desc with
  | ETuple _ -> desc
  | _ -> List [ desc; Atom ":"; ty ]

let sexp_of_step step =
  let name = Atom step.step_name in
  let input' = sexp_of_pattern step.step_input in
  let output' = sexp_of_pattern step.step_output in
  let defs' =
    List
      (List.map
         (fun (lhs, rhs) ->
           let lhs' = sexp_of_pattern lhs in
           let rhs' = sexp_of_expr rhs in
           List [ lhs'; rhs' ])
         step.step_def)
  in
  List [ Atom "step"; name; input'; output'; defs' ]

let sexp_of_item = function
  | Step s -> sexp_of_step s

let sexp_of_package pack = List (List.map sexp_of_item pack)
let sexp_of_t = sexp_of_package

let pp ppf t =
  let sexp = sexp_of_t t in
  Sexplib.Sexp.pp_hum_indent 2 ppf sexp

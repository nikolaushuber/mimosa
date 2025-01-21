open Sexplib.Sexp
open Ptree

let rec sexp_of_core_type ty = sexp_of_core_type_desc ty.type_desc

and sexp_of_core_type_desc = function
  | Type_var n -> Atom n
  | Type_tuple tys -> List (List.map sexp_of_core_type tys)
  | Type_option t -> List [ Atom "?"; sexp_of_core_type t ]
  | Type_int -> Atom "int"
  | Type_bool -> Atom "bool"
  | Type_float -> Atom "real"

let rec sexp_of_pattern pat =
  let desc = sexp_of_pattern_desc pat.pat_desc in
  match pat.pat_ty with
  | None -> desc
  | Some ty -> List [ desc; sexp_of_core_type ty ]

and sexp_of_pattern_desc = function
  | Pat_any -> Atom "_"
  | Pat_unit -> Atom "()"
  | Pat_var name -> Atom name.txt
  | Pat_tuple ps -> List (List.map sexp_of_pattern ps)

let sexp_of_constant = function
  | Const_int i -> Atom (string_of_int i)
  | Const_bool b -> Atom (string_of_bool b)
  | Const_float f -> Atom (string_of_float f)
  | Const_unit -> Atom "()"

let rec sexp_of_unop unop = sexp_of_unop_desc unop.unop_desc

and sexp_of_unop_desc = function
  | Unop_not -> Atom "not"
  | Unop_neg -> Atom "-"
  | Unop_fneg -> Atom "-."
  | Unop_is_some -> Atom "?"

let rec sexp_of_binop binop = sexp_of_binop_desc binop.binop_desc

and sexp_of_binop_desc = function
  | Binop_and -> Atom "&&"
  | Binop_or -> Atom "||"
  | Binop_implies -> Atom "=>"
  | Binop_add -> Atom "+"
  | Binop_sub -> Atom "-"
  | Binop_mul -> Atom "*"
  | Binop_div -> Atom "/"
  | Binop_fadd -> Atom "+."
  | Binop_fsub -> Atom "-."
  | Binop_fmul -> Atom "*."
  | Binop_fdiv -> Atom "/."
  | Binop_eq -> Atom "=="
  | Binop_neq -> Atom "!="
  | Binop_lt -> Atom "<"
  | Binop_leq -> Atom "<="
  | Binop_gt -> Atom ">"
  | Binop_geq -> Atom ">="
  | Binop_flt -> Atom "<."
  | Binop_fleq -> Atom "<=."
  | Binop_fgt -> Atom ">."
  | Binop_fgeq -> Atom ">=."

let rec sexp_of_expression expr = sexp_of_expr_desc expr.expr_desc

and sexp_of_expr_desc = function
  | Expr_ident id -> Atom id
  | Expr_constant c -> sexp_of_constant c
  | Expr_unop (op, e) ->
      let op' = sexp_of_unop op in
      let e' = sexp_of_expression e in
      List [ op'; e' ]
  | Expr_binop (op, e1, e2) ->
      let op' = sexp_of_binop op in
      let e1' = sexp_of_expression e1 in
      let e2' = sexp_of_expression e2 in
      List [ op'; e1'; e2' ]
  | Expr_either (e1, e2) ->
      let e1' = sexp_of_expression e1 in
      let e2' = sexp_of_expression e2 in
      List [ Atom "either"; e1'; e2' ]
  | Expr_tuple es -> List (List.map sexp_of_expression es)
  | Expr_ite (i, t, e) ->
      let i' = sexp_of_expression i in
      let t' = sexp_of_expression t in
      let e' = sexp_of_expression e in
      List [ Atom "if"; i'; t'; e' ]
  | Expr_apply (f, e) ->
      let f' = sexp_of_expression f in
      let e' = sexp_of_expression e in
      List [ f'; e' ]
  | Expr_arrow (e1, e2) ->
      let e1' = sexp_of_expression e1 in
      let e2' = sexp_of_expression e2 in
      List [ Atom "->"; e1'; e2' ]
  | Expr_fby (e1, e2) ->
      let e1' = sexp_of_expression e1 in
      let e2' = sexp_of_expression e2 in
      List [ Atom "fby"; e1'; e2' ]
  | Expr_pre e ->
      let e' = sexp_of_expression e in
      List [ Atom "pre"; e' ]
  | Expr_none -> Atom "None"
  | Expr_some e ->
      let e' = sexp_of_expression e in
      List [ Atom "Some"; e' ]

let sexp_of_step step =
  let name = Atom step.step_name.txt in
  let input' = sexp_of_pattern step.step_input in
  let output' = sexp_of_pattern step.step_output in
  let defs' =
    List
      (List.map
         (fun (lhs, rhs) ->
           let lhs' = sexp_of_pattern lhs in
           let rhs' = sexp_of_expression rhs in
           List [ lhs'; rhs' ])
         step.step_def)
  in
  List [ Atom "step"; name; input'; output'; defs' ]

let sexp_of_proto p =
  let name = Atom p.proto_name.txt in
  let input' = sexp_of_pattern p.proto_input in
  let output' = sexp_of_pattern p.proto_output in
  List [ Atom "step"; name; input'; output' ]

let sexp_of_node _ = assert false
let sexp_of_channel _ = assert false

let rec sexp_of_item item = sexp_of_item_desc item.item_desc

and sexp_of_item_desc = function
  | Step s -> sexp_of_step s
  | Proto p -> sexp_of_proto p
  | Node n -> sexp_of_node n
  | Channel c -> sexp_of_channel c

let sexp_of_t items = List (List.map sexp_of_item items)

let pp ppf t =
  let sexp = sexp_of_t t in
  Sexplib.Sexp.pp_hum_indent 2 ppf sexp

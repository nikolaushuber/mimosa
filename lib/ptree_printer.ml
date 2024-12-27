open Sexplib.Sexp
open Ptree

let rec sexp_of_core_type ty = sexp_of_core_type_desc ty.ptype_desc

and sexp_of_core_type_desc = function
  | Ptype_var n -> Atom n
  | Ptype_tuple tys -> List (List.map sexp_of_core_type tys)
  | Ptype_option t -> List [ Atom "?"; sexp_of_core_type t ]
  | Ptype_int -> Atom "int"
  | Ptype_bool -> Atom "bool"
  | Ptype_real -> Atom "real"

let rec sexp_of_pattern pat =
  let desc = sexp_of_pattern_desc pat.ppat_desc in
  match pat.ppat_ty with
  | None -> desc
  | Some ty -> List [ desc; sexp_of_core_type ty ]

and sexp_of_pattern_desc = function
  | Ppat_any -> Atom "_"
  | Ppat_unit -> Atom "()"
  | Ppat_var name -> Atom name.txt
  | Ppat_tuple ps -> List (List.map sexp_of_pattern ps)

let sexp_of_constant = function
  | Pconst_int i -> Atom (string_of_int i)
  | Pconst_bool b -> Atom (string_of_bool b)
  | Pconst_real r -> Atom (string_of_float r)
  | Pconst_unit -> Atom "()"

let rec sexp_of_unop unop = sexp_of_unop_desc unop.punop_desc

and sexp_of_unop_desc = function
  | Punop_not -> Atom "not"
  | Punop_neg -> Atom "-"
  | Punop_rneg -> Atom "-."
  | Punop_is_some -> Atom "?"

let rec sexp_of_binop binop = sexp_of_binop_desc binop.pbinop_desc

and sexp_of_binop_desc = function
  | Pbinop_and -> Atom "&&"
  | Pbinop_or -> Atom "||"
  | Pbinop_implies -> Atom "=>"
  | Pbinop_add -> Atom "+"
  | Pbinop_sub -> Atom "-"
  | Pbinop_mul -> Atom "*"
  | Pbinop_div -> Atom "/"
  | Pbinop_radd -> Atom "+."
  | Pbinop_rsub -> Atom "-."
  | Pbinop_rmul -> Atom "*."
  | Pbinop_rdiv -> Atom "/."
  | Pbinop_eq -> Atom "=="
  | Pbinop_neq -> Atom "!="
  | Pbinop_lt -> Atom "<"
  | Pbinop_leq -> Atom "<="
  | Pbinop_gt -> Atom ">"
  | Pbinop_geq -> Atom ">="
  | Pbinop_rlt -> Atom "<."
  | Pbinop_rleq -> Atom "<=."
  | Pbinop_rgt -> Atom ">."
  | Pbinop_rgeq -> Atom ">=."

let rec sexp_of_expression expr = sexp_of_expr_desc expr.pexpr_desc

and sexp_of_expr_desc = function
  | Pexpr_ident id -> (
      match id.txt with
      | Lident.Lident name -> Atom name
      | Ldot (pack, name) -> List [ Atom pack; Atom name ])
  | Pexpr_constant c -> sexp_of_constant c
  | Pexpr_unop (op, e) ->
      let op' = sexp_of_unop op in
      let e' = sexp_of_expression e in
      List [ op'; e' ]
  | Pexpr_binop (op, e1, e2) ->
      let op' = sexp_of_binop op in
      let e1' = sexp_of_expression e1 in
      let e2' = sexp_of_expression e2 in
      List [ op'; e1'; e2' ]
  | Pexpr_either (e1, e2) ->
      let e1' = sexp_of_expression e1 in
      let e2' = sexp_of_expression e2 in
      List [ Atom "either"; e1'; e2' ]
  | Pexpr_tuple es -> List (List.map sexp_of_expression es)
  | Pexpr_ite (i, t, e) ->
      let i' = sexp_of_expression i in
      let t' = sexp_of_expression t in
      let e' = sexp_of_expression e in
      List [ Atom "if"; i'; t'; e' ]
  | Pexpr_apply (f, e) ->
      let f' = sexp_of_expression f in
      let e' = sexp_of_expression e in
      List [ f'; e' ]
  | Pexpr_match (e, cases) ->
      let e' = sexp_of_expression e in
      let cases' = List.map sexp_of_case cases in
      List [ Atom "match"; e'; List cases' ]
  | Pexpr_arrow (e1, e2) ->
      let e1' = sexp_of_expression e1 in
      let e2' = sexp_of_expression e2 in
      List [ Atom "->"; e1'; e2' ]
  | Pexpr_fby (e1, e2) ->
      let e1' = sexp_of_expression e1 in
      let e2' = sexp_of_expression e2 in
      List [ Atom "fby"; e1'; e2' ]
  | Pexpr_pre e ->
      let e' = sexp_of_expression e in
      List [ Atom "pre"; e' ]
  | Pexpr_none -> Atom "None"
  | Pexpr_some e ->
      let e' = sexp_of_expression e in
      List [ Atom "Some"; e' ]

and sexp_of_case case =
  let lhs' = sexp_of_pattern case.pcase_lhs in
  let rhs' = sexp_of_expression case.pcase_rhs in
  List [ lhs'; rhs' ]

let sexp_of_step step =
  let name = Atom step.pstep_name.txt in
  let input' = sexp_of_pattern step.pstep_input in
  let output' = sexp_of_pattern step.pstep_output in
  let defs' =
    List
      (List.map
         (fun (lhs, rhs) ->
           let lhs' = sexp_of_pattern lhs in
           let rhs' = sexp_of_expression rhs in
           List [ lhs'; rhs' ])
         step.pstep_def)
  in
  List [ Atom "step"; name; input'; output'; defs' ]

let sexp_of_proto p =
  let name = Atom p.pproto_name.txt in
  let input' = sexp_of_pattern p.pproto_input in
  let output' = sexp_of_pattern p.pproto_output in
  List [ Atom "step"; name; input'; output' ]

let rec sexp_of_package_item item = sexp_of_package_item_desc item.ppack_item

and sexp_of_package_item_desc = function
  | Ppack_step s -> sexp_of_step s
  | Ppack_proto p -> sexp_of_proto p
  | Ppack_node _ -> failwith "Not yet implemented"
  | Ppack_link _ -> failwith "Not yet implemented"

let sexp_of_package pack =
  let name = Atom pack.ppack_name.txt in
  let items' = List (List.map sexp_of_package_item pack.ppack_items) in
  List [ Atom "package"; name; items' ]

let sexp_of_t = sexp_of_package

let pp ppf t =
  let sexp = sexp_of_t t in
  Sexplib.Sexp.pp_hum_indent 2 ppf sexp

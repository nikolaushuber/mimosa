open C_ast
open Fmt

let list ?(sep = cut) ?(suffix = nop) pp_v ppf v =
  match v with
  | [] -> ()
  | v -> pf ppf "%a%a" (list ~sep pp_v) v suffix ()

let rec pp_type ppf = function
  | TVoid -> string ppf "void"
  | TInt -> string ppf "int"
  | TFloat -> string ppf "float"
  | TBool -> string ppf "bool"
  | TEnum s -> pf ppf "enum@ %s" s
  | TStruct s -> pf ppf "struct@ %s" s
  | TPointer t -> pf ppf "%a *" pp_type t
  | TAlias t -> string ppf t

let pp_const ppf = function
  | CInt i -> int ppf i
  | CBool b -> bool ppf b
  | CFloat f -> float ppf f

let fmt_of_unop : Ttree.unop -> (_, _, _) format =
  let open Ttree in
  function
  | Not -> "@[!(%a)@]"
  | Neg | RNeg -> "@[(-%a)@]"
  | _ -> assert false

let fmt_of_binop : Ttree.binop -> (_, _, _) format =
  let open Ttree in
  function
  | And -> "@[(%a@ &&@ %a)@]"
  | Or -> "@[(%a@ ||@ %a)@]"
  | Add | RAdd -> "@[(%a@ +@ %a)@]"
  | Sub | RSub -> "@[(%a@ -@ %a)@]"
  | Mul | RMul -> "@[(%a@ *@ %a)@]"
  | Div | RDiv -> "@[(%a@ /@ %a)@]"
  | Eq -> "@[(%a@ ==@ %a)@]"
  | Neq -> "@[(%a@ !=@ %a)@]"
  | Lt -> "@[(%a@ <@ %a)@]"
  | Leq -> "@[(%a@ <=@ %a)@]"
  | Gt -> "@[(%a@ >@ %a)@]"
  | Geq -> "@[(%a@ >=@ %a)@]"
  | _ -> assert false

let rec pp_expr ppf = function
  | EConst c -> pp_const ppf c
  | EVar s -> string ppf s
  | EAddr (EVar v) -> pf ppf "&%s" v
  | EAddr e -> pf ppf "&(%a)" pp_expr e
  | EArrow (EVar l, EVar r) -> pf ppf "@[%s->%s@]" l r
  | EArrow (lhs, rhs) -> pf ppf "@[(%a)->(%a)@]" pp_expr lhs pp_expr rhs
  | EDot (EVar l, EVar r) -> pf ppf "@[%s.%s@]" l r
  | EDot (lhs, rhs) -> pf ppf "@[(%a).(%a)@]" pp_expr lhs pp_expr rhs
  | EUnOp (op, e) ->
      let fmt = fmt_of_unop op in
      pf ppf fmt pp_expr e
  | EBinOp (op, e1, e2) ->
      let fmt = fmt_of_binop op in
      pf ppf fmt pp_expr e1 pp_expr e2
  | ECall (f, args) -> pf ppf "@[%s(%a)@]" f (list ~sep:comma pp_expr) args

let pp_lhs ppf =
  let rec aux nested ppf = function
    | LVar s -> string ppf s
    | LArrow (l1, l2) ->
        let fmt : (_, _, _) format = if nested then "(%a->%a)" else "%a->%a" in
        pf ppf fmt (aux true) l1 (aux true) l2
    | LDot (l1, l2) ->
        let fmt : (_, _, _) format = if nested then "(%a.%a)" else "%a->%a" in
        pf ppf fmt (aux true) l1 (aux true) l2
  in
  aux false ppf

let pp_attr ppf = function
  | Static -> string ppf "static "
  | Extern -> string ppf "extern "
  | Const -> string ppf "const "
  | Inline -> string ppf "inline "

let rec pp_stmt ppf = function
  | SExpr e -> pf ppf "%a;" pp_expr e
  | SVarDecl (attrs, ty, n) ->
      pf ppf "@[%a%a %s;@]" (list pp_attr) attrs pp_type ty n
  | SVarDef (attrs, ty, n, e) ->
      pf ppf "@[%a%a %s@ =@ %a;@]" (list pp_attr) attrs pp_type ty n pp_expr e
  | SAssign (lhs, rhs) -> pf ppf "@[%a@ =@ %a;@]" pp_lhs lhs pp_expr rhs
  | SReturn e -> pf ppf "@[<hov2>return %a;@]" pp_expr e
  | SIf (c, t, e) ->
      let fmt : (_, _, _) format =
        "@[<v2>@[<hov2>if@ (%a)@]@;\
         <0 -2>{@;\
         %a@;\
         <0 -2>} else@;\
         <0 -2>{@;\
         %a@;\
         <0 -2>}@]"
      in
      pf ppf fmt pp_expr c (list pp_stmt) t (list pp_stmt) e
  | SSwitch (e, cases) ->
      pf ppf "@[<v2>@[<hov2>switch@ (%a)@]@;<0 -2>{@;%a@;<0 -2>}@]" pp_expr e
        (list pp_case) cases

and pp_case ppf (Case (pat, stmts)) =
  pf ppf "@[<v2>@[<hov2>case@ %a:@]@;%a@;break;@]" pp_case_pattern pat
    (list ~sep:cut pp_stmt) stmts

and pp_case_pattern ppf = function
  | EnumCase s -> string ppf s

let pp_preproc ppf = function
  | PMacroApp (m, args) -> pf ppf "%s(%a)" m (list pp_expr) args
  | PIncl s -> pf ppf "#include \"%s\"" s
  | PIf s -> pf ppf "#ifdef %s" s
  | PIfNot s -> pf ppf "#ifndef %s" s
  | PDef (s, so) -> pf ppf "#define %s %a" s (option string) so
  | PEndIf -> pf ppf "#endif"

let pp_param ppf (name, ty) =
  pf ppf "@[<hov2>%a@]" (pair ~sep:sp pp_type string) (ty, name)

let pp_global ppf = function
  | GEnum (name, vals) ->
      pf ppf "@[<v2>@[<hv2>enum@;%s@]@;<0 -2>{@;%a@;<0 -2>};@]" name
        (list ~sep:semi string) vals
  | GStruct (name, fields) ->
      pf ppf "@[<v2>@[<hv2>struct@;%s@]@;<0 -2>{@;%a@;<0 -2>};@]" name
        (list ~sep:semi ~suffix:(fun ppf _ -> pf ppf ";") pp_param)
        fields
  | GFunc (name, args, ty, body) ->
      pf ppf "@[<v2>@[<hov 2>%a %s (%a)@]@;<0 -2>{@;%a@;<0 -2>}@]" pp_type ty
        name (list ~sep:comma pp_param) args (list ~sep:cut pp_stmt) body
  | GProto (name, tys, ret) ->
      pf ppf "%a %s (%a);" pp_type ret name (list ~sep:comma pp_type) tys
  | GVarDecl (attrs, ty, name) ->
      pf ppf "%a%a %s;" (list pp_attr) attrs pp_type ty name
  | GVarDef (attrs, ty, name, e) ->
      pf ppf "%a%a %s = %a;" (list pp_attr) attrs pp_type ty name pp_expr e
  | GPre p -> pp_preproc ppf p

let pp ppf = pf ppf "@[<v>%a@]@." (list ~sep:(cut ++ cut) pp_global)

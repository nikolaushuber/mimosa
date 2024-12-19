open C_ast

(* TYPES *)
let tvoid = TVoid
let tint = TInt
let tfloat = TFloat
let tbool = TBool
let tenum s = TEnum s
let tstruct s = TStruct s
let tpointer t = TPointer t
let talias s = TAlias s

(* CONSTANTS *)
let const_int i = CInt i
let const_bool b = CBool b
let const_float f = CFloat f

(* EXPRESSIONS *)
let expr_const c = EConst c
let expr_var v = EVar v
let expr_addr e = EAddr e
let expr_arrow e1 e2 = EArrow (e1, e2)
let expr_dot e1 e2 = EDot (e1, e2)
let expr_unop op e = EUnOp (op, e)
let expr_binop op e1 e2 = EBinOp (op, e1, e2)
let expr_call f args = ECall (f, args)

(* LEFT-HAND SIDES *)
let lhs_var v = LVar v
let lhs_arrow l1 l2 = LArrow (l1, l2)
let lhs_dot l1 l2 = LDot (l1, l2)

(* ATTRIBUTES *)
let attr_static = Static
let attr_extern = Extern
let attr_const = Const
let attr_inline = Inline

(* STATEMENTS *)
let stmt_expr e = SExpr e
let stmt_var_decl attrs ty v = SVarDecl (attrs, ty, v)
let stmt_var_def attrs ty v e = SVarDef (attrs, ty, v, e)
let stmt_assign lhs e = SAssign (lhs, e)
let stmt_return e = SReturn e
let stmt_if c t e = SIf (c, t, e)
let stmt_switch e cases = SSwitch (e, cases)
let case_enum s = EnumCase s
let case pat stmts = Case (pat, stmts)

(* PREPROCESSOR DIRECTIVES *)
let pre_macro_app name args = PMacroApp (name, args)
let pre_include s = PIncl s
let pre_if s = PIf s
let pre_if_not s = PIfNot s
let pre_def name arg = PDef (name, arg)
let pre_end_if = PEndIf

(* GLOBAL SYMBOLS *)
let enum name vals = GEnum (name, vals)
let struct_ name fields = GStruct (name, fields)
let func name args ret body = GFunc (name, args, ret, body)
let proto name tys ret = GProto (name, tys, ret)
let var_decl attrs ty name = GVarDecl (attrs, ty, name)
let var_def attrs ty name def = GVarDef (attrs, ty, name, def)
let preproc p = GPre p

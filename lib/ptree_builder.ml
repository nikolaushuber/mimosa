open Ptree

let noloc = Location.none
let located ?(loc = noloc) id = Location.{ txt = id; loc }
let mk_ty type_desc type_loc = { type_desc; type_loc }
let tvar ?(loc = noloc) name = mk_ty (Type_var name) loc
let ttuple ?(loc = noloc) tys = mk_ty (Type_tuple tys) loc
let toption ?(loc = noloc) ty = mk_ty (Type_option ty) loc
let tint ?(loc = noloc) () = mk_ty Type_int loc
let tbool ?(loc = noloc) () = mk_ty Type_bool loc
let tfloat ?(loc = noloc) () = mk_ty Type_float loc
let mk_pattern pat_desc pat_ty pat_loc = { pat_desc; pat_ty; pat_loc }
let pany ?(loc = noloc) ty = mk_pattern Pat_any ty loc
let punit ?(loc = noloc) () = mk_pattern Pat_unit None loc
let pvar ?(loc = noloc) ty name = mk_pattern (Pat_var name) ty loc
let ptuple ?(loc = noloc) ty tup = mk_pattern (Pat_tuple tup) ty loc
let mk_expr expr_desc expr_loc = { expr_desc; expr_loc }
let eident ?(loc = noloc) id = mk_expr (Expr_ident id) loc
let constant_int i = Const_int i
let constant_bool b = Const_bool b
let constant_float f = Const_float f
let econstant ?(loc = noloc) const = mk_expr (Expr_constant const) loc
let eunit ?(loc = noloc) () = econstant ~loc Const_unit
let eint ?(loc = noloc) i = econstant ~loc (constant_int i)
let ebool ?(loc = noloc) b = econstant ~loc (constant_bool b)
let efloat ?(loc = noloc) f = econstant ~loc (constant_float f)
let evar ?(loc = noloc) n = mk_expr (Expr_ident n) loc
let eunop ?(loc = noloc) op e = mk_expr (Expr_unop (op, e)) loc
let ebinop ?(loc = noloc) op e1 e2 = mk_expr (Expr_binop (op, e1, e2)) loc
let eeither ?(loc = noloc) e1 e2 = mk_expr (Expr_either (e1, e2)) loc
let etuple ?(loc = noloc) exprs = mk_expr (Expr_tuple exprs) loc
let eite ?(loc = noloc) i t e = mk_expr (Expr_ite (i, t, e)) loc
let eapply ?(loc = noloc) func arg = mk_expr (Expr_apply (func, arg)) loc
let earrow ?(loc = noloc) e1 e2 = mk_expr (Expr_arrow (e1, e2)) loc
let efby ?(loc = noloc) e1 e2 = mk_expr (Expr_fby (e1, e2)) loc
let epre ?(loc = noloc) e = mk_expr (Expr_pre e) loc
let enone ?(loc = noloc) () = mk_expr Expr_none loc
let esome ?(loc = noloc) e = mk_expr (Expr_some e) loc
let unop ?(loc = noloc) unop_desc = { unop_desc; unop_loc = loc }
let binop ?(loc = noloc) binop_desc = { binop_desc; binop_loc = loc }

let port ?(loc = noloc) port_name port_async =
  { port_name; port_async; port_loc = loc }

let ms = Ms

let period ?(loc = noloc) period_time period_unit =
  { period_time; period_unit; period_loc = loc }

let item ?(loc = noloc) item_desc = { item_desc; item_loc = loc }

let step ?(loc = noloc) step_name step_input step_output step_def =
  item ~loc
    (Step { step_name; step_input; step_output; step_def; step_loc = loc })

let proto ?(loc = noloc) proto_name proto_input proto_output =
  item ~loc (Proto { proto_name; proto_input; proto_output; proto_loc = loc })

let node ?(loc = noloc) node_name node_implements node_inputs node_outputs
    node_period =
  item ~loc
    (Node
       {
         node_name;
         node_implements;
         node_inputs;
         node_outputs;
         node_period;
         node_loc = loc;
       })

let channel ?(loc = noloc) name ty elems =
  item ~loc
    (Channel
       {
         channel_name = name;
         channel_type = ty;
         channel_elems = elems;
         channel_loc = loc;
       })

let package items = items

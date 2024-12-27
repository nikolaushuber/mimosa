open Ptree

let noloc = Location.none
let located ?(loc = noloc) id = Location.{ txt = id; loc }
let mk_ty ptype_desc ptype_loc = { ptype_desc; ptype_loc }
let tvar ?(loc = noloc) name = mk_ty (Ptype_var name) loc
let ttuple ?(loc = noloc) tys = mk_ty (Ptype_tuple tys) loc
let toption ?(loc = noloc) ty = mk_ty (Ptype_option ty) loc
let tint ?(loc = noloc) () = mk_ty Ptype_int loc
let tbool ?(loc = noloc) () = mk_ty Ptype_bool loc
let treal ?(loc = noloc) () = mk_ty Ptype_real loc
let mk_pattern ppat_desc ppat_ty ppat_loc = { ppat_desc; ppat_ty; ppat_loc }
let pany ?(loc = noloc) ty = mk_pattern Ppat_any ty loc
let punit ?(loc = noloc) () = mk_pattern Ppat_unit None loc
let pvar ?(loc = noloc) ty name = mk_pattern (Ppat_var name) ty loc
let ptuple ?(loc = noloc) ty tup = mk_pattern (Ppat_tuple tup) ty loc
let mk_expr pexpr_desc pexpr_loc = { pexpr_desc; pexpr_loc }
let eident ?(loc = noloc) id = mk_expr (Pexpr_ident id) loc
let constant_int i = Pconst_int i
let constant_bool b = Pconst_bool b
let constant_real r = Pconst_real r
let econstant ?(loc = noloc) const = mk_expr (Pexpr_constant const) loc
let eunit ?(loc = noloc) () = econstant ~loc Pconst_unit
let eint ?(loc = noloc) i = econstant ~loc (constant_int i)
let ebool ?(loc = noloc) b = econstant ~loc (constant_bool b)
let ereal ?(loc = noloc) f = econstant ~loc (constant_real f)
let evar ?(loc = noloc) n = mk_expr (Pexpr_ident n) loc
let eunop ?(loc = noloc) op e = mk_expr (Pexpr_unop (op, e)) loc
let ebinop ?(loc = noloc) op e1 e2 = mk_expr (Pexpr_binop (op, e1, e2)) loc
let eeither ?(loc = noloc) e1 e2 = mk_expr (Pexpr_either (e1, e2)) loc
let etuple ?(loc = noloc) exprs = mk_expr (Pexpr_tuple exprs) loc
let eite ?(loc = noloc) i t e = mk_expr (Pexpr_ite (i, t, e)) loc
let eapply ?(loc = noloc) func arg = mk_expr (Pexpr_apply (func, arg)) loc
let ematch ?(loc = noloc) e cases = mk_expr (Pexpr_match (e, cases)) loc
let earrow ?(loc = noloc) e1 e2 = mk_expr (Pexpr_arrow (e1, e2)) loc
let efby ?(loc = noloc) e1 e2 = mk_expr (Pexpr_fby (e1, e2)) loc
let epre ?(loc = noloc) e = mk_expr (Pexpr_pre e) loc
let enone ?(loc = noloc) () = mk_expr Pexpr_none loc
let esome ?(loc = noloc) e = mk_expr (Pexpr_some e) loc
let case pcase_lhs pcase_rhs = { pcase_lhs; pcase_rhs }
let unop ?(loc = noloc) punop_desc = { punop_desc; punop_loc = loc }
let binop ?(loc = noloc) pbinop_desc = { pbinop_desc; pbinop_loc = loc }

let step ?(loc = noloc) pstep_name pstep_input pstep_output pstep_def =
  { pstep_name; pstep_input; pstep_output; pstep_def; pstep_loc = loc }

let proto ?(loc = noloc) pproto_name pproto_input pproto_output =
  { pproto_name; pproto_input; pproto_output; pproto_loc = loc }

let port ?(loc = noloc) pport_name pport_async =
  { pport_name; pport_async; pport_loc = loc }

let ms = Ms

let period ?(loc = noloc) pperiod_time pperiod_unit =
  { pperiod_time; pperiod_unit; pperiod_loc = loc }

let node ?(loc = noloc) pnode_name pnode_implements pnode_inputs pnode_outputs
    pnode_period =
  {
    pnode_name;
    pnode_implements;
    pnode_inputs;
    pnode_outputs;
    pnode_period;
    pnode_loc = loc;
  }

let link ?(loc = noloc) plink_name plink_desc =
  { plink_name; plink_desc; plink_loc = loc }

let channel ty = Plink_channel ty
let register ty e = Plink_register (ty, e)
let package_item ?(loc = noloc) ppack_item = { ppack_item; ppack_loc = loc }

let pack_step ?(loc = noloc) step =
  { ppack_item = Ppack_step step; ppack_loc = loc }

let pack_proto ?(loc = noloc) proto =
  { ppack_item = Ppack_proto proto; ppack_loc = loc }

let pack_node ?(loc = noloc) node =
  { ppack_item = Ppack_node node; ppack_loc = loc }

let pack_link link = Ppack_link link
let package ppack_name ppack_items = { ppack_name; ppack_items }

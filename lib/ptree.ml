type 'a loc = 'a Location.loc

type core_type = { ptype_desc : core_type_desc; ptype_loc : Location.t }

and core_type_desc =
  | Ptype_var of string
  | Ptype_tuple of core_type list
  | Ptype_option of core_type
  | Ptype_int
  | Ptype_bool

type pattern = {
  ppat_desc : pattern_desc;
  ppat_ty : core_type option;
  ppat_loc : Location.t;
}

and pattern_desc =
  | Ppat_any
  | Ppat_unit
  | Ppat_var of string loc
  | Ppat_tuple of pattern list

type constant = Pconst_int of int | Pconst_bool of bool | Pconst_unit

type unop = { punop_desc : unop_desc; punop_loc : Location.t }
and unop_desc = Punop_not | Punop_neg | Punop_pre | Punop_is_some

type binop = { pbinop_desc : binop_desc; pbinop_loc : Location.t }

and binop_desc =
  | Pbinop_and
  | Pbinop_or
  | Pbinop_implies
  | Pbinop_add
  | Pbinop_sub
  | Pbinop_mul
  | Pbinop_div
  | Pbinop_eq
  | Pbinop_neq
  | Pbinop_lt
  | Pbinop_leq
  | Pbinop_gt
  | Pbinop_geq
  | Pbinop_arrow
  | Pbinop_fby

type expression = { pexpr_desc : expr_desc; pexpr_loc : Location.t }

and expr_desc =
  | Pexpr_ident of Lident.t loc
  | Pexpr_constant of constant
  | Pexpr_unop of unop * expression
  | Pexpr_binop of binop * expression * expression
  | Pexpr_either of expression * expression
  | Pexpr_tuple of expression list
  | Pexpr_ite of expression * expression * expression
  | Pexpr_apply of Lident.t loc * expression
  | Pexpr_match of expression * case list
  | Pexpr_none
  | Pexpr_some of expression

and case = { pcase_lhs : pattern; pcase_rhs : expression }

type step = {
  pstep_name : string loc;
  pstep_input : pattern;
  pstep_output : pattern;
  pstep_def : (pattern * expression) list;
  pstep_loc : Location.t;
}

type port = { pport_name : string; pport_async : bool; pport_loc : Location.t }
type time_unit = Ms

type period = {
  pperiod_time : int;
  pperiod_unit : time_unit;
  pperiod_loc : Location.t;
}

type node = {
  pnode_name : string loc;
  pnode_implements : Ident.t loc;
  pnode_inputs : port list;
  pnode_outputs : port list;
  pnode_period : period;
  pnode_loc : Location.t;
}

type link = {
  plink_name : string;
  plink_desc : link_desc;
  plink_loc : Location.t;
}

and link_desc =
  | Plink_channel of core_type
  | Plink_register of core_type * expression

type package_item = { ppack_item : package_item_desc; ppack_loc : Location.t }

and package_item_desc =
  | Ppack_step of step
  | Ppack_node of node
  | Ppack_link of link

type package = { ppack_name : string loc; ppack_items : package_item list }
type t = package

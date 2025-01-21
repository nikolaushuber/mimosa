(** Parsetree as produced by the parser *)

type 'a loc = 'a Location.loc

type core_type = { type_desc : core_type_desc; type_loc : Location.t }

and core_type_desc =
  | Type_var of string
  | Type_tuple of core_type list
  | Type_option of core_type
  | Type_int
  | Type_bool
  | Type_float

type pattern = {
  pat_desc : pattern_desc;
  pat_ty : core_type option;
  pat_loc : Location.t;
}

and pattern_desc =
  | Pat_any
  | Pat_unit
  | Pat_var of string loc
  | Pat_tuple of pattern list

type constant =
  | Const_int of int
  | Const_float of float
  | Const_bool of bool
  | Const_unit

type unop = { unop_desc : unop_desc; unop_loc : Location.t }
and unop_desc = Unop_not | Unop_neg | Unop_fneg | Unop_is_some

type binop = { binop_desc : binop_desc; binop_loc : Location.t }

and binop_desc =
  | Binop_and (* Boolean operators *)
  | Binop_or
  | Binop_implies
  | Binop_add (* Integer operators *)
  | Binop_sub
  | Binop_mul
  | Binop_div
  | Binop_fadd (* Float operators *)
  | Binop_fsub
  | Binop_fmul
  | Binop_fdiv
  | Binop_eq (* Polymorphic comparison operators *)
  | Binop_neq
  | Binop_lt (* Integer comparison operators *)
  | Binop_leq
  | Binop_gt
  | Binop_geq
  | Binop_flt (* Float comparison operators *)
  | Binop_fleq
  | Binop_fgt
  | Binop_fgeq

type expression = { expr_desc : expr_desc; expr_loc : Location.t }

and expr_desc =
  | Expr_ident of string
  | Expr_constant of constant
  | Expr_unop of unop * expression
  | Expr_binop of binop * expression * expression
  | Expr_either of expression * expression
  | Expr_tuple of expression list
  | Expr_ite of expression * expression * expression
  | Expr_apply of expression * expression
  | Expr_arrow of expression * expression
  | Expr_fby of expression * expression
  | Expr_pre of expression
  | Expr_none
  | Expr_some of expression

type step = {
  step_name : string loc;
  step_input : pattern;
  step_output : pattern;
  step_def : (pattern * expression) list;
  step_loc : Location.t;
}

type proto = {
  proto_name : string loc;
  proto_input : pattern;
  proto_output : pattern;
  proto_loc : Location.t;
}

type port = { port_name : string loc; port_async : bool; port_loc : Location.t }
type time_unit = Ms

type period = {
  period_time : int;
  period_unit : time_unit;
  period_loc : Location.t;
}

type node = {
  node_name : string loc;
  node_implements : string loc;
  node_inputs : port list;
  node_outputs : port list;
  node_period : period;
  node_loc : Location.t;
}

type channel = {
  channel_name : string;
  channel_type : core_type;
  channel_elems : expression list;
  channel_loc : Location.t;
}

type item = { item_desc : item_desc; item_loc : Location.t }

and item_desc =
  | Step of step
  | Proto of proto
  | Node of node
  | Channel of channel

type t = item list

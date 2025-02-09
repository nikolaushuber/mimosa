open Type

type pat = { pat_desc : pat_desc; pat_ty : Type.t }
and pat_desc = PAny | PUnit | PVar of string | PTuple of pat list

type const = CUnit | CInt of int | CFloat of float | CBool of bool
type unop = Not | Neg | FNeg | IsSome

type binop =
  | And
  | Or
  | Implies
  | Add
  | Sub
  | Mul
  | Div
  | FAdd
  | FSub
  | FMul
  | FDiv
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | FLt
  | FLeq
  | FGt
  | FGeq

type expr = { expr_desc : expr_desc; expr_ty : Type.t }

and expr_desc =
  | EVar of string
  | EConst of const
  | EUnOp of unop * expr
  | EBinOp of binop * expr * expr
  | EEither of expr * expr
  | ETuple of expr list
  | EIf of expr * expr * expr
  | EApp of expr * expr
  | EArrow of expr * expr
  | EFby of expr * expr
  | EPre of expr
  | ENone
  | ESome of expr

type step = {
  step_name : string;
  step_input : pat;
  step_output : pat;
  step_def : (pat * expr) list;
}

type proto = { proto_name : string; proto_input : pat; proto_output : pat }

type channel = {
  channel_name : string;
  channel_ty : Type.t;
  channel_elems : expr list;
}

type node = {
  node_name : string;
  node_implements : string;
  node_inputs : port list;
  node_outputs : port list;
  node_period : period;
}

and port = { port_name : string; port_async : bool }
and period = int * Ptree.time_unit

type t = {
  protos : proto list;
  steps : step list;
  channels : channel list;
  nodes : node list;
}

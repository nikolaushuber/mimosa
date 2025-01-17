open Type

type pat = { pat_desc : pat_desc; pat_ty : Type.t }
and pat_desc = PAny | PUnit | PVar of string | PTuple of pat list

type const = CUnit | CInt of int | CReal of float | CBool of bool
type unop = Not | Neg | RNeg | IsSome

type binop =
  | And
  | Or
  | Implies
  | Add
  | Sub
  | Mul
  | Div
  | RAdd
  | RSub
  | RMul
  | RDiv
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | RLt
  | RLeq
  | RGt
  | RGeq

type expr = { expr_desc : expr_desc; expr_ty : Type.t }

and expr_desc =
  | EVar of Lident.t
  | EConst of const
  | EUnOp of unop * expr
  | EBinOp of binop * expr * expr
  | EEither of expr * expr
  | ETuple of expr list
  | EIf of expr * expr * expr
  | EApp of expr * expr
  | EMatch of expr * case list
  | EArrow of expr * expr
  | EFby of expr * expr
  | EPre of expr
  | ENone
  | ESome of expr

and case = { lhs : pat; rhs : expr }

type step = {
  step_name : string;
  step_input : pat;
  step_output : pat;
  step_def : (pat * expr) list;
}

type proto = { proto_name : string; proto_input : pat; proto_output : pat }

type link = { link_name : string; link_ty : Type.t; link_desc : link_desc }
and link_desc = Channel | Register of expr

type node = {
  node_name : string;
  node_implements : Lident.t;
  node_inputs : port list;
  node_outputs : node list;
  node_period : period;
}

and port = { port_name : Lident.t; port_aync : bool }
and period = int * Ptree.time_unit

type t = {
  pack_name : string;
  pack_dependencies : string list;
  pack_protos : proto list;
  pack_steps : step list;
  pack_links : link list;
  pack_nodes : node list;
}

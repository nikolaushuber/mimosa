type value =
  | VUndef
  | VUnit
  | VInt of int
  | VBool of bool
  | VFloat of float
  | VTuple of value list
  | VOption of value option
  | VLambda of pat * pat * equation list
  | VExtern of extern_func

and extern_func = value -> value

and expr =
  | EUndef
  | EUnit
  | EInt of int
  | EBool of bool
  | EFloat of float
  | EVar of string
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
  | ELam of pat * pat * equation list
  | EExtern of extern_func

and unop = Not | Neg | IsSome

and binop =
  | And
  | Or
  | Implies
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Gt
  | Geq
  | Lt
  | Leq

and pat = PUnit | PAny | PVar of string | PTuple of pat list
and equation = pat * expr

type node = {
  node_name : string;
  node_period : int64;
  mutable node_expr : expr;
  node_inputs : (string * bool) list;
  node_outputs : (string * bool) list;
}

type channel = value Queue.t

type state = {
  mutable time : int64;
  channels : (string, channel) Hashtbl.t;
  nodes : (string, node) Hashtbl.t;
  mutable exec_queue : (int64 * string list) list;
  mutable write_queue : (int64 * (string * value) list) list;
}

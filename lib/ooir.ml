type param = string * Type.t

type machine = {
  memory : param list;
  instances : (string * Ident.t) list;
  reset : instr list;
  inputs : param list;
  outputs : param list;
  locals : param list;
  def : instr list;
}

and instr =
  | Assign of string * expr
  | StateAssign of string * expr
  | TupleConstr of string * expr list
  | TupleDestr of string list * string
  | Reset of Ident.t * string
  | StepApp of string option * Ident.t * expr list * string option
  | Switch of expr * (string * instr list) list

and expr = { expr_desc : expr_desc; expr_ty : Type.t }

and expr_desc =
  | Var of string
  | StateVar of string
  | Const of Ttree.const
  | None
  | Some of expr
  | UnOp of Ttree.unop * expr
  | BinOp of Ttree.binop * expr * expr
  | If of expr * expr * expr

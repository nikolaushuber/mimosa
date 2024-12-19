type param = string * Type.t

type machine = {
  name : string;
  memory : param list;
  instances : (string * Lident.t) list;
  reset : instr list;
  inputs : param list;
  locals : param list;
  ret : Type.t;
  def : instr list;
  self : string;
}

and instr =
  | Assign of string * expr
  | StateAssign of string * expr
  | TupleConstr of string * string list
  | TupleDestr of string list * string
  | Reset of Lident.t * string
  | Return of string
  | If of string * instr list * instr list
  | StepApp of string option * Lident.t * string list * string
  | Either of string * string * instr list

and expr = { expr_desc : expr_desc; expr_ty : Type.t }

and expr_desc =
  | Var of string
  | StateVar of string
  | Const of Ttree.const
  | GlobalConst of Lident.t
  | None
  | Some of string
  | UnOp of Ttree.unop * string
  | BinOp of Ttree.binop * string * string

type item = Machine of machine
type t = Package of string * item list

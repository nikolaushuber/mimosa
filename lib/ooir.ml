type pattern = Norm.pat

type proto = {
  proto_name : string;
  proto_input : Type.t;
  proto_output : Type.t;
}

type machine = {
  name : string;
  memory : (string * Type.t) list;
  instances : (string * string) list;
  reset : instr list;
  input : pattern;
  locals : (string * Type.t) list;
  ret : Type.t;
  def : instr list;
  self : string;
}

and instr =
  | Assign of string * expr
  | StateAssign of string * expr
  | TupleConstr of string * string list
  | TupleDestr of string list * string
  | Reset of string * string
  | Return of string
  | If of string * instr list * instr list
  | StepApp of string option * string * string list * string
  | Either of string * string * instr list

and expr = { expr_desc : expr_desc; expr_ty : Type.t }

and expr_desc =
  | Var of string
  | StateVar of string
  | Const of Ttree.const
  | None
  | Some of string
  | UnOp of Ttree.unop * string
  | BinOp of Ttree.binop * string * string

type item = Machine of machine | Proto of proto
type t = { protos : proto list; machines : machine list }

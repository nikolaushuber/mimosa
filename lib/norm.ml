(** k-normal form *)

type pattern = { pat_desc : pat_desc; pat_ty : Type.t }
and pat_desc = PAny | PUnit | PVar of string | PTuple of string list

type base_expr = { base_expr_desc : base_expr_desc; base_expr_ty : Type.t }

and base_expr_desc =
  | EConst of Ttree.const
  | EVar of string
  | EGlobalVar of Lident.t
  | EUnOp of Ttree.unop * string
  | EBinOp of Ttree.binop * string * string
  | ENone
  | ESome of string

type expr = { expr_desc : expr_desc; expr_ty : Type.t }

and expr_desc =
  | EBase of base_expr
  | EEither of string * block
  | EIf of string * block * block
  | EApp of Lident.t * string
  | EFby of string * block
  | ETuple of string list

and block = (pattern * expr) list * base_expr

type step = string * pattern * Type.t * block * int
type item = Step of step
type t = Package of string * item list

(** k-normal form *)

type pattern =
  | PAny of Type.t
  | PUnit
  | PVar of Type.t * string
  | PTuple of pattern list

type expr =
  | EConst of Ttree.const
  | EVar of Lident.t
  | EUnOp of Ttree.unop * string
  | EBinOp of Ttree.binop * string * string
  | EEither of string * expr
  | ETuple of string list
  | EIf of string * expr * expr
  | EApp of Lident.t * string
  | EFby of string * expr
  | ENone
  | ESome of string
  | ELet of pattern * expr * expr

type step = string * pattern * expr
type item = Step of step
type t = Package of string * item list

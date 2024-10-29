type t =
  | TBool
  | TInt
  | TTuple of t list
  | TOption of t
  | TFunc of t * t
  | TVar of int

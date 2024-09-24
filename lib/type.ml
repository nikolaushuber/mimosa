type t =
  | TBool
  | TInt
  | TFloat
  | TTuple of t list
  | TOption of t
  | TFunc of t * t
  | TVar of ty_var ref

and ty_var = Alias of t | Unbound

let rec equal t1 t2 =
  match (t1, t2) with
  | TBool, TBool | TInt, TInt | TFloat, TFloat -> true
  | TTuple ts1, TTuple ts2 ->
      List.fold_left ( && ) true (List.map2 equal ts1 ts2)
  | TOption t1, TOption t2 -> equal t1 t2
  | TFunc (i1, o1), TFunc (i2, o2) -> equal i1 i2 && equal o1 o2
  | TVar { contents = Alias t1 }, t2 | t1, TVar { contents = Alias t2 } ->
      equal t1 t2
  | _ -> false

let compare t1 t2 = if equal t1 t2 then 0 else 1

let rec flatten = function
  | TVar { contents = Alias t } -> flatten t
  | t -> t

(* PRETTY PRINTING *)

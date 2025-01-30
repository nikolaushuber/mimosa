(* open Ordering
open Ptree

type init =
  | INull
  | IOne
  | IFunc of init * init
  | ITuple of init list


let check_init i1 i2 = match i1, i2 with
  | INull, INull
  | IOne, IOne
  | 

let rec offs_expr env e = match e.expr_desc with
  | Expr_ident id -> String.Map.find id env
  | Expr_constant _ -> IOne
  | Expr_unop (_, e) -> offs_expr env e
  | Expr_binop (_, e1, e2) -> Int.min (offs_expr env e1) (offs_expr env e2)
  | Expr_either (e1, e2) ->
    if offs_expr env e1 <> 0 then
      assert false;
    if offs_expr env e2 <> 0 then
      assert false;
    0
  |  *)

open Ordering
open Ptree
open Reserr

let rec check_expr env e =
  match e.expr_desc with
  | Expr_ident id -> (
      match String.Map.find_opt id env with
      | None -> ok true (* Unknown names will be reported by the type checker *)
      | Some x -> ok x)
  | Expr_constant _ -> ok true
  | Expr_unop (_, e) -> check_expr env e
  | Expr_binop (_, e1, e2) ->
      let* i1 = check_expr env e1 and* i2 = check_expr env e2 in
      (i1 && i2) |> ok
  | Expr_arrow (e1, e2) ->
      let* _ = check_expr env e2 in
      let* i1 = check_expr env e1 in
      if Bool.not i1 then
        let msg = "Left-hand side of -> must always be initialized" in
        let loc = e1.expr_loc in
        let err = Error.Uninitialized_sequence (Some msg) in
        error (err, loc)
      else ok true
  | Expr_fby (e1, e2) ->
      let* i1 = check_expr env e1 and* i2 = check_expr env e2 in
      if Bool.not i1 then
        let msg = "Left-hand side of fby must always be initialized" in
        let loc = e1.expr_loc in
        let err = Error.Uninitialized_sequence (Some msg) in
        error (err, loc)
      else if Bool.not i2 then
        let msg = "Right-hand side of fby must always be initialized" in
        let loc = e2.expr_loc in
        let err = Error.Uninitialized_sequence (Some msg) in
        error (err, loc)
      else ok true
  | Expr_either (e1, e2) ->
      let* i1 = check_expr env e1 and* i2 = check_expr env e2 in
      if Bool.not i1 then
        let msg = "Either expression must always be initialized" in
        let loc = e1.expr_loc in
        let err = Error.Uninitialized_sequence (Some msg) in
        error (err, loc)
      else if Bool.not i2 then
        let msg =
          "Default expression for either/or must always be initialized"
        in
        let loc = e2.expr_loc in
        let err = Error.Uninitialized_sequence (Some msg) in
        error (err, loc)
      else ok true
  | Expr_apply (_, e) ->
      let* i = check_expr env e in
      if Bool.not i then
        let msg =
          "Argument expression to function must always be initialized"
        in
        let loc = e.expr_loc in
        let err = Error.Uninitialized_sequence (Some msg) in
        error (err, loc)
      else ok true
  | Expr_none -> ok true
  | Expr_some e -> check_expr env e
  | Expr_pre e ->
      let* i = check_expr env e in
      if Bool.not i then
        let msg = "Pre can only be used on initalized expressions" in
        let loc = e.expr_loc in
        let err = Error.Uninitialized_sequence (Some msg) in
        error (err, loc)
      else ok false
  | Expr_tuple es ->
      fold_left
        (fun acc e ->
          let* i = check_expr env e in
          (acc && i) |> ok)
        true es
  | Expr_ite (c, t, e) ->
      let* ic = check_expr env c
      and* it = check_expr env t
      and* ie = check_expr env e in
      if Bool.not ic then
        let msg = "Condition expression must be initialized" in
        let loc = c.expr_loc in
        let err = Error.Uninitialized_sequence (Some msg) in
        error (err, loc)
      else if Bool.not it then
        let msg = "Conditional branch must be initialized" in
        let loc = t.expr_loc in
        let err = Error.Uninitialized_sequence (Some msg) in
        error (err, loc)
      else if Bool.not ie then
        let msg = "Conditional branch must be initialized" in
        let loc = e.expr_loc in
        let err = Error.Uninitialized_sequence (Some msg) in
        error (err, loc)
      else ok true

let rec check_eq env (p, e) =
  match (p.pat_desc, e.expr_desc) with
  | Pat_any, _ | Pat_unit, _ ->
      let* _ = check_expr env e in
      ok env
  | Pat_var v, _ ->
      let* i = check_expr env e in
      String.Map.add v.txt i env |> ok
  | Pat_tuple ps, Expr_tuple es ->
      if List.length ps = List.length es then
        fold_left check_eq env (List.combine ps es)
      else
        (* Will detect mismatch during type checking *)
        ok env
  | Pat_tuple _, _ ->
      let* i = check_expr env e in
      let* defs, _ = Eq_ordering.vars_of_pat p in
      List.fold_left
        (fun acc x -> String.Map.add x i acc)
        env (String.Set.to_list defs)
      |> ok

let check_step s =
  let* defs, _ = Eq_ordering.vars_of_pat s.step_input in
  let env =
    List.fold_left
      (fun acc x -> String.Map.add x true acc)
      String.Map.empty (String.Set.to_list defs)
  in
  let* env' = fold_left check_eq env s.step_def in
  let rec aux p =
    match p.pat_desc with
    | Pat_any -> ok ()
    | Pat_unit -> ok ()
    | Pat_var x ->
        if String.Map.find x.txt env' then ok ()
        else
          let msg = "Step-output must be initialized" in
          let loc = p.pat_loc in
          let err = Error.Uninitialized_sequence (Some msg) in
          error (err, loc)
    | Pat_tuple ps ->
        let* _ = map aux ps in
        ok ()
  in
  let* _ = aux s.step_output in
  ok ()

let f t =
  let* _ = map check_step t.steps in
  ok t

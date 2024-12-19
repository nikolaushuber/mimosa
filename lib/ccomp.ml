open C_builder
open Ooir

let trans_id =
  let open Lident in
  function
  | Lident s -> s
  | Ldot (p, s) -> Format.asprintf "%s__%s" p s

let rec gen_struct_name =
  let open Type in
  function
  | TInt -> "int"
  | TBool -> "bool"
  | TReal -> "float"
  | TOption t -> Format.asprintf "opt_%s" (gen_struct_name t)
  | TTuple ts ->
      Format.asprintf "tup_%s" (String.concat "_" (List.map gen_struct_name ts))
  | _ -> assert false

let trans_type =
  let open Type in
  function
  | TInt -> tint
  | TBool -> tbool
  | TReal -> tfloat
  | TOption _ as t -> tstruct (gen_struct_name t)
  | TTuple _ as t -> tstruct (gen_struct_name t)
  | TUnit -> tvoid
  | _ -> assert false

let trans_param (n, t) = (n, trans_type t)

let trans_const =
  let open Ttree in
  function
  (* if there are any units left, just turn them into booleans *)
  | CUnit -> const_bool false
  | CInt i -> const_int i
  | CReal r -> const_float r
  | CBool b -> const_bool b

let trans_expr self e =
  match e.expr_desc with
  | Var s -> expr_var s
  | StateVar s -> expr_arrow (expr_var self) (expr_var s)
  | Const c -> expr_const (trans_const c)
  | GlobalConst id -> expr_var (trans_id id)
  | None ->
      let base = gen_struct_name e.expr_ty in
      let func = Format.asprintf "none_%s" base in
      expr_call func []
  | Some s ->
      let base = gen_struct_name e.expr_ty in
      let func = Format.asprintf "some_%s" base in
      expr_call func [ expr_var s ]
  | UnOp (op, e) -> expr_unop op (expr_var e)
  | BinOp (op, e1, e2) -> expr_binop op (expr_var e1) (expr_var e2)

let rec trans_instr self = function
  | Assign (n, e) -> [ stmt_assign (lhs_var n) (trans_expr self e) ]
  | StateAssign (n, e) ->
      [ stmt_assign (lhs_arrow (lhs_var self) (lhs_var n)) (trans_expr self e) ]
  | TupleConstr (s, es) ->
      List.mapi
        (fun idx e ->
          let e' = expr_var e in
          let id = Format.asprintf "val%d" idx in
          stmt_assign (lhs_dot (lhs_var s) (lhs_var id)) e')
        es
  | TupleDestr (names, e) ->
      List.mapi
        (fun idx name ->
          let id = Format.asprintf "val%d" idx in
          stmt_assign (lhs_var name) (expr_dot (expr_var e) (expr_var id)))
        names
  | Reset (id, n) ->
      let name = trans_id id in
      let func = Format.asprintf "%s__reset" name in
      [
        stmt_expr
          (expr_call func
             [ expr_addr (expr_arrow (expr_var self) (expr_var n)) ]);
      ]
  | Return s -> [ stmt_return (expr_var s) ]
  | If (c, t, e) ->
      [
        stmt_if (expr_var c)
          (List.concat_map (trans_instr self) t)
          (List.concat_map (trans_instr self) e);
      ]
  | StepApp (lhs, func, args, inst) -> (
      let func = Format.asprintf "%s__step" (trans_id func) in
      let args' = List.map expr_var args in
      let args'' = args' @ [ expr_var inst ] in
      match lhs with
      | None -> [ stmt_expr (expr_call func args'') ]
      | Some lhs -> [ stmt_assign (lhs_var lhs) (expr_call func args'') ])
  | Either (lhs, e, default) ->
      let pat_none = case_enum "None" in
      let pat_some = case_enum "Some" in
      let none_stmts = List.concat_map (trans_instr self) default in
      let none_case = case pat_none none_stmts in
      let some_stmts =
        [ stmt_assign (lhs_var lhs) (expr_dot (expr_var e) (expr_var "expr")) ]
      in
      let some_case = case pat_some some_stmts in
      [
        stmt_switch
          (expr_dot (expr_var e) (expr_var "tag"))
          [ some_case; none_case ];
      ]

let trans_machine pack m =
  let ret_ty = trans_type m.ret in
  let base = Format.asprintf "%s__%s" pack m.name in
  let state_struct_name = Format.asprintf "%s__state_t" base in
  let state_struct =
    struct_ state_struct_name
      (List.map trans_param m.memory
      @ List.map
          (fun (name, id) ->
            let state_ty_name = Format.asprintf "%s__state_t" (trans_id id) in
            (name, tstruct state_ty_name))
          m.instances)
  in
  let self_arg = (m.self, tpointer (tstruct state_struct_name)) in
  let reset_func_name = Format.asprintf "%s__reset" base in
  let reset_func =
    func reset_func_name [ self_arg ] tvoid
      (List.concat_map (trans_instr m.self) m.reset)
  in
  let step_func_name = Format.asprintf "%s__step" base in
  let step_func =
    func step_func_name
      (List.map trans_param m.inputs @ [ self_arg ])
      ret_ty
      (List.map
         (fun p ->
           let n, ty = trans_param p in
           stmt_var_decl [] ty n)
         m.locals
      @ List.concat_map (trans_instr m.self) m.def)
  in
  [ state_struct; reset_func; step_func ]

let trans_item pack = function
  | Machine m -> trans_machine pack m

let f (Package (name, items)) : C_ast.t =
  let lower_case_name = String.lowercase_ascii name in
  List.concat_map (trans_item lower_case_name) items

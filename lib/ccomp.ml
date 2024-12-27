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
        [ stmt_assign (lhs_var lhs) (expr_dot (expr_var e) (expr_var "value")) ]
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

let trans_proto p =
  let tys = List.map snd p.proto_inputs in
  let in_tys = List.map trans_type tys in
  let ret_ty = trans_type p.proto_ret in
  [ proto p.proto_name in_tys ret_ty ]

let trans_item pack = function
  | Machine m -> trans_machine pack m
  | Proto p -> trans_proto p

let trans_pack p : C_ast.t =
  let lower_case_name = String.lowercase_ascii p.pack_name in
  let protos = List.concat_map trans_proto p.pack_protos in
  let machines =
    List.concat_map (trans_machine lower_case_name) p.pack_machines
  in
  protos @ machines

let find_nested_tys tys =
  let open Type in
  let rec aux acc ty =
    match ty with
    | TBool | TInt | TReal | TUnit -> Set.add ty acc
    | TOption t ->
        let acc' = aux acc t in
        Set.add ty acc'
    | TTuple ts ->
        let acc' = List.fold_left aux acc ts in
        Set.add ty acc'
    | TVar _ -> assert false
    | TFunc _ -> assert false
  in
  List.fold_left aux Set.empty tys

let tys_of_machine m =
  let in_tys = List.map snd m.inputs in
  let local_tys = List.map snd m.locals in
  let ret_ty = m.ret in
  find_nested_tys ((ret_ty :: in_tys) @ local_tys)

let tys_of_proto p =
  let in_tys = List.map snd p.proto_inputs in
  let ret_ty = p.proto_ret in
  find_nested_tys (ret_ty :: in_tys)

let tys_of_item = function
  | Machine m -> tys_of_machine m
  | Proto p -> tys_of_proto p

let tys_of_package p =
  let open Type in
  let acc =
    List.fold_left
      (fun acc i -> Set.union acc (tys_of_machine i))
      Set.empty p.pack_machines
  in
  List.fold_left (fun acc i -> Set.union acc (tys_of_proto i)) acc p.pack_protos

let get_all_types packs =
  let open Type in
  List.fold_left (fun acc p -> Set.union acc (tys_of_package p)) Set.empty packs

let rec get_opt_depth : Type.t -> Type.t * int = function
  | (TBool | TInt | TReal | TUnit | TTuple _) as t -> (t, 0)
  | TOption t ->
      let ty, n = get_opt_depth t in
      (ty, n + 1)
  | TVar _ | TFunc _ -> assert false

let get_all_opts tys =
  let open Type in
  let opts =
    Set.filter
      (function
        | TOption _ -> true
        | _ -> false)
      tys
  in
  Set.elements opts

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

let gen_opts tys =
  let tys = get_all_opts tys in
  let aux (proto_acc, struct_acc) ty =
    let base_ty =
      match ty with
      | Type.TOption t -> t
      | _ -> assert false
    in
    let name = gen_struct_name ty in
    let struct_decl = struct_decl name in
    let struct_ =
      struct_ name [ ("tag", tenum "opt_tag"); ("value", trans_type base_ty) ]
    in
    let some_proto, some_constr =
      let ret_ty = tstruct name in
      let val_ty = trans_type base_ty in
      let func_name = Format.asprintf "some_%s" name in
      ( proto func_name [ val_ty ] ret_ty,
        func func_name
          [ ("value", val_ty) ]
          ret_ty
          [
            stmt_var_decl [] ret_ty "ret";
            stmt_assign
              (lhs_dot (lhs_var "ret") (lhs_var "tag"))
              (expr_var "Some");
            stmt_assign
              (lhs_dot (lhs_var "ret") (lhs_var "value"))
              (expr_var "value");
            stmt_return (expr_var "ret");
          ] )
    in
    let none_proto, none_constr =
      let ret_ty = trans_type ty in
      let func_name = Format.asprintf "none_%s" name in
      ( proto func_name [] ret_ty,
        func func_name [] ret_ty
          [
            stmt_var_decl [] ret_ty "ret";
            stmt_assign
              (lhs_dot (lhs_var "ret") (lhs_var "tag"))
              (expr_var "None");
            stmt_return (expr_var "ret");
          ] )
    in
    ( struct_decl :: some_proto :: none_proto :: proto_acc,
      struct_ :: some_constr :: none_constr :: struct_acc )
  in
  let protos, structs = List.fold_left aux ([], []) tys in
  let tag_enum = enum "opt_tag" [ "None"; "Some" ] in
  (protos, (tag_enum :: protos) @ structs)

let get_all_tups tys =
  Type.Set.filter
    (function
      | TTuple _ -> true
      | _ -> false)
    tys
  |> Type.Set.elements

let gen_tups tys =
  let tys = get_all_tups tys in
  List.fold_left
    (fun (proto_acc, struct_acc) ty ->
      let struct_tys =
        match ty with
        | Type.TTuple ts -> ts
        | _ -> assert false
      in
      let fields =
        List.mapi
          (fun idx ty -> (Format.asprintf "val%d" idx, trans_type ty))
          struct_tys
      in
      let name = gen_struct_name ty in
      let decl = struct_decl name in
      let struct_ = struct_ name fields in
      (decl :: proto_acc, struct_ :: struct_acc))
    ([], []) tys

let f packs : C_ast.t =
  let packs' = List.concat_map trans_pack packs in
  let all_tys = get_all_types packs in
  let opts_decl, opts_def = gen_opts all_tys in
  let tup_decl, tup_def = gen_tups all_tys in
  let imports = [ preproc (pre_include "stdbool.h") ] in
  imports @ opts_decl @ tup_decl @ opts_def @ tup_def @ packs'

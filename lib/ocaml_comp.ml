open Ooir
open Ppxlib

include Ast_builder.Make (struct
  let loc = Location.none
end)

let noloc txt = { txt; loc = Location.none }

let rec dummy_val =
  let open Type in
  function
  | TInt -> eint 0
  | TReal -> efloat "0.0"
  | TBool -> ebool false
  | TUnit -> eunit
  | TOption _ -> pexp_construct (noloc (lident "None")) None
  | TFunc _ | TVar _ -> assert false
  | TTuple tys -> pexp_tuple (List.map dummy_val tys)

let trans_id = function
  | Lident.Lident s -> lident s
  | Ldot (p, s) -> Ldot (lident p, s)

let rec trans_type ty =
  let open Type in
  match ty with
  | TInt -> [%type: int]
  | TBool -> [%type: bool]
  | TReal -> [%type: float]
  | TUnit -> [%type: unit]
  | TTuple tys ->
      let tys' = List.map trans_type tys in
      ptyp_tuple tys'
  | TOption t ->
      let t' = trans_type t in
      ptyp_constr (noloc (lident "option")) [ t' ]
  | TFunc (t1, t2) ->
      let t1' = trans_type t1 in
      let t2' = trans_type t2 in
      ptyp_arrow Nolabel t1' t2'
  | TVar i -> ptyp_var ("a" ^ string_of_int i)

let rec trans_pattern p =
  let open Norm in
  let pat =
    match p.pat_desc with
    | PAny -> ppat_any
    | PUnit -> punit
    | PVar s -> pvar s
    | PTuple ps -> ppat_tuple (List.map trans_pattern ps)
  in
  let ty = trans_type p.pat_ty in
  ppat_constraint pat ty

let trans_const c =
  let open Ttree in
  match c with
  | CBool b -> ebool b
  | CInt i -> eint i
  | CReal f -> efloat (string_of_float f)
  | CUnit -> eunit

let trans_unop op s =
  let open Ttree in
  match op with
  | Not -> [%expr Bool.not [%e evar s]]
  | Neg -> [%expr Int.neg [%e evar s]]
  | RNeg -> [%expr Float.neg [%e evar s]]
  | IsSome -> [%expr Option.is_some [%e evar s]]

let trans_binop op s1 s2 =
  let open Ttree in
  let e1 = evar s1 in
  let e2 = evar s2 in
  match op with
  | And -> [%expr [%e e1] && [%e e2]]
  | Or -> [%expr [%e e1] || [%e e2]]
  | Implies -> [%expr Bool.not [%e e1] || [%e e2]]
  | Add -> [%expr [%e e1] + [%e e2]]
  | Sub -> [%expr [%e e1] - [%e e2]]
  | Mul -> [%expr [%e e1] * [%e e2]]
  | Div -> [%expr [%e e1] / [%e e2]]
  | RAdd -> [%expr [%e e1] +. [%e e2]]
  | RSub -> [%expr [%e e1] -. [%e e2]]
  | RMul -> [%expr [%e e1] *. [%e e2]]
  | RDiv -> [%expr [%e e1] /. [%e e2]]
  | Eq -> [%expr [%e e1] = [%e e2]]
  | Neq -> [%expr [%e e1] <> [%e e2]]
  | Lt | RLt -> [%expr [%e e1] < [%e e2]]
  | Leq | RLeq -> [%expr [%e e1] <= [%e e2]]
  | Gt | RGt -> [%expr [%e e1] > [%e e2]]
  | Geq | RGeq -> [%expr [%e e1] >= [%e e2]]

let trans_expr e =
  match e.expr_desc with
  | Var s -> eapply (evar "!") [ evar s ]
  | StateVar s -> evar s
  | Const c -> trans_const c
  | GlobalConst id -> pexp_ident (noloc (trans_id id))
  | None -> pexp_construct (noloc (lident "None")) None
  | Some s -> pexp_construct (noloc (lident "Some")) (Some (evar s))
  | UnOp (op, s) -> trans_unop op s
  | BinOp (op, s1, s2) -> trans_binop op s1 s2

let rec eseq = function
  | [ e ] -> e
  | e :: es -> pexp_sequence e (eseq es)
  | [] -> eunit

let rec trans_instr = function
  | Assign (name, e) -> eapply (evar ":=") [ evar name; trans_expr e ]
  | StateAssign (name, e) -> pexp_setinstvar (noloc name) (trans_expr e)
  | TupleConstr (lhs, rhs) ->
      pexp_apply (evar ":=")
        [ (Nolabel, evar lhs); (Nolabel, pexp_tuple (List.map evar rhs)) ]
  | TupleDestr (lhs, rhs) ->
      let binding =
        let pat = ppat_tuple (List.map (fun l -> pvar (l ^ "'")) lhs) in
        let expr = evar rhs in
        value_binding ~pat ~expr
      in
      let assign_expr =
        List.map
          (fun l ->
            pexp_apply (evar ":=")
              [ (Nolabel, evar l); (Nolabel, evar (l ^ "'")) ])
          lhs
        |> eseq
      in
      pexp_let Nonrecursive [ binding ] assign_expr
  | Reset (id, _) ->
      pexp_send (pexp_ident (noloc (trans_id id))) (noloc "reset")
  | Return s -> pexp_apply (evar "!") [ (Nolabel, evar s) ]
  | If (c, t, e) ->
      pexp_ifthenelse
        (eapply (evar "!") [ evar c ])
        (List.map trans_instr t |> eseq)
        (Some (List.map trans_instr e |> eseq))
  | StepApp (lhs, _, args, self) -> (
      let run = pexp_send (pexp_ident (noloc (lident self))) (noloc "run") in
      let app =
        pexp_apply run
          (List.map (fun x -> (Nolabel, eapply (evar "!") [ evar x ])) args)
      in
      match lhs with
      | None -> app
      | Some s -> pexp_apply (evar ":=") [ (Nolabel, evar s); (Nolabel, app) ])
  | Either (lhs, e, o) ->
      pexp_apply (evar ":=")
        [
          (Nolabel, evar lhs);
          ( Nolabel,
            [%expr
              Option.value [%e evar e]
                ~default:[%e List.map trans_instr o |> eseq]] );
        ]

let gen_proto_sig proto =
  let name = noloc proto.proto_name in
  let inp_typ = trans_type proto.proto_input in
  let out_typ = trans_type proto.proto_output in
  let type_ = ptyp_arrow Nolabel inp_typ out_typ in
  let prim = [] in
  let val_desc = value_description ~name ~type_ ~prim in
  psig_value val_desc

let gen_proto_ty protos =
  match List.map gen_proto_sig protos with
  | [] -> []
  | vals ->
      let mod_typ =
        module_type_declaration ~name:(noloc "Extern")
          ~type_:(Some (pmty_signature vals))
      in
      [ pstr_modtype mod_typ ]

let class_ty name in_ty out_ty =
  let class_sig =
    class_signature ~self:ptyp_any
      ~fields:
        [
          pctf_method
            ( noloc "run",
              Public,
              Concrete,
              ptyp_arrow Nolabel (trans_type in_ty) (trans_type out_ty) );
          pctf_method
            ( noloc "reset",
              Public,
              Concrete,
              ptyp_arrow Nolabel [%type: unit] [%type: unit] );
        ]
  in
  let obj = pcty_signature class_sig in
  let class_desc =
    class_infos ~virt:Concrete ~params:[] ~name:(noloc name) ~expr:obj
  in
  psig_class [ class_desc ]

let gen_intf_ty p =
  let proto_infs =
    List.map
      (fun p -> class_ty p.proto_name p.proto_input p.proto_output)
      p.pack_protos
  in
  let step_intfs =
    List.map (fun s -> class_ty s.name s.input.pat_ty s.ret) p.pack_machines
  in
  let mod_typ =
    module_type_declaration ~name:(noloc "Intf")
      ~type_:(Some (pmty_signature (proto_infs @ step_intfs)))
  in
  pstr_modtype mod_typ

let gen_proto_obj p =
  let name = p.proto_name in
  let class_struct =
    class_structure ~self:ppat_any
      ~fields:
        [
          pcf_method
            ( noloc "run",
              Public,
              Cfk_concrete (Fresh, pexp_ident (noloc (Ldot (lident "E", name))))
            );
          pcf_method
            (noloc "reset", Public, Cfk_concrete (Fresh, [%expr fun () -> ()]));
        ]
  in
  let obj = pcl_structure class_struct in
  let class_desc =
    class_infos ~virt:Concrete ~params:[] ~name:(noloc name) ~expr:obj
  in
  pstr_class [ class_desc ]

let gen_instance_field (name, id) =
  pcf_val
    (noloc name, Immutable, Cfk_concrete (Fresh, pexp_new (noloc (trans_id id))))

let gen_run_method m =
  let local_refs =
    List.map
      (fun (name, ty) ->
        let pat = pvar name in
        let expr = pexp_apply (evar "ref") [ (Nolabel, dummy_val ty) ] in
        value_binding ~pat ~expr)
      m.locals
  in
  let body = List.map trans_instr m.def in
  let run_method =
    pexp_fun Nolabel None (trans_pattern m.input)
      (pexp_let Nonrecursive local_refs (body |> eseq))
  in
  pcf_method (noloc "run", Public, Cfk_concrete (Fresh, run_method))

let gen_machine_obj m =
  let name = m.name in
  let memory_vals =
    List.map
      (fun (name, ty) ->
        pcf_val (noloc name, Mutable, Cfk_concrete (Fresh, dummy_val ty)))
      m.memory
  in
  let instance_vals = List.map gen_instance_field m.instances in
  let run_method = gen_run_method m in
  let class_struct =
    class_structure ~self:ppat_any
      ~fields:(memory_vals @ instance_vals @ [ run_method ])
  in
  let obj = pcl_structure class_struct in
  let class_desc =
    class_infos ~virt:Concrete ~params:[] ~name:(noloc name) ~expr:obj
  in
  pstr_class [ class_desc ]

let gen_make p =
  let inner_mod_expr =
    let proto_objs = List.map gen_proto_obj p.pack_protos in
    let machine_objs = List.map gen_machine_obj p.pack_machines in
    pmod_structure (proto_objs @ machine_objs)
  in
  let mod_expr =
    let funct_param =
      if p.pack_protos = [] then Unit
      else Named (noloc (Option.some "E"), pmty_ident (noloc (lident "Extern")))
    in
    pmod_functor funct_param inner_mod_expr
  in
  let mod_expr_with_deps =
    List.fold_left
      (fun expr dep ->
        let funct_param =
          Named
            ( noloc (Option.some dep),
              pmty_ident (noloc (Ldot (lident dep, "Intf"))) )
        in
        pmod_functor funct_param expr)
      mod_expr p.pack_dependencies
  in
  let mod_bind =
    module_binding ~name:(noloc (Option.some "Make")) ~expr:mod_expr_with_deps
  in
  pstr_module mod_bind

let trans_package p =
  let name = p.pack_name in
  let proto_intf = gen_proto_ty p.pack_protos in
  let pack_intf = gen_intf_ty p in
  let make_functor = gen_make p in
  let mod_expr = pmod_structure (proto_intf @ [ pack_intf; make_functor ]) in
  let mod_bind =
    module_binding ~name:(noloc (Option.some name)) ~expr:mod_expr
  in
  [ pstr_module mod_bind ]

open Ttree
open Ppxlib

include Ast_builder.Make (struct
  let loc = Location.none
end)

let noloc txt = { txt; loc = Location.none }

let rec trans_type ty =
  let open Type in
  match ty with
  | TInt -> [%type: int]
  | TBool -> [%type: bool]
  | TFloat -> [%type: float]
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
  match p.pat_desc with
  | PAny -> [%expr PAny]
  | PUnit -> [%expr PUnit]
  | PVar v -> [%expr PVar [%e estring v]]
  | PTuple ps -> [%expr PTuple [%e elist (List.map trans_pattern ps)]]

let trans_const = function
  | CUnit -> [%expr EUnit]
  | CInt i -> [%expr EInt [%e eint i]]
  | CBool b -> [%expr EBool [%e ebool b]]
  | CFloat f -> [%expr EFloat [%e efloat f]]

let trans_unop = function
  | Not -> [%expr Not]
  | Neg | FNeg -> [%expr Neg]
  | IsSome -> [%expr IsSome]

let trans_binop = function
  | And -> [%expr And]
  | Or -> [%expr Or]
  | Implies -> [%expr Implies]
  | Add | FAdd -> [%expr Add]
  | Sub | FSub -> [%expr Sub]
  | Mul | FMul -> [%expr Mul]
  | Div | FDiv -> [%expr Div]
  | Eq -> [%expr Eq]
  | Neq -> [%expr Neq]
  | Lt | FLt -> [%expr Lt]
  | Leq | FLeq -> [%expr Leq]
  | Gt | FGt -> [%expr Gt]
  | Geq | FGeq -> [%expr Geq]

let rec trans_expr e =
  match e.expr_desc with
  | EVar s -> (
      match e.expr_ty with
      | Type.TFunc _ -> evar s
      | _ -> [%expr EVar [%e estring s]])
  | EConst c -> trans_const c
  | EUnOp (op, e) -> [%expr EUnOp ([%e trans_unop op], [%e trans_expr e])]
  | EBinOp (op, e1, e2) ->
      [%expr
        EBinOp ([%e trans_binop op], [%e trans_expr e1], [%e trans_expr e2])]
  | EEither (e1, e2) -> [%expr EEither ([%e trans_expr e1], [%e trans_expr e2])]
  | ETuple es -> [%expr ETuple [%e elist (List.map trans_expr es)]]
  | EIf (c, t, e) ->
      [%expr EIf ([%e trans_expr c], [%e trans_expr t], [%e trans_expr e])]
  | EApp (f, a) -> [%expr EApp ([%e trans_expr f], [%e trans_expr a])]
  | EArrow (e1, e2) -> [%expr EArrow ([%e trans_expr e1], [%e trans_expr e2])]
  | EFby (e1, e2) -> [%expr EFby ([%e trans_expr e1], [%e trans_expr e2])]
  | EPre e -> [%expr EPre [%e trans_expr e]]
  | ENone -> [%expr ENone]
  | ESome e -> [%expr ESome [%e trans_expr e]]

let trans_eq (p, e) = [%expr [%e trans_pattern p], [%e trans_expr e]]
let trans_eqs eqs = elist (List.map trans_eq eqs)

let trans_step s =
  pstr_value Nonrecursive
    [
      value_binding ~pat:(pvar s.step_name)
        ~expr:
          [%expr
            ELam
              ( [%e trans_pattern s.step_input],
                [%e trans_pattern s.step_output],
                [%e trans_eqs s.step_def] )];
    ]

let proto_sig proto =
  let name = noloc proto.proto_name in
  let inp_typ = trans_type proto.proto_input.pat_ty in
  let out_typ = trans_type proto.proto_output.pat_ty in
  let type_ = ptyp_arrow Nolabel inp_typ out_typ in
  let prim = [] in
  let val_desc = value_description ~name ~type_ ~prim in
  psig_value val_desc

let extern_module_sig protos =
  match List.map proto_sig protos with
  | [] -> None
  | vals ->
      let mod_typ =
        module_type_declaration ~name:(noloc "Extern")
          ~type_:(Some (pmty_signature vals))
      in
      Some (pstr_modtype mod_typ)

let create_arg ty =
  let open Type in
  let rec aux n = function
    | TUnit -> (n, ([%pat? VUnit], eunit))
    | TInt ->
        let var = Format.asprintf "arg_%d" n in
        (n + 1, ([%pat? VInt [%p pvar var]], evar var))
    | TBool ->
        let var = Format.asprintf "arg_%d" n in
        (n + 1, ([%pat? VBool [%p pvar var]], evar var))
    | TFloat ->
        let var = Format.asprintf "arg_%d" n in
        (n + 1, ([%pat? VFloat [%p pvar var]], evar var))
    | TTuple tys ->
        let n', cases = List.fold_left_map aux n tys in
        let llhs, lrhs = List.split cases in
        (n', ([%pat? VTuple [%p plist llhs]], pexp_tuple lrhs))
    | TOption ty ->
        let var = Format.asprintf "arg_%d" n in
        let n', (p, e) = aux (n + 1) ty in
        let p' = [%pat? VOption [%p pvar var]] in
        let e' =
          [%expr
            Option.map
              (function
                | [%p p] -> [%e e]
                | _ -> assert false)
              [%e evar var]]
        in
        (n', (p', e'))
    | _ -> assert false
  in
  let _, (p, e) = aux 0 ty in
  [%expr
    match input with
    | [%p p] -> [%e e]
    | _ -> assert false]

let create_ret ty =
  let open Type in
  let rec aux n = function
    | TUnit -> (n, (punit, [%expr VUnit]))
    | TInt ->
        let var = Format.asprintf "ret_%d" n in
        (n + 1, (pvar var, [%expr VInt [%e evar var]]))
    | TBool ->
        let var = Format.asprintf "ret_%d" n in
        (n + 1, (pvar var, [%expr VBool [%e evar var]]))
    | TFloat ->
        let var = Format.asprintf "ret_%d" n in
        (n + 1, (pvar var, [%expr VFloat [%e evar var]]))
    | TTuple tys ->
        let n', cases = List.fold_left_map aux n tys in
        let ps, es = List.split cases in
        (n', (ppat_tuple ps, [%expr VTuple [%e elist es]]))
    | TOption ty ->
        let var = Format.asprintf "ret_%d" n in
        let n', (p, e) = aux (n + 1) ty in
        ( n',
          ( pvar var,
            [%expr
              Option.fold ~none:(VOption None)
                ~some:(fun [%p p] -> VOption (Some [%e e]))
                [%e evar var]] ) )
    | _ -> assert false
  in
  let p, e = aux 0 ty |> snd in
  [%expr
    match ret with
    | [%p p] -> [%e e]]

let trans_proto p =
  let pat = pvar p.proto_name in
  let expr =
    [%expr
      EExtern
        [%e
          pexp_fun Nolabel None (pvar "input")
            [%expr
              let arg = [%e create_arg p.proto_input.pat_ty] in
              let ret =
                [%e pexp_ident (noloc (Ldot (Lident "E", p.proto_name)))] arg
              in
              [%e create_ret p.proto_output.pat_ty]]]]
  in
  let vb = value_binding ~pat ~expr in
  pstr_value Nonrecursive [ vb ]

let val_of_const_expr e =
  match e.expr_desc with
  | EConst CUnit -> [%expr VUnit]
  | EConst (CInt i) -> [%expr VInt [%e eint i]]
  | EConst (CBool b) -> [%expr VBool [%e ebool b]]
  | EConst (CFloat f) -> [%expr VFloat [%e efloat f]]
  | _ -> assert false

let trans_node n =
  let trans_port p = pexp_tuple [ estring p.port_name; ebool p.port_opt ] in
  [%stri
    let [%p pvar n.node_name] =
      {
        node_name = [%e estring n.node_name];
        node_period = [%e eint64 (fst n.node_period |> Int64.of_int)];
        node_expr = [%e evar n.node_implements];
        node_inputs = [%e elist (List.map trans_port n.node_inputs)];
        node_outputs = [%e elist (List.map trans_port n.node_outputs)];
      }]

let open_sim = [%stri open Mimosa.Sim_ast]

let gen_init p =
  let node_names = List.map (fun n -> evar n.node_name) p.nodes in
  let channels =
    List.map
      (fun chan ->
        let name = chan.channel_name in
        let elems = elist (List.map val_of_const_expr chan.channel_elems) in
        pexp_tuple [ estring name; elems ])
      p.channels
  in
  [
    [%stri
      let init =
        Mimosa.Sim.create_init_state [%e elist channels] [%e elist node_names]];
  ]

let trans_pack p =
  let steps = List.map trans_step p.steps in
  let protos = List.map trans_proto p.protos in
  let extern_sig = extern_module_sig p.protos in
  let nodes = List.map trans_node p.nodes in
  let init = gen_init p in
  let str_items = pmod_structure (protos @ steps @ nodes @ init) in
  match extern_sig with
  | Some extern_sig ->
      let mod_expr =
        let funct_param =
          Named (noloc (Option.some "E"), pmty_ident (noloc (lident "Extern")))
        in
        pmod_functor funct_param str_items
      in
      let mod_bind =
        module_binding ~name:(noloc (Option.some "Simulation")) ~expr:mod_expr
      in
      [ open_sim; extern_sig; pstr_module mod_bind ]
  | None ->
      [
        open_sim;
        pstr_module
          (module_binding ~name:(noloc (Some "Simulation")) ~expr:str_items);
      ]

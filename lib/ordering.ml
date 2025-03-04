open Ptree

type t = {
  protos : proto list;
  steps : step list;
  channels : channel list;
  nodes : node list;
}

let steps_used_in_expr acc =
  let rec aux acc expr =
    match expr.expr_desc with
    | Expr_ident id -> String.Set.add id acc
    | Expr_constant _ -> acc
    | Expr_unop (_, e) | Expr_pre e -> aux acc e
    | Expr_binop (_, e1, e2)
    | Expr_arrow (e1, e2)
    | Expr_fby (e1, e2)
    | Expr_either (e1, e2)
    | Expr_apply (e1, e2) ->
        let acc' = aux acc e1 in
        aux acc' e2
    | Expr_tuple es -> List.fold_left aux acc es
    | Expr_ite (c, t, e) -> List.fold_left aux acc [ c; t; e ]
    | Expr_none -> acc
    | Expr_some e -> aux acc e
  in
  aux acc

let steps_used_by_step step =
  List.fold_left
    (fun acc (_, rhs) -> steps_used_in_expr acc rhs)
    String.Set.empty step.step_def

let check_protos step_locs protos =
  let open Reserr in
  let aux acc p =
    let name = p.proto_name.txt in
    match String.Map.find_opt name acc with
    | Some loc ->
        let err = Error.(Symbol_redefinition (`Step, name, loc)) in
        error (err, p.proto_loc)
    | None -> String.Map.add name p.proto_name.loc acc |> ok
  in
  fold_left aux step_locs protos

let check_channels channels =
  let open Reserr in
  let aux acc c =
    let name = c.channel_name.txt in
    match String.Map.find_opt name acc with
    | Some loc ->
        let err = Error.(Symbol_redefinition (`Channel, name, loc)) in
        error (err, c.channel_name.loc)
    | None -> String.Map.add name c.channel_name.loc acc |> ok
  in
  let* _ = fold_left aux String.Map.empty channels in
  ok ()

let check_nodes step_locs nodes =
  let open Reserr in
  let aux acc n =
    let* _ =
      let impl_step = n.node_implements.txt in
      match String.Map.find_opt impl_step step_locs with
      | Some _ -> ok ()
      | None ->
          let loc = n.node_implements.loc in
          let err = Error.(Unknown_symbol (`Step, impl_step)) in
          error (err, loc)
    in
    let name = n.node_name.txt in
    match String.Map.find_opt name acc with
    | Some loc ->
        let err = Error.(Symbol_redefinition (`Node, name, loc)) in
        error (err, n.node_name.loc)
    | None -> String.Map.add name n.node_name.loc acc |> ok
  in
  let* _ = fold_left aux String.Map.empty nodes in
  ok ()

let order items =
  let open Reserr in
  let steps, protos, channels, nodes =
    List.fold_right
      (fun item (acc_step, acc_proto, acc_channel, acc_node) ->
        match item.item_desc with
        | Step s -> (s :: acc_step, acc_proto, acc_channel, acc_node)
        | Proto p -> (acc_step, p :: acc_proto, acc_channel, acc_node)
        | Channel c -> (acc_step, acc_proto, c :: acc_channel, acc_node)
        | Node n -> (acc_step, acc_proto, acc_channel, n :: acc_node))
      items ([], [], [], [])
  in
  let aux (step_map, deps) step =
    let steps_used = steps_used_by_step step in
    let name = step.step_name.txt in
    let dep = (name, String.Set.elements steps_used) in
    match String.Map.find_opt name step_map with
    | None ->
        (String.Map.add name step.step_name.loc step_map, dep :: deps) |> ok
    | Some loc ->
        let err = Error.(Symbol_redefinition (`Step, name, loc)) in
        error (err, step.step_name.loc)
  in
  let* step_locs, deps = fold_left aux (String.Map.empty, []) steps in
  let step_map =
    List.mapi
      (fun i step ->
        let name = step.step_name.txt in
        (name, i))
      steps
  in
  let dag =
    List.map
      (fun (name, deps) ->
        ( List.assoc name step_map,
          List.filter_map (Fun.flip List.assoc_opt step_map) deps ))
      deps
  in
  let* sorted =
    match Tsort.sort dag with
    | Tsort.Sorted list -> ok list
    | ErrorCycle list ->
        let steps = List.map (List.nth steps) list in
        let names = List.map (fun step -> step.step_name.txt) steps in
        let err = Error.(Dependency_cycle (`Steps, names)) in
        error (err, Ptree_builder.noloc)
  in
  let sorted_steps = List.map (fun i -> List.nth steps i) sorted in
  let* ordered_steps = map Eq_ordering.order_step sorted_steps in
  let* step_locs = check_protos step_locs protos in
  let* _ = check_nodes step_locs nodes in
  let* _ = check_channels channels in
  { steps = ordered_steps; protos; nodes; channels } |> ok

let f = order

let pp ppf { protos; steps; channels; nodes } =
  let open Sexplib.Sexp in
  let open Ptree_printer in
  let p = List.map sexp_of_proto protos in
  let s = List.map sexp_of_step steps in
  let c = List.map sexp_of_channel channels in
  let n = List.map sexp_of_node nodes in
  let all = List (p @ s @ c @ n) in
  Sexplib.Sexp.pp_hum_indent 2 ppf all

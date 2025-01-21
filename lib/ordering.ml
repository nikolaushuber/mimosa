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
    | Expr_ite (i, t, e) -> List.fold_left aux acc [ i; t; e ]
    | Expr_none -> acc
    | Expr_some e -> aux acc e
  in
  aux acc

let steps_used_by_step step =
  List.fold_left
    (fun acc (_, rhs) -> steps_used_in_expr acc rhs)
    String.Set.empty step.step_def

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
        let err = Error.Step_redefine (name, loc) in
        error (err, step.step_name.loc)
  in
  let* _, deps = fold_left aux (String.Map.empty, []) steps in
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
        let err = Error.Cycle_in_steps names in
        error (err, Ptree_builder.noloc)
  in
  let sorted_steps = List.map (fun i -> List.nth steps i) sorted in
  let* ordered_steps = map Eq_ordering.order_step sorted_steps in
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

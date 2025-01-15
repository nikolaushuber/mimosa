(* Orders items within a package *)

open Ptree

let steps_used_in_expr acc =
  let rec aux acc expr =
    match expr.pexpr_desc with
    | Pexpr_ident id -> (
        match id.txt with
        | Lident id -> String.Set.add id acc
        | Ldot _ -> acc)
    | Pexpr_constant _ -> acc
    | Pexpr_unop (_, e) | Pexpr_pre e -> aux acc e
    | Pexpr_binop (_, e1, e2)
    | Pexpr_arrow (e1, e2)
    | Pexpr_fby (e1, e2)
    | Pexpr_either (e1, e2)
    | Pexpr_apply (e1, e2) ->
        let acc' = aux acc e1 in
        aux acc' e2
    | Pexpr_tuple es -> List.fold_left aux acc es
    | Pexpr_ite (i, t, e) -> List.fold_left aux acc [ i; t; e ]
    | Pexpr_none -> acc
    | Pexpr_some e -> aux acc e
  in
  aux acc

let steps_used_by_step step =
  List.fold_left
    (fun acc (_, rhs) -> steps_used_in_expr acc rhs)
    String.Set.empty step.pstep_def

let order_pack p =
  let open Reserr in
  let items = p.ppack_items in
  let step_items, proto_items, link_items, node_items =
    List.fold_right
      (fun item (acc_step, acc_proto, acc_link, acc_node) ->
        match item.ppack_item with
        | Ppack_step _ -> (item :: acc_step, acc_proto, acc_link, acc_node)
        | Ppack_proto _ -> (acc_step, item :: acc_proto, acc_link, acc_node)
        | Ppack_link _ -> (acc_step, acc_proto, item :: acc_link, acc_node)
        | Ppack_node _ -> (acc_step, acc_proto, acc_link, item :: acc_node))
      items ([], [], [], [])
  in
  let steps =
    List.map
      (fun item ->
        match item.ppack_item with
        | Ppack_step s -> s
        | _ -> assert false)
      step_items
  in
  let aux (step_map, deps) step =
    let steps_used = steps_used_by_step step in
    let name = step.pstep_name.txt in
    let dep = (name, String.Set.elements steps_used) in
    match String.Map.find_opt name step_map with
    | None ->
        (String.Map.add name step.pstep_name.loc step_map, dep :: deps) |> ok
    | Some loc ->
        let err = Error.Step_redefine (name, loc) in
        error (err, step.pstep_name.loc)
  in
  let* _, deps = fold_left aux (String.Map.empty, []) steps in
  let step_map =
    List.mapi
      (fun i step ->
        let name = step.pstep_name.txt in
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
        let names = List.map (fun step -> step.pstep_name.txt) steps in
        let err = Error.Cycle_in_steps names in
        let loc = p.ppack_name.loc in
        error (err, loc)
  in
  let ordered_steps = List.map (fun i -> List.nth step_items i) sorted in
  let ppack_items = proto_items @ ordered_steps @ link_items @ node_items in
  { p with ppack_items } |> ok

let f d = Reserr.map order_pack d

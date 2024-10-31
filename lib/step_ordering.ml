(* Orders steps within a package *)

open Ptree
module Map = Map.Make (String)
module Set = Set.Make (String)

let steps_used_in_expr acc =
  let rec aux acc expr =
    match expr.pexpr_desc with
    | Pexpr_ident _ -> acc
    | Pexpr_constant _ -> acc
    | Pexpr_unop (_, e) -> aux acc e
    | Pexpr_binop (_, e1, e2) ->
        let acc' = aux acc e1 in
        aux acc' e2
    | Pexpr_either (e1, e2) ->
        let acc' = aux acc e1 in
        aux acc' e2
    | Pexpr_apply (f, e) ->
        let acc' =
          match f.txt with
          | Lident id -> Set.add id acc
          | Ldot _ -> acc
        in
        aux acc' e
    | Pexpr_tuple es -> List.fold_left aux acc es
    | Pexpr_ite (i, t, e) -> List.fold_left aux acc [ i; t; e ]
    | Pexpr_match (e, cases) ->
        let acc' = aux acc e in
        List.fold_left aux_cases acc' cases
    | Pexpr_none -> acc
    | Pexpr_some e -> aux acc e
  and aux_cases acc case = aux acc case.pcase_rhs in
  aux acc

let steps_used_by_step step =
  List.fold_left
    (fun acc (_, rhs) -> steps_used_in_expr acc rhs)
    Set.empty step.pstep_def

let f p =
  let open Reserr in
  let items = p.ppack_items in
  let step_items, others =
    List.partition
      (fun item ->
        match item.ppack_item with
        | Ppack_step _ -> true
        | _ -> false)
      items
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
    let dep = (name, Set.elements steps_used) in
    match Map.find_opt name step_map with
    | None -> (Map.add name step.pstep_loc step_map, dep :: deps) |> ok
    | Some loc ->
        let err = Error.Step_redefine (name, loc) in
        error (err, step.pstep_name.loc)
  in
  let* _, deps = fold_left aux (Map.empty, []) steps in
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
  let ppack_items = others @ ordered_steps in
  { p with ppack_items } |> ok

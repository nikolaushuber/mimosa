(** Equation ordering: This pass orders equations inside each step. Cyclic
    dependencies are reported as errors if they don't go through at least one
    shift operator. *)

open Ptree

(* Returns the set of names defined by a pattern. If a symbol is defined
   multiple times the result is an error. [init_map] is a map from symbols to
   locations, which can be used to raise an error for previously defined
   symbols *)
let vars_of_pat ?(init_map = String.Map.empty) =
  let open Reserr in
  let rec aux ((set, loc_map) as acc) pat =
    match pat.pat_desc with
    | Pat_any -> ok acc
    | Pat_unit -> ok acc
    | Pat_var name ->
        if String.Map.mem name.txt loc_map then
          let first_loc = String.Map.find name.txt loc_map in
          let err = Error.Local_symbol_redef (name.txt, first_loc) in
          error (err, name.loc)
        else
          (String.Set.add name.txt set, String.Map.add name.txt name.loc loc_map)
          |> ok
    | Pat_tuple pats -> Reserr.fold_left aux acc pats
  in
  aux (String.Set.empty, init_map)

let rec check_any_in_output pat =
  let open Reserr in
  match pat.pat_desc with
  | Pat_unit | Pat_var _ -> ok ()
  | Pat_tuple ps -> fold_left (fun () -> check_any_in_output) () ps
  | Pat_any ->
      let loc = pat.pat_loc in
      let err = Error.Output_any in
      error (err, loc)

(* Returns the set of symbols used by an expression *)
let vars_used_by_expr =
  let open Reserr in
  let rec aux set expr =
    match expr.expr_desc with
    | Expr_ident id -> String.Set.add id set |> ok
    | Expr_constant _ -> ok set
    | Expr_unop (_, e) | Expr_pre e -> aux set e
    | Expr_binop (_, e1, e2)
    | Expr_fby (e1, e2)
    | Expr_arrow (e1, e2)
    | Expr_either (e1, e2) ->
        fold_left aux set [ e1; e2 ]
    | Expr_apply (_, e) -> aux set e
    | Expr_tuple es -> fold_left aux set es
    | Expr_ite (e1, e2, e3) -> fold_left aux set [ e1; e2; e3 ]
    | Expr_none -> ok set
    | Expr_some e -> aux set e
  in
  aux String.Set.empty

let order_step step =
  let open Reserr in
  let* in_set, init_map = vars_of_pat step.step_input in
  let* out_set, out_map = vars_of_pat ~init_map step.step_output in

  (* For each equation, find out which symbols are defined and used *)
  let* defs, uses =
    let aux (init_map, list) (pat, e) =
      let* defs, map' = vars_of_pat ~init_map pat in
      let* uses = vars_used_by_expr e in
      (map', (defs, uses) :: list) |> ok
    in
    let* _, defs_uses = fold_left aux (init_map, []) step.step_def in
    List.rev defs_uses |> List.split |> ok
  in

  (* all defined symbols by the given equations *)
  let all_defs = List.fold_left String.Set.union String.Set.empty defs in

  (* does the output use any patterns? *)
  let* _ = check_any_in_output step.step_output in

  (* Are all outputs defined? *)
  let missing_outs = String.Set.diff out_set all_defs in
  if not (String.Set.is_empty missing_outs) then
    let elem = String.Set.choose missing_outs in
    let loc = String.Map.find elem out_map in
    let err = Error.Output_not_defined elem in
    error (err, loc)
  else
    (* all symbols used by the given equations *)
    let all_uses = List.fold_left String.Set.union String.Set.empty uses in

    (* Are all inputs used? *)
    let unused_inputs = String.Set.diff in_set all_uses in
    if not (String.Set.is_empty unused_inputs) then
      let elem = String.Set.choose unused_inputs in
      let loc = String.Map.find elem init_map in
      let err = Error.Input_unused elem in
      error (err, loc)
    else
      (* create map from each defined symbol to equation where it was defined *)
      let def_map, rev_map =
        let map_eq_number i defs =
          String.Set.fold
            (fun id (map, rev_map) -> ((id, i) :: map, (i, id) :: rev_map))
            defs ([], [])
        in
        let def_list, rev_list = List.mapi map_eq_number defs |> List.split in
        (List.flatten def_list, List.flatten rev_list)
      in

      let dependencies =
        (* Undefined names will be detected during type-checking *)
        let find_name name acc =
          try List.assoc name def_map :: acc with _ -> acc
        in

        List.mapi (fun i set -> (i, String.Set.fold find_name set [])) uses
      in

      let* sorted =
        match Tsort.sort dependencies with
        | Tsort.Sorted list -> ok list
        | ErrorCycle list ->
            let names = List.map (Fun.flip List.assoc rev_map) list in
            let err = Error.Cycle_in_equations names in
            error (err, step.step_loc)
      in

      { step with step_def = List.map (List.nth step.step_def) sorted } |> ok

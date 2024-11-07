module Tree : sig
  type 'a t

  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val map : ('a -> 'b Reserr.t) -> 'a t -> 'b t Reserr.t
  val fold_left : ('a -> 'b -> 'a Reserr.t) -> 'a -> 'b t -> 'a Reserr.t

  val fold_left_map :
    ('acc -> 'a -> ('acc * 'b) Reserr.t) ->
    'acc ->
    'a t ->
    ('acc * 'b t) Reserr.t

  val pp : 'a Fmt.t -> 'a t Fmt.t
end = struct
  type 'a t = 'a list

  let of_list = Fun.id
  let to_list = Fun.id
  let map f l = Reserr.map f l
  let fold_left f acc l = Reserr.fold_left f acc l

  let fold_left_map f acc l =
    let open Reserr in
    let* acc, b =
      fold_left
        (fun (acc, items) item ->
          let* acc, b = f acc item in
          (acc, b :: items) |> ok)
        (acc, []) l
    in
    (acc, List.rev b) |> ok

  let pp pp_item = Fmt.list pp_item
end

(* Resolves depedency between packages *)

open Ptree

(* For now we don't have enums *)
let pack_in_pat acc _ = acc

let pack_in_expr acc =
  let rec aux acc expr =
    match expr.pexpr_desc with
    | Pexpr_ident id -> (
        match id.txt with
        | Lident _ -> acc
        | Ldot (p, _) -> Set.String.add p acc)
    | Pexpr_constant _ -> acc
    | Pexpr_unop (_, e) | Pexpr_pre e -> aux acc e
    | Pexpr_binop (_, e1, e2)
    | Pexpr_either (e1, e2)
    | Pexpr_fby (e1, e2)
    | Pexpr_arrow (e1, e2) ->
        List.fold_left aux acc [ e1; e2 ]
    | Pexpr_apply (f, e) ->
        let acc' =
          match f.txt with
          | Lident _ -> acc
          | Ldot (p, _) -> Set.String.add p acc
        in
        aux acc' e
    | Pexpr_tuple es -> List.fold_left aux acc es
    | Pexpr_ite (e1, e2, e3) -> List.fold_left aux acc [ e1; e2; e3 ]
    | Pexpr_match (e, cases) ->
        let acc' = aux acc e in
        List.fold_left aux_cases acc' cases
    | Pexpr_none -> acc
    | Pexpr_some e -> aux acc e
  and aux_cases acc case =
    let acc' = pack_in_pat acc case.pcase_lhs in
    aux acc' case.pcase_rhs
  in
  aux acc

let pack_in_step acc step =
  List.fold_left
    (fun acc (lhs, rhs) ->
      let acc' = pack_in_pat acc lhs in
      pack_in_expr acc' rhs)
    acc step.pstep_def

let pack_in_item acc item =
  match item.ppack_item with
  | Ppack_step s -> pack_in_step acc s
  | Ppack_node _ -> acc
  | Ppack_link _ -> acc

let pack_dependency p =
  List.fold_left pack_in_item Set.String.empty p.ppack_items
  |> Set.String.elements

let f ps : Ptree.t Tree.t Reserr.t =
  let open Reserr in
  let packages = List.map (fun p -> p.ppack_name.txt) ps in
  let rec check_unique xs =
    match xs with
    | x :: xs ->
        if List.mem x xs then error (Package_redefinition x, Location.none)
        else check_unique xs
    | [] -> ok ()
  in
  let* _ = check_unique packages in
  let pack_map, _ =
    List.mapi
      (fun i p ->
        let pack = p.ppack_name.txt in
        ((pack, i), (i, pack)))
      ps
    |> List.split
  in
  let dag =
    List.mapi
      (fun i p ->
        let dep = pack_dependency p in
        (i, List.filter_map (Fun.flip List.assoc_opt pack_map) dep))
      ps
  in
  let* sorted =
    match Tsort.sort dag with
    | Tsort.Sorted list -> ok list
    | ErrorCycle list ->
        let names = List.map (List.nth packages) list in
        let err = Error.Cycle_in_packages names in
        error (err, Location.none)
  in
  List.map (List.nth ps) sorted |> Tree.of_list |> ok

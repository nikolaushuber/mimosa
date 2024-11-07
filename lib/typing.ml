module State = struct
  type t = { next : int }

  let init ?(n = 0) () = { next = n }

  let next state =
    let var = state.next in
    let state = { next = state.next + 1 } in
    (var, state)
end

module rec Type : sig
  type t =
    | TUnit
    | TBool
    | TReal
    | TInt
    | TTuple of t list
    | TOption of t
    | TFunc of t * t
    | TVar of int

  val ftv : t -> Set.Int.t
  val apply : Subst.t -> t -> t
  val generalize : Env.t -> t -> Scheme.t
  val unify : ?loc:Location.t -> t -> t -> Subst.t Reserr.t
  val pp : Format.formatter -> t -> unit
end = struct
  type t =
    | TUnit
    | TBool
    | TReal
    | TInt
    | TTuple of t list
    | TOption of t
    | TFunc of t * t
    | TVar of int

  let rec ftv = function
    | TUnit | TBool | TReal | TInt -> Set.Int.empty
    | TTuple tup ->
        List.fold_left
          (fun acc t -> Set.Int.union acc (ftv t))
          Set.Int.empty tup
    | TOption t -> ftv t
    | TFunc (t1, t2) -> Set.Int.union (ftv t1) (ftv t2)
    | TVar i -> Set.Int.singleton i

  let rec apply s = function
    | TVar n -> (
        match Map.Int.find_opt n s with
        | Some t -> t
        | None -> TVar n)
    | TFunc (l, r) -> TFunc (apply s l, apply s r)
    | TUnit -> TUnit
    | TInt -> TInt
    | TBool -> TBool
    | TReal -> TReal
    | TTuple ts -> TTuple (List.map (apply s) ts)
    | TOption t -> TOption (apply s t)

  let generalize env t : Scheme.t =
    let vars = Set.Int.diff (Type.ftv t) (Env.ftv env) in
    (vars, t)

  let var_bind u t =
    match t with
    | Type.TVar i when i = u -> Map.Int.empty
    | _ -> Map.Int.singleton u t

  let rec unify ?(loc = Location.none) t1 t2 =
    let open Reserr in
    match (t1, t2) with
    | TFunc (l, r), TFunc (l', r') ->
        let* s1 = unify ~loc l l' in
        let* s2 = unify ~loc (Type.apply s1 r) (Type.apply s1 r') in
        Subst.compose s2 s1 |> ok
    | TVar u, t | t, TVar u -> var_bind u t |> ok
    | TInt, TInt -> Map.Int.empty |> ok
    | TBool, TBool -> Map.Int.empty |> ok
    | TUnit, TUnit -> Map.Int.empty |> ok
    | TOption t1, TOption t2 -> unify ~loc t1 t2
    | TTuple ts1, TTuple ts2 ->
        fold_left
          (fun s (t1, t2) ->
            let* s1 = unify ~loc (Type.apply s t1) (Type.apply s t2) in
            Subst.compose s1 s |> ok)
          Subst.empty (List.combine ts1 ts2)
    | _ ->
        let t1_str = Format.asprintf "%a" Type.pp t1 in
        let t2_str = Format.asprintf "%a" Type.pp t2 in
        let err = Error.Unification (t1_str, t2_str) in
        error (err, loc)

  let rec pp ppf =
    let open Fmt in
    function
    | TUnit -> string ppf "unit"
    | TBool -> string ppf "bool"
    | TReal -> string ppf "real"
    | TInt -> string ppf "int"
    | TTuple ts -> pf ppf "(%a)" (list ~sep:comma pp) ts
    | TOption t -> pf ppf "%a opt" pp t
    | TFunc (l, r) -> pf ppf "%a --> %a" pp l pp r
    | TVar i -> pf ppf "_%d" i
end

and Scheme : sig
  type t = Set.Int.t * Type.t

  val ftv : t -> Set.Int.t
  val apply : Subst.t -> t -> t
  val instantiate : t -> State.t -> Type.t * State.t
end = struct
  type t = Set.Int.t * Type.t

  let ftv (vars, t) = Set.Int.diff (Type.ftv t) vars

  let apply s (vars, t) =
    let s' = Set.Int.fold Map.Int.remove vars s in
    let t' = Type.apply s' t in
    (vars, t')

  let instantiate ts state =
    let vars, t = ts in
    let vars = Set.Int.to_list vars in
    let aux state _ =
      let n, state' = State.next state in
      (state', Type.TVar n)
    in
    let state', nvars = List.fold_left_map aux state vars in
    let s = List.combine vars nvars |> Map.Int.of_list in
    (Type.apply s t, state')
end

and Env : sig
  type t = Scheme.t Map.String.t

  val empty : t
  val find_opt : string -> t -> Scheme.t option
  val add : t -> string -> Scheme.t -> t
  val remove : t -> string -> t
  val ftv : t -> Set.Int.t
  val apply : Subst.t -> t -> t
end = struct
  type t = Scheme.t Map.String.t

  let empty = Map.String.empty
  let find_opt = Map.String.find_opt
  let add env name s = Map.String.add name s env
  let remove env var = Map.String.remove var env

  let ftv env =
    Map.String.fold
      (fun _ s acc -> Set.Int.union acc (Scheme.ftv s))
      env Set.Int.empty

  let apply s env = Map.String.map (Scheme.apply s) env
end

and Subst : sig
  type t = Type.t Map.Int.t

  val empty : t
  val compose : t -> t -> t
  val compose_list : t list -> t
end = struct
  type t = Type.t Map.Int.t

  let empty = Map.Int.empty

  let compose s1 s2 =
    let s2' = Map.Int.map (Type.apply s1) s2 in
    let f _ a b =
      match (a, b) with
      | Some _, _ -> a
      | _ -> b
    in
    Map.Int.merge f s2' s1

  let compose_list = function
    | [] -> empty
    | [ x ] -> x
    | x :: xs -> List.fold_left compose x xs
end

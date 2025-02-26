module rec Type : sig
  type t =
    | TUnit
    | TBool
    | TFloat
    | TInt
    | TTuple of t list
    | TOption of t
    | TFunc of t * t
    | TVar of int

  val ftv : t -> Int.Set.t
  val apply : Subst.t -> t -> t
  val generalize : Env.t -> t -> Scheme.t
  val unify : ?loc:Location.t -> t -> t -> Subst.t Reserr.t
  val minimise : t -> Subst.t
  val pp : t Fmt.t
  val sexp_of : t -> Sexplib.Sexp.t
end = struct
  type t =
    | TUnit
    | TBool
    | TFloat
    | TInt
    | TTuple of t list
    | TOption of t
    | TFunc of t * t
    | TVar of int

  let rec ftv = function
    | TUnit | TBool | TFloat | TInt -> Int.Set.empty
    | TTuple tup ->
        List.fold_left
          (fun acc t -> Int.Set.union acc (ftv t))
          Int.Set.empty tup
    | TOption t -> ftv t
    | TFunc (t1, t2) -> Int.Set.union (ftv t1) (ftv t2)
    | TVar i -> Int.Set.singleton i

  let rec apply (s : Subst.t) = function
    | TVar n -> (
        match Int.Map.find_opt n s with
        | Some t -> t
        | None -> TVar n)
    | TFunc (l, r) -> TFunc (apply s l, apply s r)
    | TUnit -> TUnit
    | TInt -> TInt
    | TBool -> TBool
    | TFloat -> TFloat
    | TTuple ts -> TTuple (List.map (apply s) ts)
    | TOption t -> TOption (apply s t)

  let generalize env t : Scheme.t =
    let vars = Int.Set.diff (Type.ftv t) (Env.ftv env) in
    (vars, t)

  let var_bind u t =
    match t with
    | Type.TVar i when i = u -> Int.Map.empty
    | _ -> Int.Map.singleton u t

  let minimise t =
    let rec aux acc = function
      | TUnit | TInt | TBool | TFloat -> acc
      | TVar n -> (
          match Int.Map.find_opt n acc with
          | None ->
              let n' = Int.Map.cardinal acc in
              Int.Map.add n (TVar n') acc
          | Some _ -> acc)
      | TOption t -> aux acc t
      | TTuple ts -> List.fold_left aux acc ts
      | TFunc (t1, t2) ->
          let acc' = aux acc t1 in
          aux acc' t2
    in
    aux Subst.empty t

  let unify ?(loc = Location.none) t1 t2 =
    let exception Type_error in
    let open Reserr in
    let rec aux t1 t2 =
      match (t1, t2) with
      | TFunc (l, r), TFunc (l', r') ->
          let* s1 = aux l l' in
          let* s2 = aux (Type.apply s1 r) (Type.apply s1 r') in
          Subst.compose s2 s1 |> ok
      | TVar u, t | t, TVar u -> var_bind u t |> ok
      | TInt, TInt -> Int.Map.empty |> ok
      | TBool, TBool -> Int.Map.empty |> ok
      | TUnit, TUnit -> Int.Map.empty |> ok
      | TFloat, TFloat -> Int.Map.empty |> ok
      | TOption t1, TOption t2 -> aux t1 t2
      | TTuple ts1, TTuple ts2 ->
          fold_left
            (fun s (t1, t2) ->
              let* s1 = aux (Type.apply s t1) (Type.apply s t2) in
              Subst.compose s1 s |> ok)
            Subst.empty (List.combine ts1 ts2)
      | _ -> raise Type_error
    in
    try aux t1 t2
    with Type_error ->
      let t1_str = Format.asprintf "%a" Type.pp t1 in
      let t2_str = Format.asprintf "%a" Type.pp t2 in
      let err = Error.Unification (t1_str, t2_str) in
      error (err, loc)

  let rec pp ppf =
    let open Fmt in
    function
    | TUnit -> string ppf "unit"
    | TBool -> string ppf "bool"
    | TFloat -> string ppf "float"
    | TInt -> string ppf "int"
    | TTuple ts -> pf ppf "(%a)" (list ~sep:(fun ppf _ -> pf ppf " * ") pp) ts
    | TOption t -> pf ppf "%a?" pp t
    | TFunc (l, r) -> pf ppf "%a --> %a" pp l pp r
    | TVar i ->
        if i > 25 then pf ppf "'x%d" (i - 25)
        else pf ppf "'%c" (Char.chr (97 + i))

  let rec sexp_of =
    let open Sexplib.Sexp in
    let rec insert_stars = function
      | [] -> []
      | [ x ] -> [ x ]
      | x :: xs -> x :: Atom "*" :: insert_stars xs
    in
    function
    | TUnit -> Atom "unit"
    | TInt -> Atom "int"
    | TBool -> Atom "bool"
    | TFloat -> Atom "real"
    | TTuple ts -> List (insert_stars (List.map sexp_of ts))
    | TOption t -> List [ sexp_of t; Atom "?" ]
    | TFunc (t1, t2) -> List [ sexp_of t1; Atom "->"; sexp_of t2 ]
    | TVar i ->
        if i > 25 then Atom (Format.asprintf "'x%d" (i - 25))
        else Atom (Format.asprintf "'%c" (Char.chr (97 + i)))
end

and Scheme : sig
  type t = Int.Set.t * Type.t

  val ftv : t -> Int.Set.t
  val apply : Subst.t -> t -> t
  val instantiate : t -> int ref -> Type.t
  val pp : t Fmt.t
end = struct
  type t = Int.Set.t * Type.t

  let ftv (vars, t) = Int.Set.diff (Type.ftv t) vars

  let apply s (vars, t) =
    let s' = Int.Set.fold Int.Map.remove vars s in
    let t' = Type.apply s' t in
    (vars, t')

  let instantiate ts state =
    let vars, t = ts in
    let vars = Int.Set.to_list vars in
    let aux _ =
      let n = !state in
      incr state;
      Type.TVar n
    in
    let nvars = List.map aux vars in
    let s = List.combine vars nvars |> Int.Map.of_list in
    Type.apply s t

  let pp =
    let open Fmt in
    let dot ppf _ = Format.pp_print_string ppf "." in
    let pp_set ppf set =
      let elems = Int.Set.to_list set in
      let elems' =
        List.map
          (fun i ->
            if i > 25 then str "'x%d" (i - 25)
            else str "'%c" (Char.chr (97 + i)))
          elems
      in
      pf ppf "%a" (list ~sep:comma string) elems'
    in
    pair ~sep:dot (braces pp_set) Type.pp |> hovbox
end

and Env : sig
  type t = Scheme.t String.Map.t

  val empty : t
  val find_opt : string -> t -> Scheme.t option
  val add : t -> string -> Scheme.t -> t
  val remove : t -> string -> t
  val ftv : t -> Int.Set.t
  val apply : Subst.t -> t -> t
  val pp : t Fmt.t
end = struct
  type t = Scheme.t String.Map.t

  let empty = String.Map.empty
  let find_opt = String.Map.find_opt
  let add env name s = String.Map.add name s env
  let remove env var = String.Map.remove var env

  let ftv env =
    String.Map.fold
      (fun _ s acc -> Int.Set.union acc (Scheme.ftv s))
      env Int.Set.empty

  let apply s env = String.Map.map (Scheme.apply s) env

  let pp =
    String.Map.pp
      ~sep:(fun ppf _ -> Format.pp_print_string ppf " => ")
      Scheme.pp
    |> Fmt.hovbox
end

and Subst : sig
  type t = Type.t Int.Map.t

  val empty : t
  val compose : t -> t -> t
  val compose_list : t list -> t
  val pp : t Fmt.t
end = struct
  type t = Type.t Int.Map.t

  let empty = Int.Map.empty

  let compose s1 s2 =
    let s2' = Int.Map.map (Type.apply s1) s2 in
    let f _ a b =
      match (a, b) with
      | Some _, _ -> a
      | _ -> b
    in
    Int.Map.merge f s1 s2'

  let compose_list = List.fold_left compose empty

  let pp ppf map =
    let open Fmt in
    let pp_aux ppf (i, e) = pf ppf "%a => %a" Type.pp (Type.TVar i) Type.pp e in
    pf ppf "%a" (list ~sep:comma pp_aux) (Int.Map.to_list map)
end

include Type

module Set = Set.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare
  let pp = Type.pp
end)

module Map = Map.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare
  let pp = Type.pp
end)

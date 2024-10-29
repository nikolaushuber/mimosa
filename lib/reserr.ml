type err = Error.t * Location.t
type warning = Warning.t * Location.t
type 'a t = ('a, err list) result * warning list

let ok x = (Result.ok x, [])
let error x = (Result.error [ x ], [])
let warns xs = (Result.ok (), xs)
let warn x = warns [ x ]

let ( let* ) x f =
  match x with
  | Ok v, w1 ->
      let r, w2 = f v in
      (r, w1 @ w2)
  | (Error _, _) as x -> x

let ( >>= ) = ( let* )

let ( and* ) (x, wx) (y, wy) =
  let r =
    match (x, y) with
    | Error e1, Error e2 -> Error (e1 @ e2)
    | Error e, _ | _, Error e -> Error e
    | Ok x, Ok y -> Ok (x, y)
  in
  (r, wx @ wy)

let fmap f r =
  let* r = r in
  f r |> ok

let ( <$> ) = fmap

let amap f r =
  let* f = f and* r = r in
  f r |> ok

let ( <*> ) = amap

let sequence r =
  let rec aux = function
    | [] -> ok []
    | ((Ok _, _) as x) :: xs ->
        let* y = x and* ys = aux xs in
        ok (y :: ys)
    | ((Error _, _) as x) :: _ -> x
  in
  aux r

let of_option ~default = Option.fold ~none:(error default) ~some:ok

let to_option = function
  | Ok x, _ -> Some x
  | _ -> None

let map f l = List.map f l |> sequence
let concat_map f l = fmap List.concat (map f l)

let rec fold_left f acc = function
  | [] -> ok acc
  | x :: xs ->
      let* acc = f acc x in
      fold_left f acc xs

let pp_loc ppf loc =
  let file = loc.Location.loc_start.pos_fname in
  let input = Pp_loc.Input.file file in
  Pp_loc.pp ppf ~max_lines:10 ~input
    [
      ( Pp_loc.Position.of_lexing loc.loc_start,
        Pp_loc.Position.of_lexing loc.loc_end );
    ]

let pp_err ppf (err, loc) =
  let open Fmt in
  pf ppf "%a@\n%a@[%a: %a@]"
    (styled `Bold Location.print_loc)
    loc pp_loc loc (styled `Bold string) "Error" Error.pp err

let pp_warn _ _ = ()

let pp quiet pp_ok ppf =
  let open Fmt in
  function
  | Ok x, w -> (
      pp_ok ppf x;
      if not quiet then
        match w with
        | [] -> ()
        | ws -> pf stderr "%a" (list Warning.pp) ws)
  | Error es, w -> pf stderr "%a@;%a" (list pp_err) es (list pp_warn) w

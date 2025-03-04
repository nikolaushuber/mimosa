type err = Error.t * Location.t
type 'a t = ('a, err list) result

let ok = Result.ok
let error x = Result.error [ x ]
let ( let* ) = Result.bind
let ( >>= ) = ( let* )

let ( and* ) x y =
  match (x, y) with
  | Error e1, Error e2 -> Error (e1 @ e2)
  | Error e, _ | _, Error e -> Error e
  | Ok x, Ok y -> Ok (x, y)

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
    | (Ok _ as x) :: xs ->
        let* y = x and* ys = aux xs in
        ok (y :: ys)
    | (Error _ as x) :: _ -> x
  in
  aux r

let of_option ~default = Option.fold ~none:(error default) ~some:ok
let to_option = Result.to_option
let map f l = List.map f l |> sequence
let concat_map f l = fmap List.concat (map f l)

let rec fold_left f acc = function
  | [] -> ok acc
  | x :: xs ->
      let* acc = f acc x in
      fold_left f acc xs

let fold_right f l acc = fold_left (Fun.flip f) acc (List.rev l)

let fold_left_map f acc l =
  let* acc, b =
    fold_left
      (fun (acc, items) item ->
        let* acc, b = f acc item in
        (acc, b :: items) |> ok)
      (acc, []) l
  in
  (acc, List.rev b) |> ok

let fold_right_map f l acc =
  let* b, acc =
    fold_right
      (fun item (items, acc) ->
        let* b, acc = f item acc in
        (b :: items, acc) |> ok)
      l ([], acc)
  in
  (List.rev b, acc) |> ok

let pp_loc ppf loc =
  let open Fmt in
  if Location.is_none loc then ()
  else
    let file = loc.Location.loc_start.pos_fname in
    let input = Pp_loc.Input.file file in
    pf ppf "%a@\n%a"
      (styled `Bold Location.print_loc)
      loc
      (Pp_loc.pp ~max_lines:10 ~input)
      [
        ( Pp_loc.Position.of_lexing loc.loc_start,
          Pp_loc.Position.of_lexing loc.loc_end );
      ]

let pp_err ppf (err, loc) =
  let open Fmt in
  pf ppf "%a@[%a: %a@]" pp_loc loc (styled `Bold string) "Error" Error.pp err

let pp pp_ok ppf =
  let open Fmt in
  function
  | Ok x -> pp_ok ppf x
  | Error es -> (list pp_err) stderr es

let unpack = function
  | Ok x -> x
  | err ->
      pp (fun _ _ -> ()) stderr err;
      exit 1

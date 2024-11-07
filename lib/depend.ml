type 'a t = 'a list

let of_list : 'a list -> 'a t = Fun.id
let to_list : 'a t -> 'a list = Fun.id
let map f (l : 'a t) = Reserr.map f l
let fold_left f (acc : 'a) (l : 'b list) = Reserr.fold_left f acc l

let fold_left_map (f : 'acc -> 'a -> ('acc * 'b) Reserr.t) (acc : 'acc)
    (l : 'a list) : ('acc * 'b list) Reserr.t =
  let open Reserr in
  let* acc, b =
    fold_left
      (fun (acc, items) item ->
        let* acc, b = f acc item in
        (acc, b :: items) |> ok)
      (acc, []) l
  in
  (acc, List.rev b) |> ok

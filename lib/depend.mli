type 'a t

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val map : ('a -> 'b Reserr.t) -> 'a t -> 'b list Reserr.t
val fold_left : ('a -> 'b -> 'a Reserr.t) -> 'a -> 'b list -> 'a Reserr.t

val fold_left_map :
  ('acc -> 'a -> ('acc * 'b) Reserr.t) ->
  'acc ->
  'a list ->
  ('acc * 'b list) Reserr.t

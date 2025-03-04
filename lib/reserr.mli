type err = Error.t * Location.t
type 'a t

val ok : 'a -> 'a t
val error : err -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val unpack : 'a t -> 'a

val sequence : 'a t list -> 'a list t
(** [sequence rs] returns [ok] of the list of ['a] iff there is no [error] in
    [rs] *)

val fold_left : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
val fold_right : ('b -> 'a -> 'a t) -> 'b list -> 'a -> 'a t

val fold_left_map :
  ('acc -> 'a -> ('acc * 'b) t) -> 'acc -> 'a list -> ('acc * 'b list) t

val fold_right_map :
  ('a -> 'acc -> ('b * 'acc) t) -> 'a list -> 'acc -> ('b list * 'acc) t

val of_option : default:err -> 'a option -> 'a t
val to_option : 'a t -> 'a option
val map : ('a -> 'b t) -> 'a list -> 'b list t
val concat_map : ('a -> 'b list t) -> 'a list -> 'b list t
val fmap : ('a -> 'b) -> 'a t -> 'b t
val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
val amap : ('a -> 'b) t -> 'a t -> 'b t
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

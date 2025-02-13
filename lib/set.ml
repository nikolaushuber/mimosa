module Make (O : sig
  include Stdlib.Set.OrderedType

  val pp : t Fmt.t
end) =
struct
  include Stdlib.Set.Make (O)
  open Fmt

  let pp ppf t = pf ppf "{ %a }" (list ~sep:comma O.pp) (to_list t)
end

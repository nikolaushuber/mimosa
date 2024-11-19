module Make (O : sig
  include Stdlib.Set.OrderedType

  val pp : t Fmt.t
end) =
struct
  include Stdlib.Set.Make (O)
  open Fmt

  let pp ?(prefix = any "{") ?(suffix = any "}") ?(sep = comma) ppf t =
    pf ppf "%a%a%a" prefix () (list ~sep O.pp) t suffix ()
end

module Make (O : sig
  include Stdlib.Map.OrderedType

  val pp : t Fmt.t
end) =
struct
  include Stdlib.Map.Make (O)
  open Fmt

  let pp ?(prefix = nop) ?(suffix = nop) ?(sep = comma) ?(arrow = any "@ =>@ ")
      pp_v ppf t =
    let l = to_list t in
    pf ppf "%a%a%a" prefix ()
      (list ~sep (pair ~sep:arrow O.pp pp_v))
      l suffix ()
end

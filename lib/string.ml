include Stdlib.String
module Set = Set.Make (Stdlib.String)

module Map = struct
  include Map.Make (Stdlib.String)

  let pp ?(sep = Fmt.comma) pp_v ppf map =
    let open Fmt in
    let l = to_list map in
    pf ppf "%a" (list ~sep:comma (pair ~sep string pp_v)) l
end

include Stdlib.Int
module Set = Set.Make (Stdlib.Int)

module Map = struct
  include Map.Make (Stdlib.Int)

  let pp ?(sep = Fmt.comma) pp_v ppf map =
    let open Fmt in
    let l = to_list map in
    pf ppf "%a" (list ~sep:comma (pair ~sep int pp_v)) l
end

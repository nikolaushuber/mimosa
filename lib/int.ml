include Stdlib.Int

module Set = Set.Make (struct
  include Stdlib.Int

  let pp = Fmt.int
end)

module Map = Map.Make (struct
  include Stdlib.Int

  let pp = Fmt.int
end)

include Stdlib.String

module Set = Set.Make (struct
  include Stdlib.String

  let pp = Fmt.string
end)

module Map = Map.Make (struct
  include Stdlib.String

  let pp = Fmt.string
end)

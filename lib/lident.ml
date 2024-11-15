type t = Lident of string | Ldot of string * string

let make ?(package = None) name =
  match package with
  | Some p -> Ldot (p, name)
  | None -> Lident name

let flatten = function
  | Lident s -> [ s ]
  | Ldot (p, s) -> [ p; s ]

let last = function
  | Lident s | Ldot (_, s) -> s

let pp ppf =
  let open Fmt in
  function
  | Lident s -> string ppf s
  | Ldot (p, s) -> pf ppf "%s.%s" p s

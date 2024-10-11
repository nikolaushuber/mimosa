type kind =
  | Lexer_unknown_character of char
  | Lexer_eof_inside_comment
  | Non_unique_global_symbol of string * Location.t

type t = kind * Location.t

open Fmt

let pp_kind ppf = function
  | Lexer_unknown_character c -> pf ppf "Unknown character '%c'" c
  | Lexer_eof_inside_comment -> pf ppf "Reached end-of-file inside comment"
  | Non_unique_global_symbol (name, _) ->
      pf ppf "Redefinition of global symbol \"%s\"" name

let styled_list l pp = List.fold_left (fun acc x -> styled x acc) pp l
let pp_error ppf _ = styled `Bold string ppf "Error"

let pp ppf (err, loc) =
  let file = loc.Location.loc_start.pos_fname in
  let input = Pp_loc.Input.file file in
  pf ppf "%a@\n%a%a: @[%a.@]"
    (styled `Bold Location.print_loc)
    loc
    (Pp_loc.pp ~max_lines:10 ~input)
    [
      ( Pp_loc.Position.of_lexing loc.loc_start,
        Pp_loc.Position.of_lexing loc.loc_end );
    ]
    pp_error () pp_kind err

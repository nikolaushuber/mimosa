type t =
  | Lexer_unknown_character of char
  | Lexer_eof_inside_comment
  | Non_unique_global_symbol of string * Location.t
  | Local_symbol_redef of string * Location.t
  | Output_not_defined of string
  | Input_unused of string
  | Dependency_cycle of string list

open Fmt

let pp ppf = function
  | Lexer_unknown_character c -> pf ppf "Unknown character '%c'" c
  | Lexer_eof_inside_comment -> pf ppf "Reached end-of-file inside comment"
  | Non_unique_global_symbol (name, _) ->
      pf ppf "Redefinition of global symbol %s" name
  | Local_symbol_redef (name, loc) ->
      let file = loc.Location.loc_start.pos_fname in
      let input = Pp_loc.Input.file file in
      pf ppf "Redefinition of local symbol %s@\nFirst defined here:@\n%a" name
        (Pp_loc.pp ~max_lines:10 ~input)
        [
          ( Pp_loc.Position.of_lexing loc.loc_start,
            Pp_loc.Position.of_lexing loc.loc_end );
        ]
  | Output_not_defined name ->
      pf ppf "No equation found for output symbol %s" name
  | Input_unused name -> pf ppf "Input symbol %s unused" name
  | Dependency_cycle names ->
      pf ppf "Dependency between symbols: @[%a@]" (list ~sep:comma string) names

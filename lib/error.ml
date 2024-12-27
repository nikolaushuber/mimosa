type t =
  | Lexer_unknown_character of char
  | Lexer_eof_inside_comment
  | Syntax_error
  | Step_redefine of string * Location.t
  | Local_symbol_redef of string * Location.t
  | Output_not_defined of string
  | Output_any
  | Input_unused of string
  | Cycle_in_steps of string list
  | Cycle_in_equations of string list
  | Cycle_in_packages of string list
  | Package_redefinition of string
  | Unification of string * string
  | Unbound_package of string
  | Unbound_value of string
  | Missing_type_in_proto
  | Typevar_in_proto

open Fmt

let pp ppf = function
  | Lexer_unknown_character c -> pf ppf "Unknown character '%c'" c
  | Lexer_eof_inside_comment -> pf ppf "Reached end-of-file inside comment"
  | Syntax_error -> pf ppf "Syntax error"
  | Step_redefine (name, loc) ->
      let file = loc.Location.loc_start.pos_fname in
      let input = Pp_loc.Input.file file in
      pf ppf "Redefinition of step %s@\nFirst defined here:@\n%a" name
        (Pp_loc.pp ~max_lines:10 ~input)
        [
          ( Pp_loc.Position.of_lexing loc.loc_start,
            Pp_loc.Position.of_lexing loc.loc_end );
        ]
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
  | Output_any -> text ppf "Cannot use _ pattern in output of step definition"
  | Input_unused name -> pf ppf "Input symbol %s unused" name
  | Cycle_in_steps names ->
      pf ppf "Dependency cycle between steps: @[%a@]" (list ~sep:comma string)
        names
  | Cycle_in_equations names ->
      pf ppf "Dependency between symbols: @[%a@]" (list ~sep:comma string) names
  | Package_redefinition name -> pf ppf "Multiple packages with name %s" name
  | Cycle_in_packages names ->
      pf ppf "Cyclic dependency between packages: @[%a@]"
        (list ~sep:comma string) names
  | Unification (t1, t2) ->
      pf ppf
        "This expression has type %s, but an expression of type %s was expected"
        t2 t1
  | Unbound_package name -> pf ppf "Undefined package %s" name
  | Unbound_value name -> pf ppf "Unknown name %s" name
  | Missing_type_in_proto ->
      pf ppf "%a@\n%a" text "Missing type annotation" text
        "Externally defined steps must specify their argument types"
  | Typevar_in_proto ->
      pf ppf "%a@\n%a" text "Type variable in external step" text
        "Externally defined steps cannot use type variables in their signature"

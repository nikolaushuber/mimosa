type t =
  | Channel_multiple_use of [ `Write | `Read ] * string * Location.t
  | Channel_unused of [ `Write | `Read ] * string
  | Dependency_cycle of [ `Steps | `Equations ] * string list
  | Input_unused of string
  | Missing_type_in_proto
  | Optional_port_type of string * string
  | Output_any
  | Output_not_defined of string
  | Parser_error of [ `Unknown_char of char | `Eof_in_comment | `Syntax_error ]
  | Symbol_redefinition of
      [ `Step | `Local_sym | `Channel | `Node ] * string * Location.t
  | Unexpected_typevar of [ `Channel | `Proto ]
  | Unification of string * string
  | Uninitialized_sequence of string option
  | Unknown_symbol of [ `Step | `Channel | `Var ] * string

open Fmt

let pp ppf = function
  | Channel_multiple_use (dir, name, loc) ->
      let file = loc.Location.loc_start.pos_fname in
      let input = Pp_loc.Input.file file in
      pf ppf "Channel %s is %s multiple times.@\nIt was also %s here:@\n%a" name
        (match dir with
        | `Write -> "written to"
        | `Read -> "read from")
        (match dir with
        | `Write -> "written"
        | `Read -> "read")
        (Pp_loc.pp ~max_lines:10 ~input)
        [
          ( Pp_loc.Position.of_lexing loc.loc_start,
            Pp_loc.Position.of_lexing loc.loc_end );
        ]
  | Channel_unused (dir, name) ->
      pf ppf "Channel %s is never %s" name
        (match dir with
        | `Write -> "written to"
        | `Read -> "read from")
  | Dependency_cycle (kind, names) ->
      pf ppf "Dependency cycle between %s: @[%a@]"
        (match kind with
        | `Steps -> "steps"
        | `Equations -> "equations")
        (list ~sep:comma string) names
  | Input_unused name -> pf ppf "Input symbol %s unused" name
  | Symbol_redefinition (kind, name, loc) ->
      let file = loc.Location.loc_start.pos_fname in
      let input = Pp_loc.Input.file file in
      pf ppf "Redefinition of %s %s@\nAlso defined here:@\n%a"
        (match kind with
        | `Step -> "step"
        | `Local_sym -> "local symbol"
        | `Channel -> "channel"
        | `Node -> "node")
        name
        (Pp_loc.pp ~max_lines:10 ~input)
        [
          ( Pp_loc.Position.of_lexing loc.loc_start,
            Pp_loc.Position.of_lexing loc.loc_end );
        ]
  | Missing_type_in_proto ->
      pf ppf "%a@\n%a" text "Missing type annotation" text
        "Externally defined steps must specify their argument types"
  | Output_any -> text ppf "Cannot use _ pattern in output of step definition"
  | Optional_port_type (name, ty) ->
      pf ppf
        "Port %s is marked as optional@\n\
         The port has type %s, but an option type was expected"
        name ty
  | Output_not_defined name ->
      pf ppf "No equation found for output symbol %s" name
  | Parser_error kind ->
      pf ppf "Error during parsing:@\n%a" text
        (match kind with
        | `Unknown_char c -> str "Unknown character '%c'" c
        | `Eof_in_comment -> "EOF reached inside comment"
        | `Syntax_error -> "Syntax error")
  | Unexpected_typevar kind ->
      pf ppf "Unexpect type variable:@\n%a" text
        (match kind with
        | `Channel -> "Channels must use monomorphic types"
        | `Proto ->
            "Externally defined steps cannot use type variables in their \
             signature")
  | Unification (t1, t2) ->
      pf ppf
        "This expression has type %s, but an expression of type %s was expected"
        t2 t1
  | Uninitialized_sequence msg ->
      pf ppf "%a@\n%a" text "Uninitialized sequence" (option text) msg
  | Unknown_symbol (kind, name) ->
      pf ppf "Unknown %s %s"
        (match kind with
        | `Step -> "step"
        | `Channel -> "channel"
        | `Var -> "variable")
        name

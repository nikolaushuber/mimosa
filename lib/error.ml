type kind = Lexer_unknown_character of char | Lexer_eof_inside_comment
type t = kind * Location.t

let pp _ _ = ()

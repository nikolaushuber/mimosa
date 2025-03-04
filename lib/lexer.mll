{
    open Parser
    exception LexError of Reserr.err
}

let space = [' ' '\t' '\r']
let newline = ('\013'* '\010')
let digit = ['0' - '9']
let lower = ['a' - 'z']
let upper = ['A' - 'Z']
let alpha = (lower | upper)
let identifier = ('_' | alpha ) (alpha | digit | "_")*
let lower_ident = ('_' | lower ) (alpha | digit | '_')*
let upper_ident = upper (alpha | digit | '_')*
let integer = digit+
let float = digit+ "." digit+


rule tokenize = parse
    | space { tokenize lexbuf }
    | newline { Lexing.new_line lexbuf; tokenize lexbuf }
    | "(*" { read_comment lexbuf }

    (* Unary operators *)
    | "!" { TK_NOT }
    | "pre" { TK_PRE }
    | "?" { TK_QUESTIONMARK }

    (* Binary operators *)
    | "&&" { TK_AND }
    | "||" { TK_OR }
    | "=>" { TK_IMPLIES }
    | "+" { TK_ADD }
    | "-" { TK_SUB }
    | "*" { TK_MUL }
    | "/" { TK_DIV }
    | "+." { TK_FADD }
    | "-." { TK_FSUB }
    | "*." { TK_FMUL }
    | "/." { TK_FDIV }
    | "==" { TK_EQ }
    | "!=" { TK_NEQ }
    | "<" { TK_LT }
    | "<=" { TK_LEQ }
    | ">" { TK_GT }
    | ">=" { TK_GEQ }
    | "<." { TK_FLT }
    | "<=." { TK_FLEQ }
    | ">." { TK_FGT }
    | ">=." { TK_FGEQ }
    | "->" { TK_ARROW }
    | "fby" { TK_FBY }

    (* Keywords *)
    | "step" { TK_STEP }
    | "if" { TK_IF }
    | "then" { TK_THEN }
    | "else" { TK_ELSE }
    | "channel" { TK_CHANNEL }
    | "node" { TK_NODE }
    | "implements" { TK_IMPLEMENTS }
    | "every" { TK_EVERY }
    | "ms" { TK_MS }
    | "either" { TK_EITHER }
    | "or" { TK_EITHER_OR }

    (* Types *)
    | "int" { TK_TY_INT }
    | "bool" { TK_TY_BOOL }
    | "float" { TK_TY_FLOAT }

    (* Constants *)
    | "true" { TK_BOOL true }
    | "false" { TK_BOOL false }
    | "None" { TK_NONE }
    | "Some" { TK_SOME }
    | "_" { TK_ANY }
    | integer as i { TK_INT (int_of_string i) }
    | float as f { TK_FLOAT f }
    | lower_ident as id { TK_STRING id }

    (* Others *)
    | "(" { TK_LPAREN }
    | ")" { TK_RPAREN }
    | "{" { TK_LBRACE }
    | "}" { TK_RBRACE }
    | "," { TK_COMMA }
    | "=" { TK_ASSIGN }
    | ";" { TK_SEMI }
    | ":" { TK_COLON }
    | eof { TK_EOF }
    | _ as e {
        let open Error in
        let loc = Location.{
            loc_start = Lexing.lexeme_start_p lexbuf;
            loc_end = Lexing.lexeme_end_p lexbuf;
            loc_ghost = false
        } in
        let kind = Parser_error (`Unknown_char e) in
        raise ( LexError (kind, loc) )
    }

and read_comment = parse 
    | "*)" { tokenize lexbuf }
    | newline { Lexing.new_line lexbuf; read_comment lexbuf }
    | eof { 
        let open Error in
        let loc = Location.{
            loc_start = Lexing.lexeme_start_p lexbuf;
            loc_end = Lexing.lexeme_end_p lexbuf;
            loc_ghost = false
        } in
        let kind = Parser_error (`Eof_in_comment) in 
        raise (LexError (kind, loc))
    }
    | _ { read_comment lexbuf }

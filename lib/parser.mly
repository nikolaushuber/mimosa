%{
    open Ptree_builder

    let package_of_loc pos =
        pos.Lexing.pos_fname
        |> Filename.basename
        |> Filename.chop_extension
        |> String.capitalize_ascii

    let to_loc (loc_start, loc_end) = Location.{
        loc_start;
        loc_end;
        loc_ghost = false;
    }
%}

(* Unary operators *)
%token TK_NOT "not"
%token TK_PRE "pre"
%token TK_QUESTIONMARK "?"

(* Binary operators *)
%token TK_AND "&&"
%token TK_OR "||"
%token TK_IMPLIES "=>"
%token TK_ADD "+"
%token TK_SUB "-"
%token TK_MUL "*"
%token TK_DIV "/"
%token TK_EQ "=="
%token TK_NEQ "!="
%token TK_LT "<"
%token TK_LEQ "<="
%token TK_GT ">"
%token TK_GEQ ">="
%token TK_ARROW "->"
%token TK_FBY "fby"

(* Keywords *)
%token TK_PACKAGE "package"
%token TK_STEP "step"
%token TK_IF "if"
%token TK_THEN "then"
%token TK_ELSE "else"
%token TK_CHANNEL "channel"
%token TK_REGISTER "register"
%token TK_NODE "node"
%token TK_IMPLEMENTS "implements"
%token TK_EVERY "every"
%token TK_MS "ms"
%token TK_ASYNC "async"
%token TK_NONE "None"
%token TK_SOME "Some"
%token TK_EITHER "either"
%token TK_EITHER_OR "or"
%token TK_ANY "_"

(* Types *)
%token TK_TY_INT "int"
%token TK_TY_BOOL "bool"

(* Constants *)
%token <int> TK_INT
%token <float> TK_FLOAT
%token <string> TK_LSTRING
%token <string> TK_USTRING
%token <bool> TK_BOOL

(* Others *)
%token TK_LPAREN "("
%token TK_RPAREN ")"
%token TK_LBRACE "{"
%token TK_RBRACE "}"
%token TK_COMMA ","
%token TK_ASSIGN "="
%token TK_SEMI ";"
%token TK_COLON ":"
%token TK_BAR "|"
%token TK_EOF "eof"
%token TK_DOT "."

%nonassoc EITHER
%nonassoc PREC_TUPLE
%left ","
%left "else"
%right "->" "fby"
%left "=>"
%left "||"
%left "&&"
%left "==" "!=" "<" "<=" ">" ">="
%right PREC_UNARY_NOT
%left "+" "-"
%left "*" "/"
%right "pre"
%right PREC_UNARY_MINUS "?"
%nonassoc "Some"

%start <Ptree.t> parse
%%

located_lower_string:
    | s = TK_LSTRING { located ~loc:(to_loc $loc) s }

located_upper_string:
    | s = TK_USTRING { located ~loc:(to_loc $loc) s }

parse:
    | pack = package tl = toplevel_def* "eof" {
        package pack tl
    }

package:
    | "package" name = located_upper_string { name }
    | { located (package_of_loc $startpos) }

toplevel_def:
    | s = step { pack_step s }

step:
    |   "step"
        name = located_lower_string
        ps = simple_pattern
        "="
        rs = simple_pattern
        "{" body = step_body "}"
    {
        step ~loc:(to_loc $loc) name ps rs body
    }

simple_ty:
    | "int" { tint ~loc:(to_loc $loc) () }
    | "bool" { tbool ~loc:(to_loc $loc) () }
    | "(" t = ty ")" { t }

ty:
    | t = simple_ty { t }
    | t = ty "?" { toption ~loc:(to_loc $loc) t }
    | t = tuple_ty %prec PREC_TUPLE { ttuple ~loc:(to_loc $loc) t }

tuple_ty:
    | tty = tuple_ty "*" t = ty { tty @ [t] }
    | ty1 = ty "*" ty2 = ty { [ty1; ty2] }

step_body:
    | s = step_stmt ";"? { [s] }
    | s = step_stmt ";" sl = step_body { s :: sl }

step_stmt:
    | eq = equation { eq }

equation:
    | l = pattern "=" e = expr { l, e }

simple_pattern:
    | "(" ")" { punit ~loc:(to_loc $loc) () }
    | id = located_lower_string { pvar ~loc:(to_loc $loc) None id }
    | id = located_lower_string ":" ty = ty { pvar ~loc:(to_loc $loc) (Some ty) id }
    | "_" { pany ~loc:(to_loc $loc) None }
    | "_" ":" ty = ty { pany ~loc:(to_loc $loc) (Some ty) }
    | "(" l = pattern ")" { l }

pattern:
    | p = simple_pattern { p }
    | t = pattern_tuple %prec PREC_TUPLE { ptuple ~loc:(to_loc $loc) None t }

pattern_tuple:
    | lt = pattern_tuple "," l = pattern { lt @ [l] }
    | l1 = pattern "," l2 = pattern { [l1; l2] }

ident_loc:
    | s = TK_LSTRING { located ~loc:(to_loc $loc) (Lident.make s) }
    | p = TK_USTRING "." s = TK_LSTRING {
        located ~loc:(to_loc $loc) (Lident.make ~package:(Some p) s)
    }

literal_constant:
    | i = TK_INT { eint ~loc:(to_loc $loc) i }
    | b = TK_BOOL { ebool ~loc:(to_loc $loc) b }
    | "(" ")" { eunit ~loc:(to_loc $loc) () }

simple_expr:
    | "(" e = expr ")" { e }
    | c = literal_constant { c }
    | v = ident_loc { evar ~loc:(to_loc $loc) v }
    | "None" { enone ~loc:(to_loc $loc) () }

expr:
    | e = simple_expr { e }

    (* Unary operations *)
    | "not" e = expr %prec PREC_UNARY_NOT {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Punop_not in
        eunop ~loc:(to_loc $loc) op e
    }
    | "-" e = expr %prec PREC_UNARY_MINUS {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Punop_neg in
        eunop ~loc:(to_loc $loc) op e
    }
    | "pre" e = expr {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Punop_pre in
        eunop ~loc:(to_loc $loc) op e
    }
    | "?" e = expr {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Punop_is_some in
        eunop ~loc:(to_loc $loc) op e
    }

    (* Some constructor *)
    | "Some" e = expr { esome ~loc:(to_loc $loc) e }

    (* Binary operations *)
    | e1 = expr op = binop e2 = expr { ebinop ~loc:(to_loc $loc) op e1 e2 }

    (* Conditional statement *)
    | "if" cond = expr "then" e1 = expr "else" e2 = expr {
        eite ~loc:(to_loc $loc) cond e1 e2
    }

    (* Step application *)
    | func = ident_loc arg = simple_expr {
        eapply ~loc:(to_loc $loc) func arg
    }

    (* Tuple construction *)
    | el = tuple_elements %prec PREC_TUPLE { etuple ~loc:(to_loc $loc) el }

    (* Either/or *)
    | "either" eo = expr "or" e = expr %prec EITHER {
        eeither ~loc:(to_loc $loc) eo e
    }

tuple_elements:
    | te = tuple_elements "," e = expr { te @ [e] }
    | e1 = expr "," e2 = expr { [e1; e2] }

%inline binop:
    | "&&" { binop ~loc:(to_loc $loc) Ptree.Pbinop_and }
    | "||" { binop ~loc:(to_loc $loc) Ptree.Pbinop_or }
    | "=>" { binop ~loc:(to_loc $loc) Ptree.Pbinop_implies }
    | "+" { binop ~loc:(to_loc $loc) Ptree.Pbinop_add }
    | "-" { binop ~loc:(to_loc $loc) Ptree.Pbinop_sub }
    | "*" { binop ~loc:(to_loc $loc) Ptree.Pbinop_mul }
    | "/" { binop ~loc:(to_loc $loc) Ptree.Pbinop_div }
    | "==" { binop ~loc:(to_loc $loc) Ptree.Pbinop_eq }
    | "!=" { binop ~loc:(to_loc $loc) Ptree.Pbinop_neq }
    | "<" { binop ~loc:(to_loc $loc) Ptree.Pbinop_lt }
    | "<=" { binop ~loc:(to_loc $loc) Ptree.Pbinop_leq }
    | ">" { binop ~loc:(to_loc $loc) Ptree.Pbinop_gt }
    | ">=" { binop ~loc:(to_loc $loc) Ptree.Pbinop_geq }
    | "->" { binop ~loc:(to_loc $loc) Ptree.Pbinop_arrow }
    | "fby" { binop ~loc:(to_loc $loc) Ptree.Pbinop_fby }

/* Definition of system file parser 
 * This could go into its own file, but ultimately we should 
 * probably combine .mim and .msys files into one general
 * description format. 
 */ 

// parse_sys: 
//     | l = topdef_sys* "eof" { 
//         package_of_loc $startpos, l 
//     }

// topdef_sys: 
//     | "node" name = TK_LSTRING "implements" 
//       package = TK_USTRING "::" step = TK_LSTRING 
//       "(" inputs = separated_list(",", param_sys) ")" "=" 
//       "(" outputs = separated_list(",", param_sys) ")" 
//       "every" time = TK_INT tunit = time_unit {
//         let open Sys_ast in 
//         let period = time, tunit in 
//         let desc = mk_node_desc package step inputs outputs period in 
//         name, mk_node $loc desc 
//     }
//     | "channel" name = TK_LSTRING ":" ty = ty {
//         let open Sys_ast in 
//         name, mk_link $loc (mk_channel ty)  
//     } 
//     | "register" name = TK_LSTRING ":" ty = ty "=" l = literal_constant {
//         let open Sys_ast in 
//         let e, _ = l in 
//         name, mk_link $loc (mk_register ty e) 
//     }

// time_unit: 
//     | "ms" { Sys_ast.mk_ms () } 

// param_sys:
//     | async = "async"? name = TK_LSTRING { 
//         let open Sys_ast in 
//         mk_port $loc name (Option.is_some async) 
//     }

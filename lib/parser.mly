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
%token TK_NOT "~"
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
%token TK_RADD "+."
%token TK_RSUB "-."
%token TK_RMUL "*."
%token TK_RDIV "/."
%token TK_EQ "=="
%token TK_NEQ "!="
%token TK_LT "<"
%token TK_LEQ "<="
%token TK_GT ">"
%token TK_GEQ ">="
%token TK_RLT "<."
%token TK_RLEQ "<=."
%token TK_RGT ">."
%token TK_RGEQ ">=."
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
%token TK_TY_REAL "real"

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
%left "==" "!=" "<" "<=" ">" ">=" "<." "<=." ">." ">=."
%right PREC_UNARY_NOT
%left "+" "-" "+." "-."
%left "*" "/" "*." "/."
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
    | p = proto { pack_proto p }
    | n = node { pack_node n }
    | l = link { pack_link l }

step:
    |   "step"
        name = located_lower_string
        ps = simple_pattern
        "-" "->"
        rs = simple_pattern
        "{" body = step_body "}"
    {
        step ~loc:(to_loc $loc) name ps rs body
    }

proto:
    |   "step"
        name = located_lower_string
        ps = simple_pattern
        "-" "->"
        rs = simple_pattern
    {
        proto ~loc:(to_loc $loc) name ps rs
    }

link:
    | "channel" name = TK_LSTRING ":" ty = ty {
        channel ~loc:(to_loc $loc) name ty
    }
    | "register" name = TK_LSTRING ":" ty = ty "=" e = literal_constant{
        register ~loc:(to_loc $loc) name ty e
    }

node:
    | "node" name = located_lower_string
      "implements" impl = ident_loc
      "(" inputs = separated_list(",", node_port) ")"
      "-" "->"
      "(" outputs = separated_list(",", node_port) ")"
      "every" period = periodicity
    {
      node ~loc:(to_loc $loc) name impl inputs outputs period
    }

periodicity:
    | time = TK_INT tunit = time_unit {
        period ~loc:(to_loc $loc) time tunit
    }

time_unit:
    | "ms" { ms }

node_port:
    | async = "async"? name = ident_loc {
         port ~loc:(to_loc $loc) name (Option.is_some async)
    }

simple_ty:
    | "int" { tint ~loc:(to_loc $loc) () }
    | "bool" { tbool ~loc:(to_loc $loc) () }
    | "real" { treal ~loc:(to_loc $loc) () }
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
    | f = TK_FLOAT { ereal ~loc:(to_loc $loc) f }
    | "(" ")" { eunit ~loc:(to_loc $loc) () }

simple_expr:
    | "(" e = expr ")" { e }
    | c = literal_constant { c }
    | v = ident_loc { evar ~loc:(to_loc $loc) v }
    | "None" { enone ~loc:(to_loc $loc) () }

expr:
    | e = simple_expr { e }

    (* Unary operations *)
    | "~" e = expr %prec PREC_UNARY_NOT {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Punop_not in
        eunop ~loc:(to_loc $loc) op e
    }
    | "-" e = expr %prec PREC_UNARY_MINUS {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Punop_neg in
        eunop ~loc:(to_loc $loc) op e
    }
    | "-." e = expr %prec PREC_UNARY_MINUS {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Punop_rneg in
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

    (* Temporal operations *)
    | e1 = expr "->" e2 = expr { earrow ~loc:(to_loc $loc) e1 e2 }
    | e1 = expr "fby" e2 = expr { efby ~loc:(to_loc $loc) e1 e2 }
    | "pre" e = expr { epre ~loc:(to_loc $loc) e }

    (* Conditional statement *)
    | "if" cond = expr "then" e1 = expr "else" e2 = expr {
        eite ~loc:(to_loc $loc) cond e1 e2
    }

    (* Step application *)
    | func = simple_expr arg = simple_expr {
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
    | "+." { binop ~loc:(to_loc $loc) Ptree.Pbinop_radd }
    | "-." { binop ~loc:(to_loc $loc) Ptree.Pbinop_rsub }
    | "*." { binop ~loc:(to_loc $loc) Ptree.Pbinop_rmul }
    | "/." { binop ~loc:(to_loc $loc) Ptree.Pbinop_rdiv }
    | "==" { binop ~loc:(to_loc $loc) Ptree.Pbinop_eq }
    | "!=" { binop ~loc:(to_loc $loc) Ptree.Pbinop_neq }
    | "<" { binop ~loc:(to_loc $loc) Ptree.Pbinop_lt }
    | "<=" { binop ~loc:(to_loc $loc) Ptree.Pbinop_leq }
    | ">" { binop ~loc:(to_loc $loc) Ptree.Pbinop_gt }
    | ">=" { binop ~loc:(to_loc $loc) Ptree.Pbinop_geq }
    | "<." { binop ~loc:(to_loc $loc) Ptree.Pbinop_rlt }
    | "<=." { binop ~loc:(to_loc $loc) Ptree.Pbinop_rleq }
    | ">." { binop ~loc:(to_loc $loc) Ptree.Pbinop_rgt }
    | ">=." { binop ~loc:(to_loc $loc) Ptree.Pbinop_rgeq }

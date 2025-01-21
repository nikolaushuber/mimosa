%{
    open Ptree_builder

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
%token TK_FADD "+."
%token TK_FSUB "-."
%token TK_FMUL "*."
%token TK_FDIV "/."
%token TK_EQ "=="
%token TK_NEQ "!="
%token TK_LT "<"
%token TK_LEQ "<="
%token TK_GT ">"
%token TK_GEQ ">="
%token TK_FLT "<."
%token TK_FLEQ "<=."
%token TK_FGT ">."
%token TK_FGEQ ">=."
%token TK_ARROW "->"
%token TK_FBY "fby"

(* Keywords *)
%token TK_STEP "step"
%token TK_IF "if"
%token TK_THEN "then"
%token TK_ELSE "else"
%token TK_CHANNEL "channel"
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
%token TK_TY_FLOAT "float"

(* Constants *)
%token <int> TK_INT
%token <float> TK_FLOAT
%token <string> TK_STRING
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

parse:
    | tl = toplevel_def* "eof" { tl }

with_loc(X):
    | x = X { located ~loc:(to_loc $loc) x }

toplevel_def:
    | s = step { s }
    | p = proto { p }
    | n = node { n }
    | c = channel { c }

step:
    |   "step"
        name = with_loc(TK_STRING)
        ps = simple_pattern
        "-" "->"
        rs = simple_pattern
        "{" body = step_body "}"
    {
        step ~loc:(to_loc $loc) name ps rs body
    }

proto:
    |   "step"
        name = with_loc(TK_STRING)
        ps = simple_pattern
        "-" "->"
        rs = simple_pattern
    {
        proto ~loc:(to_loc $loc) name ps rs
    }

channel:
    | "channel" name = TK_STRING ":" ty = ty {
        channel ~loc:(to_loc $loc) name ty []
    }

node:
    | "node" name = with_loc(TK_STRING)
      "implements" impl = with_loc(TK_STRING)
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
    | async = "async"? name = with_loc(TK_STRING) {
         port ~loc:(to_loc $loc) name (Option.is_some async)
    }

simple_ty:
    | "int" { tint ~loc:(to_loc $loc) () }
    | "bool" { tbool ~loc:(to_loc $loc) () }
    | "float" { tfloat ~loc:(to_loc $loc) () }
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
    | id = with_loc(TK_STRING) { pvar ~loc:(to_loc $loc) None id }
    | id = with_loc(TK_STRING) ":" ty = ty { pvar ~loc:(to_loc $loc) (Some ty) id }
    | "_" { pany ~loc:(to_loc $loc) None }
    | "_" ":" ty = ty { pany ~loc:(to_loc $loc) (Some ty) }
    | "(" l = pattern ")" { l }

pattern:
    | p = simple_pattern { p }
    | t = pattern_tuple %prec PREC_TUPLE { ptuple ~loc:(to_loc $loc) None t }

pattern_tuple:
    | lt = pattern_tuple "," l = pattern { lt @ [l] }
    | l1 = pattern "," l2 = pattern { [l1; l2] }

literal_constant:
    | i = TK_INT { eint ~loc:(to_loc $loc) i }
    | b = TK_BOOL { ebool ~loc:(to_loc $loc) b }
    | f = TK_FLOAT { efloat ~loc:(to_loc $loc) f }
    | "(" ")" { eunit ~loc:(to_loc $loc) () }

simple_expr:
    | "(" e = expr ")" { e }
    | c = literal_constant { c }
    | v = TK_STRING { evar ~loc:(to_loc $loc) v }
    | "None" { enone ~loc:(to_loc $loc) () }

expr:
    | e = simple_expr { e }

    (* Unary operations *)
    | "~" e = expr %prec PREC_UNARY_NOT {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Unop_not in
        eunop ~loc:(to_loc $loc) op e
    }
    | "-" e = expr %prec PREC_UNARY_MINUS {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Unop_neg in
        eunop ~loc:(to_loc $loc) op e
    }
    | "-." e = expr %prec PREC_UNARY_MINUS {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Unop_fneg in
        eunop ~loc:(to_loc $loc) op e
    }
    | "?" e = expr {
        let op = unop ~loc:(to_loc $loc($1)) Ptree.Unop_is_some in
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
    | "&&" { binop ~loc:(to_loc $loc) Ptree.Binop_and }
    | "||" { binop ~loc:(to_loc $loc) Ptree.Binop_or }
    | "=>" { binop ~loc:(to_loc $loc) Ptree.Binop_implies }
    | "+" { binop ~loc:(to_loc $loc) Ptree.Binop_add }
    | "-" { binop ~loc:(to_loc $loc) Ptree.Binop_sub }
    | "*" { binop ~loc:(to_loc $loc) Ptree.Binop_mul }
    | "/" { binop ~loc:(to_loc $loc) Ptree.Binop_div }
    | "+." { binop ~loc:(to_loc $loc) Ptree.Binop_fadd }
    | "-." { binop ~loc:(to_loc $loc) Ptree.Binop_fsub }
    | "*." { binop ~loc:(to_loc $loc) Ptree.Binop_fmul }
    | "/." { binop ~loc:(to_loc $loc) Ptree.Binop_fdiv }
    | "==" { binop ~loc:(to_loc $loc) Ptree.Binop_eq }
    | "!=" { binop ~loc:(to_loc $loc) Ptree.Binop_neq }
    | "<" { binop ~loc:(to_loc $loc) Ptree.Binop_lt }
    | "<=" { binop ~loc:(to_loc $loc) Ptree.Binop_leq }
    | ">" { binop ~loc:(to_loc $loc) Ptree.Binop_gt }
    | ">=" { binop ~loc:(to_loc $loc) Ptree.Binop_geq }
    | "<." { binop ~loc:(to_loc $loc) Ptree.Binop_flt }
    | "<=." { binop ~loc:(to_loc $loc) Ptree.Binop_fleq }
    | ">." { binop ~loc:(to_loc $loc) Ptree.Binop_fgt }
    | ">=." { binop ~loc:(to_loc $loc) Ptree.Binop_fgeq }

open Ooir
open Fmt

let pp_expr ppf e =
  match e.expr_desc with
  | Var s -> string ppf s
  | StateVar s -> pf ppf "!%s" s
  | Const c -> Ttree_printer.pp_const ppf c
  | GlobalConst id -> Lident.pp ppf id
  | None -> string ppf "None"
  | Some s -> pf ppf "@[Some@;%s@]" s
  | UnOp (op, e) -> pf ppf "%a %s" Ttree_printer.pp_unop op e
  | BinOp (op, e1, e2) ->
      pf ppf "@[<2>%s@;%a@;%s@]" e1 Ttree_printer.pp_binop op e2

let rec pp_instr ppf = function
  | Assign (lhs, rhs) -> pf ppf "@[<2>%s@;<1 2>=@;%a@]" lhs pp_expr rhs
  | StateAssign (lhs, rhs) -> pf ppf "@[<2>%s@;<1 2>:=@;%a@]" lhs pp_expr rhs
  | TupleConstr (lhs, rhs) ->
      pf ppf "@[<2>%s@;<1 2>=@;%a@]" lhs (list ~sep:comma string) rhs
  | TupleDestr (lhs, rhs) ->
      pf ppf "@[<2>%a@;<1 2>=@;%s@]" (list ~sep:comma string) lhs rhs
  | Reset (f, self) -> pf ppf "%a.reset( %s )" Lident.pp f self
  | Return s -> pf ppf "@[<2>return@;%s@]" s
  | If (c, t, e) ->
      let fmt : (_, _, _) format =
        "@[<hv0>@[<2>if@ %s@]@;@[<2>then@ %a@]@;@[<2>else@;%a@]@]"
      in
      let pp_list = list ~sep:(fun ppf _ -> pf ppf ";@;<1 2>") pp_instr in
      pf ppf fmt c pp_list t pp_list e
  | StepApp (None, f, args, self) ->
      pf ppf "@[<2>%a@;(@;%a,@ %s@;<1 -2>)@]" Lident.pp f
        (list ~sep:comma string) args self
  | StepApp (Some v, f, args, self) ->
      pf ppf "@[<2>%s@;=@;%a@;(@;%a,@ %s@;<1 -2>)@]" v Lident.pp f
        (list ~sep:comma string) args self
  | Either (lhs, e1, e2) ->
      let pp_either ppf (lhs, e) =
        pf ppf "@[<hov2>|@ Some %s:@]@;@[<hov2>%s@ =@ %s@]" e lhs e
      in
      let pp_or ppf e =
        let pp_list = list ~sep:(fun ppf _ -> pf ppf ";@;<1 2>") pp_instr in
        pf ppf "@[<hov2>|@ None:@]@;%a" pp_list e
      in
      pf ppf "@[<v2>@[<hov>case@ (%s)@ of@]@;<0 -2>%a@;<0 -2>%a@]" e1 pp_either
        (lhs, e1) pp_or e2

let pp_machine ppf m =
  pf ppf
    "@[@[<2>machine@;\
     %s@]@\n\
     @[<2>memory:@;\
     %a@]@\n\
     @[<2>instances:@;\
     %a@]@\n\
     @[<2>reset:@;\
     %a@]@\n\
     @[<2>inputs:@;\
     %a@]@\n\
     @[<v>body:@;\
     <1 2>%a@]@\n"
    m.name
    (list ~sep:comma (pair ~sep:Norm_printer.colon string Type.pp))
    m.memory
    (list ~sep:comma (pair ~sep:Norm_printer.colon string Lident.pp))
    m.instances
    (list ~sep:(fun ppf _ -> pf ppf ";@;<1 2>") pp_instr)
    m.reset
    (list ~sep:comma (pair ~sep:Norm_printer.colon string Type.pp))
    m.inputs
    (list ~sep:(fun ppf _ -> pf ppf ";@;<1 2>") pp_instr)
    m.def

let pp_proto ppf p =
  pf ppf "@[@[<2>proto@;%s@]@\n@[<2>inputs:@;%a@]@\n@]@\n" p.proto_name
    (list ~sep:comma (pair ~sep:Norm_printer.colon string Type.pp))
    p.proto_inputs

let pp_items pp ppf l =
  pf ppf "@[<v0>%a@]" (list ~sep:(fun ppf _ -> pf ppf "@\n") pp) l

let pp ppf p =
  pf ppf "@[<v>@[package %s@]@;@;%a@;%a@]" p.pack_name (pp_items pp_proto)
    p.pack_protos (pp_items pp_machine) p.pack_machines

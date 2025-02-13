open Sim_ast

module type Sim = sig
  type t

  val init : unit -> t
  val exec_ms : t -> t
end

let ( +: ) = Int64.add
let ( +:. ) x y = Int64.(add x (of_int y))

let rec expr_of_val = function
  | VUnit -> EUnit
  | VBool b -> EBool b
  | VInt i -> EInt i
  | VFloat f -> EFloat f
  | VOption None -> ENone
  | VOption (Some v) -> ESome (expr_of_val v)
  | VLambda (p1, p2, b) -> ELam (p1, p2, b)
  | VTuple vs -> ETuple (List.map expr_of_val vs)
  | VExtern f -> EExtern f
  | VUndef -> EUndef

let rec env_proj env = function
  | PVar x -> List.assoc x env
  | PUnit -> VUnit
  | PAny -> VUndef
  | PTuple ps -> VTuple (List.map (env_proj env) ps)

let rec eval env = function
  | EUndef -> (VUndef, fun _ -> EUndef)
  | EUnit -> (VUnit, fun _ -> EUnit)
  | EInt i -> (VInt i, fun _ -> EInt i)
  | EBool b -> (VBool b, fun _ -> EBool b)
  | EFloat f -> (VFloat f, fun _ -> EFloat f)
  | EVar v -> (
      try (List.assoc v env, fun _ -> EVar v)
      with Not_found -> failwith ("Unknown variable " ^ v))
  | EUnOp (op, e) ->
      let v, cont = eval env e in
      (eval_unop op v, fun env' -> EUnOp (op, cont env'))
  | EBinOp (op, e1, e2) ->
      let v1, cont1 = eval env e1 in
      let v2, cont2 = eval env e2 in
      (eval_binop op v1 v2, fun env' -> EBinOp (op, cont1 env', cont2 env'))
  | EEither (e1, e2) -> (
      let v, cont1 = eval env e1 in
      match v with
      | VOption (Some v) -> (v, fun env' -> EEither (cont1 env', e2))
      | VOption None ->
          let v, cont2 = eval env e2 in
          (v, fun env' -> EEither (cont1 env', cont2 env'))
      | _ -> assert false)
  | ETuple es ->
      let vs, conts = List.map (eval env) es |> List.split in
      (VTuple vs, fun env' -> ETuple (List.map (fun x -> x env') conts))
  | EIf (c, t, e) -> (
      let vc, contc = eval env c in
      match vc with
      | VBool true ->
          let vt, contt = eval env t in
          (vt, fun env' -> EIf (contc env', contt env', e))
      | VBool false ->
          let ve, conte = eval env e in
          (ve, fun env' -> EIf (contc env', t, conte env'))
      | _ -> assert false)
  | EArrow (e1, e2) ->
      let v1, _ = eval env e1 in
      let _, cont2 = eval env e2 in
      (v1, fun env' -> cont2 env')
  | EFby (e1, e2) ->
      let v, _ = eval env e1 in
      (v, fun _ -> e2)
  | EPre e ->
      ( VUndef,
        fun env' ->
          let v, cont = eval env' e in
          EArrow (expr_of_val v, EPre (cont env')) )
  | ENone -> (VOption None, fun _ -> ENone)
  | ESome e ->
      let v, cont = eval env e in
      (VOption (Some v), fun env' -> ESome (cont env'))
  | ELam (p1, p2, b) -> (VLambda (p1, p2, b), fun _ -> ELam (p1, p2, b))
  | EApp (e1, e2) -> (
      let vf, contf = eval env e1 in
      match vf with
      | VLambda (p1, p2, b) ->
          let arg, contarg = eval env e2 in
          let env = env_bind [] p1 arg in
          let env', contb = eval_eqs env b in
          ( env_proj env' p2,
            fun env'' -> EApp (ELam (p1, p2, contb env'), contarg env'') )
      | VExtern f ->
          let arg, contarg = eval env e2 in
          (f arg, fun env' -> EApp (contf env', contarg env'))
      | _ -> assert false)
  | EExtern f -> (VExtern f, fun _ -> EExtern f)

and eval_unop op v =
  match (op, v) with
  | Not, VBool b -> VBool (Bool.not b)
  | Neg, VInt i -> VInt (-i)
  | Neg, VFloat f -> VFloat (-.f)
  | IsSome, VOption (Some _) -> VBool true
  | IsSome, VOption None -> VBool false
  | _, VUndef -> VUndef
  | _ -> assert false

and eval_binop op v1 v2 =
  match (op, v1, v2) with
  | And, VBool b1, VBool b2 -> VBool (b1 && b2)
  | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
  | Implies, VBool b1, VBool b2 -> VBool (Bool.not b1 || b2)
  | Add, VInt i1, VInt i2 -> VInt (i1 + i2)
  | Sub, VInt i1, VInt i2 -> VInt (i1 - i2)
  | Mul, VInt i1, VInt i2 -> VInt (i1 * i2)
  | Div, VInt i1, VInt i2 -> VInt (i1 / i2)
  | Add, VFloat f1, VFloat f2 -> VFloat (f1 +. f2)
  | Sub, VFloat f1, VFloat f2 -> VFloat (f1 -. f2)
  | Mul, VFloat f1, VFloat f2 -> VFloat (f1 *. f2)
  | Div, VFloat f1, VFloat f2 -> VFloat (f1 /. f2)
  | Eq, e1, e2 -> VBool (e1 = e2)
  | Neq, e1, e2 -> VBool (e1 <> e2)
  | Gt, e1, e2 -> VBool (e1 > e2)
  | Geq, e1, e2 -> VBool (e1 >= e2)
  | Lt, e1, e2 -> VBool (e1 < e2)
  | Leq, e1, e2 -> VBool (e1 <= e2)
  | _, VUndef, _ | _, _, VUndef -> VUndef
  | _ -> assert false

and env_bind env p v =
  match (p, v) with
  | PAny, _ -> env
  | PUnit, VUnit -> env
  | PVar x, _ -> (x, v) :: env
  | PTuple ps, VTuple vs ->
      assert (List.length ps = List.length vs);
      List.fold_left2 env_bind env ps vs
  | _ -> assert false

and eval_eqs env eqs =
  let env', eqs' =
    List.fold_left_map
      (fun env (p, e) ->
        let v, cont = eval env e in
        (env_bind env p v, (p, cont)))
      env eqs
  in
  (env', fun env' -> List.map (fun (p, cont) -> (p, cont env')) eqs')

let create_init_state chan_elems node_list =
  let time = Int64.minus_one in
  let channels = Hashtbl.create (List.length chan_elems) in
  let init_elems =
    List.fold_left
      (fun acc (name, elems) ->
        Hashtbl.add channels name (Queue.create ());
        List.map (fun e -> (name, e)) elems @ acc)
      [] chan_elems
  in
  let nodes = Hashtbl.create (List.length node_list) in
  let node_names =
    List.fold_left
      (fun acc node ->
        Hashtbl.add nodes node.node_name node;
        node.node_name :: acc)
      [] node_list
  in
  let exec_queue = [ (0L, node_names) ] in
  let write_queue = [ (0L, init_elems) ] in
  fun () -> { time; channels; nodes; exec_queue; write_queue }

let insert_into_timed_queue queue time elem =
  let rec aux = function
    | [] -> [ (time, [ elem ]) ]
    | (xt, xl) :: xs when xt = time -> (xt, elem :: xl) :: xs
    | (xt, xl) :: xs when xt >= time -> (time, [ elem ]) :: (xt, xl) :: xs
    | (xt, xl) :: xs -> (xt, xl) :: aux xs
  in
  aux queue

let check_release state node =
  let channels = state.channels in
  let input_ports = node.node_inputs in
  List.fold_left
    (fun acc (name, async) ->
      if async then acc && true
      else
        let channel = Hashtbl.find channels name in
        Bool.not (Queue.is_empty channel) && acc)
    true input_ports

let get_input state node =
  let channels = state.channels in
  let input_ports = node.node_inputs in
  let input_vals =
    List.map
      (fun (name, async) ->
        let channel = Hashtbl.find channels name in
        if async then VOption (Queue.take_opt channel) else Queue.take channel)
      input_ports
  in
  match input_vals with
  | [] -> VUnit
  | [ x ] -> x
  | xs -> VTuple xs

let write_output state node value =
  let write_time = state.time +: node.node_period in
  let out_ports = node.node_outputs in
  let aux ports v =
    match (ports, v) with
    | [], _ -> []
    | [ (name, async) ], v ->
        if async then
          match v with
          | VOption None -> []
          | VOption (Some v) -> [ (name, v) ]
          | _ -> assert false
        else [ (name, v) ]
    | ps, VTuple vs ->
        List.map2
          (fun (name, async) v ->
            if async then
              match v with
              | VOption None -> None
              | VOption (Some v) -> Some (name, v)
              | _ -> assert false
            else Some (name, v))
          ps vs
        |> List.filter_map Fun.id
    | _ -> assert false
  in
  let writes = aux out_ports value in
  let write_queue =
    List.fold_left
      (fun queue elem -> insert_into_timed_queue queue write_time elem)
      state.write_queue writes
  in
  state.write_queue <- write_queue

let exec_node state name =
  let node = Hashtbl.find state.nodes name in
  if check_release state node then (
    let input_val = get_input state node in
    let input_expr = expr_of_val input_val in
    let app = EApp (node.node_expr, input_expr) in
    let ret, cont = eval [] app in
    let next_e =
      match cont [] with
      | EApp (f, _) -> f
      | _ -> assert false
    in
    node.node_expr <- next_e;
    write_output state node ret)
  else ();
  let exec_queue =
    insert_into_timed_queue state.exec_queue
      (state.time +: node.node_period)
      name
  in
  state.exec_queue <- exec_queue

let update_time state = state.time <- Int64.succ state.time

let exec_writes state =
  let current_time = state.time in
  let channels = state.channels in
  let write_queue =
    match state.write_queue with
    | [] -> []
    | (xt, elems) :: xs when xt = current_time ->
        List.iter
          (fun (name, value) ->
            let channel = Hashtbl.find channels name in
            Queue.push value channel)
          elems;
        xs
    | xs -> xs
  in
  state.write_queue <- write_queue

let exec_nodes state =
  match state.exec_queue with
  | (xt, elems) :: xs when xt = state.time ->
      state.exec_queue <- xs;
      List.iter (exec_node state) elems
  | _ -> ()

let exec_ms state =
  update_time state;
  exec_writes state;
  exec_nodes state

let exec t ms =
  for _ = 1 to ms do
    exec_ms t
  done

open Sim_ast
open Fmt

let rec pp_val : value Fmt.t =
 fun ppf -> function
  | VUndef -> string ppf "nil"
  | VUnit -> string ppf "()"
  | VInt i -> int ppf i
  | VBool b -> bool ppf b
  | VFloat f -> float ppf f
  | VTuple vs -> (parens (list ~sep:comma pp_val)) ppf vs
  | VOption None -> string ppf "None"
  | VOption (Some v) -> pf ppf "Some %a" pp_val v
  | VLambda _ -> string ppf "<fun>"
  | VExtern _ -> string ppf "<ext>"

let pp =
  braces
    (record
       [
         field "time" (fun s -> s.time) int64;
         field "channels"
           (fun s -> s.channels)
           (hashtbl ~sep:semi (pair string (queue pp_val)));
         field "exec_queue"
           (fun s -> s.exec_queue)
           (list ~sep:comma
              (brackets
                 (pair ~sep:semi int64 (brackets (list ~sep:semi string)))));
         field "write_queue"
           (fun s -> s.write_queue)
           (list ~sep:semi
              (parens
                 (pair ~sep:comma int64
                    (brackets (list (parens (pair ~sep:comma string pp_val)))))));
       ])

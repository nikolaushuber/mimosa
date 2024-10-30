(* Generates the boiler-plater dune configuration for check tests *)

let usage () =
  Printf.fprintf stderr "Usage: %s [MIMOSA FILE] ..." Sys.argv.(0);
  exit 1

let rec print_rules pos =
  if pos < Array.length Sys.argv then (
    let m = Sys.argv.(pos) in
    Printf.printf
      {|(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   %s_errors
   (run mimosa check %%{dep:%s.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff %s_errors.expected %s_errors))))

|}
      m m m m;
    print_rules (pos + 1))

let () =
  let nb_args = Array.length Sys.argv in
  if nb_args < 2 then usage () else print_rules 1

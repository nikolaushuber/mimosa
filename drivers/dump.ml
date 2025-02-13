open Cmdliner
open Mimosa

let main_dump_ptree file =
  let open Reserr in
  let open Fmt in
  pp false Ptree_printer.pp stdout (Parse.f file)

let ptree =
  Cmd.(
    v
      (info "ptree" ~doc:"Dump parsetree.")
      Term.(const main_dump_ptree $ Args.file))

let main_order file =
  let open Reserr in
  let open Fmt in
  pp false Ordering.pp stdout (Parse.f file >>= Ordering.f)

let eqorder =
  Cmd.(
    v
      (info "eqorder" ~doc:"Dump parsetree after ordering.")
      Term.(const main_order $ Args.file))

let main_dump_ttree file =
  let open Reserr in
  let open Fmt in
  pp false Ttree_printer.pp stdout (Parse.f file >>= Ordering.f >>= Typecheck.f)

let ttree =
  Cmd.(
    v
      (info "ttree" ~doc:"Dump AST after type checking.")
      Term.(const main_dump_ttree $ Args.file))

let fmt_of_path p =
  match p with
  | None -> Fmt.stdout
  | Some p -> open_out p |> Format.formatter_of_out_channel

let main_sim file out =
  let open Reserr in
  let open Fmt in
  let fmt = fmt_of_path out in
  pf fmt "@[<v>%a@]@." Ppxlib_ast.Pprintast.structure
    (Parse.f file
    >>= Ordering.f
    >>= Typecheck.f
    |> Reserr.unpack
    |> Comp_sim.trans_pack)

let sim =
  Cmd.(
    v
      (info "sim" ~doc:"Dump simulation code.")
      Term.(const main_sim $ Args.file $ Args.output_file))

let cmd =
  let doc = "Dump tool for debugging information." in
  let info = Cmd.info "dump" ~doc in
  let cmds = [ ptree; eqorder; ttree; sim ] in
  Cmd.group info cmds

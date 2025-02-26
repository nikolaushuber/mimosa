open Cmdliner
open Mimosa

let main_ast file =
  let open Reserr in
  let open Fmt in
  pp Ptree_printer.pp stdout (Parse.f file)

let ast =
  Cmd.(
    v
      (info "ast" ~doc:"Dump AST after parsing.")
      Term.(const main_ast $ Args.file))

let main_ordered file =
  let open Reserr in
  let open Fmt in
  pp Ordering.pp stdout (Parse.f file >>= Ordering.f)

let ordered =
  Cmd.(
    v
      (info "ordered" ~doc:"Dump AST after ordering.")
      Term.(const main_ordered $ Args.file))

let main_typed file =
  let open Reserr in
  let open Fmt in
  pp Ttree_printer.pp stdout (Parse.f file >>= Ordering.f >>= Typecheck.f)

let typed =
  Cmd.(
    v
      (info "typed" ~doc:"Dump AST after type checking.")
      Term.(const main_typed $ Args.file))

let cmd =
  let doc = "Dump tool for debugging information." in
  let info = Cmd.info "dump" ~doc in
  let cmds = [ ast; ordered; typed ] in
  Cmd.group info cmds

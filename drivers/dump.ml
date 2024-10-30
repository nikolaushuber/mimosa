open Cmdliner
open Mimosa

let main_dump_ptree files =
  let open Reserr in
  let open Fmt in
  pp false (list Ptree_printer.pp) stdout (map Parse.f files)

let ptree =
  Cmd.(
    v
      (info "ptree" ~doc:"Dump the parsetree.")
      Term.(const main_dump_ptree $ Args.files))

let main_pack_dep files =
  let open Reserr in
  let open Fmt in
  pp false (list string) stdout
    (let* ptrees = map Parse.f files in
     let* ordered = Dependency.f ptrees in
     List.map (fun p -> p.Ptree.ppack_name.txt) ordered |> ok)

let packdep =
  Cmd.(
    v
      (info "packdep" ~doc:"Dump package order after depedency resolution.")
      Term.(const main_pack_dep $ Args.files))

let cmd =
  let doc = "Dump tool for debugging information." in
  let info = Cmd.info "dump" ~doc in
  let cmds = [ ptree; packdep ] in
  Cmd.group info cmds

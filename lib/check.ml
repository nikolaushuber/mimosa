open Cmdliner

let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES")

let main files =
  let open Reserr in
  pp false
    (fun _ _ -> ())
    Fmt.stdout
    (let* ptrees = map Parse.f files in
     map Check_parsetree.check ptrees)

let term = Term.(const main $ files)
let info = Cmd.info "check" ~doc:"Check a given set of files."
let cmd = Cmd.v info term

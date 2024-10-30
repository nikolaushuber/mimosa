open Cmdliner
open Mimosa

let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES")

let main files =
  let open Reserr in
  pp false
    (fun _ _ -> ())
    Fmt.stdout
    (let* ptrees = map Parse.f files in
     let* _ = map Check_parsetree.check ptrees in
     map Eq_ordering.f ptrees)

let term = Term.(const main $ files)
let info = Cmd.info "check" ~doc:"Check a given set of files."
let cmd = Cmd.v info term

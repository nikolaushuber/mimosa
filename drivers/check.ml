open Cmdliner
open Mimosa

let main file =
  let open Reserr in
  pp false
    (fun _ _ -> ())
    Fmt.stdout
    (Parse.f file >>= Ordering.f >>= Typecheck.f)

let term = Term.(const main $ Args.file)
let info = Cmd.info "check" ~doc:"Check a given set of files."
let cmd = Cmd.v info term

open Cmdliner
open Mimosa

let main file =
  let open Reserr in
  pp
    (fun _ _ -> ())
    Fmt.stdout
    (Parse.f file >>= Ordering.f >>= Initialisation.f >>= Typecheck.f)

let term = Term.(const main $ Args.file)
let info = Cmd.info "check" ~doc:"Check a given file for syntax and type correctness."
let cmd = Cmd.v info term

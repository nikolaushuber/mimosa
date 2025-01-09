open Cmdliner
open Mimosa

let main files =
  let open Reserr in
  pp false
    (fun _ _ -> ())
    Fmt.stdout
    (map Parse.f files
    >>= Global_odering.f
    >>= Eq_ordering.f
    >>= Local_ordering.f
    >>= Typecheck.f)

let term = Term.(const main $ Args.files)
let info = Cmd.info "check" ~doc:"Check a given set of files."
let cmd = Cmd.v info term

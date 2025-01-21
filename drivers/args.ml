open Cmdliner

let file = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")

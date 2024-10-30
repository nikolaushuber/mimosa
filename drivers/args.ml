open Cmdliner

let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES")

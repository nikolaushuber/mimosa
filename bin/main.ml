open Cmdliner
open Drivers

let () =
  let doc = "The mimosa tool" in
  let info = Cmd.info "mimosa" ~doc in
  let cmds = [ Check.cmd; Dump.cmd; Sim.cmd ] in
  let group = Cmd.group info cmds in
  Cmd.eval group |> exit

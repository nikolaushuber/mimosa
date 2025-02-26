open Cmdliner
open Drivers

let () =
  let doc = "Run mimosa." in
  let info = Cmd.info "mimosa" ~doc in
  let cmds = [ Check.cmd; Dump.cmd; Sim.cmd ] in
  let group = Cmd.group info cmds in
  Cmd.eval group |> exit

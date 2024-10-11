open Cmdliner

let () =
  let doc = "Run mimosa." in
  let info = Cmd.info "ortac" ~doc in
  let cmds = [ Mimosa.Check.cmd ] in
  let group = Cmd.group info cmds in
  Cmd.eval group |> exit

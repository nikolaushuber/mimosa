open Pid
open Mimosa.Sim

let curr_level = ref 0.0
let cmd_queue = Queue.create ()
let target = try float_of_string Sys.argv.(1) with _ -> 4.5
let _ = Queue.push target cmd_queue

module E : Extern = struct
  let recv_cmd () = Queue.take_opt cmd_queue
  let actuate l = curr_level := !curr_level +. (0.1 *. l)
  let sense () = !curr_level
end

module Simulation = Simulation (E)
open Minttea

type state = { sim : Mimosa.Sim_ast.state }

let tref = Riot.Ref.make ()
let init _ = Command.Set_timer (tref, 0.1)
let initial_model = { sim = Simulation.init () }

let update evt model =
  let open Event in
  match evt with
  | Event.KeyDown Escape -> (model, Command.Quit)
  | Event.Timer _ ->
      exec model.sim 100;
      (model, Command.Set_timer (tref, 0.1))
  | _ -> (model, Command.Noop)

let view _ =
  let open Fmt in
  str "Level: %.3f\nGoal: %.3f\n%s" !curr_level target
    "Press ESC to end simulation"

let _ =
  let app = Minttea.app ~init ~update ~view () in
  Minttea.start ~initial_model app

open Pid
open Mimosa.Sim

let curr_level = ref 0.0
let curr_cmd = Queue.create ()

module E : Extern = struct
  let recv_cmd () = Queue.take_opt curr_cmd
  let set_level l = curr_level := !curr_level +. (0.1 *. l)
  let get_level () = !curr_level
end

module Simulation = Simulation (E)
open Minttea
open Leaves

type state = {
  sim : Mimosa.Sim_ast.state;
  curr_goal : float;
  text : Text_input.t;
}

let tref = Riot.Ref.make ()
let init _ = Command.Set_timer (tref, 0.1)

let initial_model =
  {
    sim = Simulation.init ();
    curr_goal = 0.0;
    text = Text_input.make "" ~prompt:"Next goal: " ();
  }

let update evt model =
  let open Event in
  match evt with
  | Event.KeyDown Escape -> (model, Command.Quit)
  | Event.Timer _ ->
      exec model.sim 100;
      (model, Command.Set_timer (tref, 0.1))
  | Event.KeyDown Enter ->
      let opt_goal = float_of_string_opt (Text_input.current_text model.text) in
      let model' =
        match opt_goal with
        | Some goal ->
            Queue.push goal curr_cmd;
            { model with curr_goal = goal }
        | None -> model
      in
      let text = Text_input.set_text "" model.text in
      ({ model' with text }, Command.Noop)
  | Event.KeyDown _ ->
      let text = Text_input.update model.text evt in
      ({ model with text }, Command.Noop)
  | _ -> (model, Command.Noop)

let view m =
  let open Fmt in
  str "Level: %.3f\nGoal: %.3f\n%s\n" !curr_level m.curr_goal
    (Text_input.view m.text)

let _ =
  let app = Minttea.app ~init ~update ~view () in
  Minttea.start ~initial_model app

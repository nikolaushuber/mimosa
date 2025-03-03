open Example
open Mimosa.Sim

module E : Extern = struct
  let random_bool = Random.bool
  let print_bool b = print_string (if b then "t " else "f ")
end

module Simulation = Simulation (E)

let sim_time_ms = try Sys.argv.(1) |> int_of_string with _ -> 1000

let _ =
  Random.self_init ();
  let sim = Simulation.init () in
  exec sim sim_time_ms;
  print_newline ()

open Example
open Mimosa.Sim

module E : Extern = struct
  let random_bool = Random.bool
  let print_bool b = print_string (if b then "t " else "f ")
end

module Simulation = Simulation (E)

let _ =
  Random.self_init ();
  let sim = Simulation.init () in
  exec sim 400;
  print_newline ()

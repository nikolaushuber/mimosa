open Fib2
open Mimosa.Sim

module E : Extern = struct
  let print_int x =
    print_int x;
    print_newline ()
end

module Simulation = Simulation (E)

let sim_time_ms = try Sys.argv.(1) |> int_of_string with _ -> 1000

let _ =
  let sim = Simulation.init () in
  exec sim sim_time_ms;
  print_newline ()

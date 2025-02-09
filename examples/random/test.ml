open Random

let current_int = ref 0
let output = ref []

module E : Random.Extern = struct
  let random_int () =
    let x = !current_int in
    incr current_int;
    x

  let print_int i = output := i :: !output
end

module Simulation = Simulation (E)
open Mimosa.Sim

let%test _ =
  let sim = Simulation.init () in
  let _' = exec sim 10000 in
  List.mapi (fun i o -> 2 * i = o) (List.rev !output)
  |> List.fold_left ( && ) true

open Edge_detector
open Mimosa.Sim

let input_signal = [ true; false; false; true; true; true; false ]

let rec get_edges = function
  | true :: false :: xs -> false :: get_edges (false :: xs)
  | false :: true :: xs -> true :: get_edges (true :: xs)
  | [ true ] -> [ false ]
  | _ :: xs -> get_edges xs
  | [] -> []

let signal_queue = Queue.create ()
let _ = List.iter (fun x -> Queue.push x signal_queue) input_signal
let output = ref []

module E : Extern = struct
  let poll () = try Queue.pop signal_queue with _ -> false
  let print_bool b = output := b :: !output
end

let sim_time = 100 * (List.length input_signal + 2)

module Simulation = Simulation (E)

let _ =
  let sim = Simulation.init () in
  exec sim sim_time;
  let expected_output = get_edges input_signal in
  assert (expected_output = List.rev !output)

open Controller
open Mimosa.Sim

let led_on = ref false
let level = ref false

module E : Extern = struct
  let pin_level () = !level
  let set_led = ( := ) led_on
end

module Simulation = Simulation (E)

let _ =
  let sim = Simulation.init () in
  exec_ms sim;
  assert (Bool.not !led_on);
  level := true;
  exec sim 200;
  level := false;
  assert !led_on;
  exec sim 200;
  assert (Bool.not !led_on)

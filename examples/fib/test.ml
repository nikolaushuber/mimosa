open Fib
open Mimosa.Sim

let output = ref []

module E : Extern = struct
  let print_int x = output := x :: !output
end

module Simulation = Simulation (E)

let fib n =
  if n < 2 then n
  else
    let fib_prev = ref 1 and fib_curr = ref 1 in
    for _ = 2 to n - 1 do
      let temp = !fib_curr in
      fib_curr := !fib_curr + !fib_prev;
      fib_prev := temp
    done;
    !fib_curr

let%test _ =
  let sim = Simulation.init () in
  exec sim 1000;
  List.mapi (fun i res -> res = fib i) (List.rev !output)
  |> List.fold_left ( && ) true

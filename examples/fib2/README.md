# Fibonacci

This example is an adaptation of the `fib` example in this repository. Instead of feeding both numbers to the addition through channels, the last Fibonacci number is remembered inside the **add** node. This example illustrates how to use the `pre` and `->` operators.

## Simulation harness

The **print** node is implemented to print to stdout.

## Running the example

The example can be run in the following way:

```bash
dune exec -- fib2_sim.exe 400
```

The argument `400` is the amount of milliseconds that shall be simulated.

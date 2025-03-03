# PID controller

This example showcases a simple PID controller implemented in Mimosa.

## Simulation harness

The simulation uses sleep instructions to run roughly in real-time. It always prints the current level and the goal to the screen. The goal can be changed in the `pid_sim.ml` file.

You can also play around with the PID control coefficients inside the **pid** step in `pid.mim`!

## Running the example

The example can be run in the following way:

```bash
dune exec -- pid_sim.exe
```

To exit the simulation, press the ESC key.

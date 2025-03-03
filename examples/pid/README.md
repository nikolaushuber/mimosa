# PID controller

This example showcases a simple PID controller implemented in Mimosa.

## Simulation harness

The simulation uses sleep instructions to run roughly in real-time. It always prints the current level and the goal to the screen.

You can play around with the control coefficients inside the **pid** step in `pid.mim`!

## Running the example

The example can be run in the following way:

```bash
dune exec -- pid_sim.exe 4.5
```

The argument `4.5` is the target that shall be reached by the controller.

To exit the simulation, press the ESC key.

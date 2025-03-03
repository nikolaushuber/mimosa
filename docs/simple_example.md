# Guided tour through Mimosa

This guide walks through a very simple example of a Mimosa program, and shows how to compile and simulate it.

## Example program

![Simple example](images/random_bool.png#only-light)
![Simple example](images/random_bool_light.png#only-dark)

As a first example, we will implement the program above. It consists of three *nodes*, which are called **rand**, **invert**, and **print**.

Node **rand** creates random Booleans, node **invert** inverts them, and node **print** will print them. The nodes communicate through the channels **a** and **b**, which are both FIFO buffers holding Boolean values.

Each node will try to execute periodically, in our example all nodes have the same period of *50ms*. If a node does not have input available (i.e., the channel it reads from is empty), then it will stay idle until its next release.

Nodes are similar to *threads* or *tasks* in (real-time) operating systems. Each node implements a *step*, which is similar to a function in other programming languages. We will start by defining the steps for our example program.

First, we define two *step prototypes*, i.e., steps for which we only define the name and type signature, but will leave the implementation to be defined externally.

```text
step random_bool () --> (_ : bool)
step print_bool (_ : bool) --> ()
```

A step definition starts with the `step` keyword, followed by the name of the step (which must start with a lower-case letter), and then the signature, which is comprised of an input and output [pattern](steps.md#patterns) separated by `-->`.

We could have named the formal parameters inside the input and ouput definitions (e.g., `() --> (out : bool)`), however, for step prototypes we are only interested in their type signature, therefore, we do not need to invent names for the parameters (and can instead use `_` as the name).

The only step we will implement fully in Mimosa is the **invert** step:

```text
step invert (in : bool) --> (out : bool)
{
    out = !in;
}
```

Here, the step signature is followed by a set of equations (or in this case just one equation), which defines the output `out` as the inverted input `in`. For more information on the available operators please refer to the [language definition](steps.md#expressions).

Next, we can define the two channels:

```text
channel a : bool
channel b : bool
```

Each channel is given a name and a type. Optionally, a channel can also be followed by a list of *initial values* that shall be present inside the channel when the program starts executing. For more information on channel definitions please have a look at the [language definition](channels.md) again.

Finally, we can define the nodes:

```text
node rand implements random_bool () --> (a) every 50ms
node invert implements invert (a) --> (b) every 50ms
node print implements print_bool (b) --> () every 50ms
```

Each node has a name, a step that it implements, an *interface definition*, and a *period*. Node names are in a different namespace than steps, therefore a node can have the same name as a step (like here for `invert`). The interface refers to the channels we defined before. For each node, the interface type must be compatible with the type of the step it implements.

!!! note
    Each defined channel needs to be connected once to a node input and once to a node output. Unconnected channels are flagged by the compiler.

The full program therefore looks like this:

```text
step random_bool () --> (_ : bool)
step print_bool (_ : bool) --> ()

step invert (in : bool) --> (out : bool)
{
    out = !in;
}

channel a : bool
channel b : bool

node rand implements random_bool () --> (a) every 50ms
node invert implements invert (a) --> (b) every 50ms
node print implements print_bool (b) --> () every 50ms
```

!!! note
    The order of top-level definitions is not relevant, the compiler will order the items automatically according to their dependencies. This also means, that no two definitions of the same class (i.e., steps, nodes, or channels) can have the same name.

We can check if the program is syntactically correct, and if it type checks, by using the `mimosa check` command. If we save the program above into a file `example.mim` we can then run

```text
mimosa check example.mim
```

If everyone is OK, the above command will just return. You can try what happens if you change the type of one of the channels from `bool` to `int`!

## Optional inputs

In the example above, all nodes were running with the same period, which makes sense, since they have to wait for data to be available in their respective input buffer. However, sometimes a node may run, even if there is no available input data (for example, a control algorithm may need to compute inputs to an actuator even if there is no new command from a human operator).

To simulate this behaviour, let's assume that the `print` node in the example above shall run at a higher frequency (e.g., with a period of *10ms*), and in case there is no new input, it shall just print `false`.

For this, we will first define a new step:

```text
step print_opt_bool (in : bool?) --> ()
{
    _ = print_bool (either in or false);
}
```

Here, the input type switched from `bool` to `bool?`, which denotes an *optional*  Boolean type. In the definition of the step we only have one equation (and since we do not care about the return value of `print_bool` we can use the `_` pattern on the left-hand side again). The definition of the equation uses the `print_bool` step we have defined as a prototype before.

The `either ... or ...` expression lets us unpack a value of an optional type, so `either in or false` means, that if `in` is of the form `Some v`, then `v` is returned, or if `in` is `None`, it returns `false` instead (you can think of the expression after `or` as defining a default value in case the expression after `either` is `None`).

For more information on optional values, please have a look at the [language definition](steps.md#optionals) again.

With that, we can change the **print** node accordingly:

```text
node print implements print_opt_bool (b?) --> () every 10ms
```

An input port can be marked with `?` to declare it optional. Whenever the respective node tries to execute, it will look into the input channel, and if there is a value `v` inside it wrap it inside an optional value (i.e., `Some v`) before executing the implemented step with the wrapped input. Analogously, if the channel is empty, the step function will be executed with `None` as input. Mind the difference in types between the channel `b` (i.e., `bool`) and the input of the step `print_opt_bool` (i.e., `bool?`).

## Simulation

Mimosa programs can be simulated through a deep embedding into OCaml. The `mimosa sim` command can compile a given Mimosa program into this embedding. If we run

```text
mimosa sim example.mim
```

We get the following (slightly simplified) output:

```ocaml
open Mimosa.Sim_ast

module type Extern = sig
    val random_bool : unit -> bool
    val print_bool : bool -> unit
end

module Simulation (E : Extern) = struct ... end
```

First, a module type `Extern` is defined, which describes the signatures of the OCaml functions we need to implement for the step prototypes. `Simulation` is a functor (a function that takes a module as input and returns a new module), which given a module of type `Extern` defines a module which can then run the simulation.

This means, that the code produced by `mimosa sim` can be used both for defining unit tests, as well as for interactive simulation, depending on how the `Extern` module is implemented.

For a first try, we can implement the module in the following way:

```ocaml
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
```

This implements the external functions through functions from the OCaml standard library. We can then instantiate the `Simulation` functor with these functions, and finally create a simulation run `sim` by calling `Simulation.init ()` and executing it for *400ms*. When the above simulation is run, 6 random Booleans should be printed to the terminal (it takes *100ms*  for the first value to reach the print node).

This example is also implemented in the project repository under `/examples/minimal`. For details on how to run the above code, please refer to that  directory (in particular to the README).

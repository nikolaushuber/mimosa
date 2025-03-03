# Nodes

A node is an instantiation of a step as a process, which communicates through channels with other nodes. It defines its *interface* by referring to the names of channels, and the interface must be compatible with the type signature of the step it implements. It also defines the *node period*, which marks the periodical release pattern of the node.

```text
node my_node implements my_step (a, b) --> (c) every 100ms
```

This defines the node `my_node`, which implements the step `my_step`. The input and output patterns refer to the channels of a Mimosa program, from which inputs are read and to which outputs will be written, respectively. The period defines, when a node will try to execute. For example, the node `my_node` will try to execute at `0ms, 100ms, 200ms, ...`.

If data is available inside all input channels, then the node can execute. In case at least one of the required inputs is not available, the node stays idle until its next release time.

Inputs and outputs may be defined as optional (by marking the respective channel with `?`):

```text
node print_opt implements print_opt_int (a?) --> () every 100ms
node receive_opt implements maybe_receive_int () --> (b?) every 100ms
```

If an input is marked as optional, the connected channel is inspected, and in case a value is present, it is wrapped with the `Some` constructor (see [language definition](steps.md#optionals)) before it is handed to the implemented step, in case there is no value in the channel, `None` is used instead.

Analogously, if an output is defined as optional, the respective return value of the step must be of optional type, and in case it is of the form `Some v`, the value `v` is written to the respective output channel, if it is `None`, no value is written out.

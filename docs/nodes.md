## Nodes

A node is an instantiation of a step as a process, which communicates through
channels with other nodes. It defines its *interface* by referring to the
names of channels, and the interface must be compatible with the type signature
of the step it implements. It also defined the *node period*, which marks the
periodical release pattern of the node.

```text
node <name> implements <step> <input pattern> --> <output pattern> every <period>
```

This defines a node with the given name, which implements a particular step.
The input and output pattern refer to the channels of a Mimosa program, from
which inputs my be read and to which output will be written to.
The input and output pattern must be compatible with the step being implemented.
The period defines, when a node will try to execute. For example, a period of
`100ms` will make the node try to execute at `0ms, 100ms, 200ms, ...`.

If data is available inside all input channels, then the node can execute. In case
at least one of the required inputs is not available, the node stays idle until
its next release time.

Inputs and outputs may be defined as optional. If an input is marked as optional,
the connected channel is inspected, and in case a value is present, it is wrapped
with the `Some` constructor ([see](steps.md#optionals)) before it is handed to
the implemented step, in case there is no value in the channel, `None` is used.

Analogously, if an output is defined as optional, the respective return value of
the step must be of optional type, and in case it is of the form `Some v`, the
value `v` is written to the respective output channel, if it is `None`, no value
is written out.

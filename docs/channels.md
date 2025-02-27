# Channels

A channel represents a FIFO buffer, and is defined by the keyword `channel`
followed by the name, the type of values it stores, and possibly a the set of
initial values which shall already be present in the channel at startup of the
program:

```text
channel x : int
channel y : bool = { 1; 2 }
```

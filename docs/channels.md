# Channels

A channel represents a FIFO buffer, and is defined by the keyword `channel`
followed by the name, the type of values it stores, and possibly a set of
initial values which shall already be present in the channel at startup of the
program:

```text
channel x : bool
channel y : int = { 1; 2 }
```

Initial values are added to the channel left-to-right, so a node reading from channel y will first read `1`, and then `2`. 

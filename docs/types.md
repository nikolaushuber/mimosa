# Types

Mimosa implements a Hindley-Milner-style type system, which supports type inference, so that
type annotations are, for the most part, not necessary. They can of course still be given, as
shown in some of the examples below.

Mimosa has 4 inbuilt basic types: `unit`, `bool`, `int`, `float`. In addition to these basic types,
it has two higher-order type constructors, one for tuples, and one for optionals.

A tuple type is defined with the `*` type operator: `bool * int`, `bool * (float * int)`, ...
Optional types are defined with the postfix `?` type operator: `int?`, `(bool * int)?, ...

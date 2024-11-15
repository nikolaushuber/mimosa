# MIMOSA

## Types

Mimosa has 4 inbuilt base types: `unit`, `bool`, `int`, `real`.
The numerical precision of both `int` and `real` is application dependent.

In addition to the base types, Mimosa offers two higher-order type constructors,
one for defining tuples, and one for defining optional values. Tuple types are
defined with the `*` operator: `bool * int`, while optionals are defined with
the `?` operator: `int?`.

## Expressions

An expression is formed inductively through the following constructions:

- Constants: `5`, `true`, `4.2`
- Variables: `x`, `y`
- Unary operations: `~true`, `-5`
- Binary operations: `5 + 3`, `true && false`
- Either: `either x or y` (which evaluates to `a` if `x == Some a`, or `y` 
otherwise)
- Tuple construction: `1, true, 3.4`
- Conditional: `if true then ... else ...`
- Step application: `add (4, 5)`, `abs (-5)`
- Initialization: `0 -> x`
- Followed-by: `0 fby x`
- Previous-value: `pre x`
- None: `None`
- Some: `Some x`

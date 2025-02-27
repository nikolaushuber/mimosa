# Steps

Steps are the Mimosa equivalent to functions in other languages, they are elementary
units of computation (specificially, they do not have any notion of time):

```text
step add (a, b) --> c
{
    c = a + b;
}
```

A step definition starts with the keyword *step*, followed by the name of the step
(which must start with a lowercase letter), followed by the inputs and outputs separated
by the `-->` keyword.

Following the signature is the body of the step definition, which is a collection
of equations. Intermediate variables may be defined:

```text
...
    tmp = a + b;
    c = tmp;
...
```

and the order of equations is not important, so it could equivalently be written as

```text
...
    c = tmp;
    tmp = a + b;
...
```

The compiler will automatically order the equations according to their dependencies,
cyclic dependencies lead to a compile-time error.

In the example above, we did not use any type annotations. If prefered, they can be
added explicitly:

```text
step add (a : int, b : int) --> (c : int)
{
    c = a + b;
}
```



### Patterns

Patterns appear at multiple places in a Mimosa program. They define the input and
output of each step, and are also used as the left-hand side of equations.

The basic pattern is a just a *variable pattern*, to which a sequence of values can be bound.
Nested patterns are provided by *tuple patterns*.

Examples:

```text
x = 1;
z, y = 2, 3;
...
```

## Expressions

### Constants

There are four different families of constants available in Mimosa:

- Unit: `()`
- Integer: `1`, `-3`, ...
- Float: `1.4`, `0.6`, ...
- Boolean: `true`, `false`

### Variables

Expression can refer to other named expression by just using the name:

```text
x = ...;
y = x;
```

### Tuples

Tuples are build inductively by using `,` on subexpressions:

```text
1, x, 5.0
```

### Arithematic, logic, and comparison operators

Mimosa offers the standard set of arithmetic and logic operators, which are commong in other programming languages as well. Similar to OCaml, certain operators exist in two flavours, one for integers, and one for floats:

- `+`, `-`, `*`, `/`: integer arithmetic
- `+.`, `-.`, `*.`, `/.`: floating point arithmetic

- `&&`: and
- `||`: or
- `=>`: implies
- `!`: not

- `<`, `<=`, `>=`, `>`: integer comparison
- `<.`, `<=.`, `>=.`, `>.`: floating point comparison

- `==`, `!=`: polymorphic comparison

The following table lists the precedence level of all operators classes from the lowest to the highest precedence.

| Operator | Associativity |
| --- | --- |
| `=>` | left |
| `||` | left |
| `&&` | left |
| `<` `<=` `>=` `>` `<.` `<=.` `>=.` `>.` `==` `!=` | left |
| `!` | right |
| `+` `-` `+.` `-.` | left |
| `*` `/` `*.` `/.` | left |
| `?` `-` `-.` (unary negate) | right |

### Memory operators

As each expression semantically representes a (possible inifinite) sequence of
values, there are special operators which allow to shift these sequences,
thereby allowing to define memory.

The `pre` operator allows to refer to the previous values of a sequence. This
introduces an undefined value at the first cycle, which can be removed by using
the initialization operator `->` operator:

```text
x = 0 -> pre y
```

Similarly to `->` the `fby` operator also allows to prepend a sequence by an
initial element, however, it does not evaluate the second argument.

Assuming the variable `x` represents the sequence `1, 2, 3, 4, ...`, then 
`0 -> x` representes `0, 2, 3, 4, ...` while `0 fby x` represents `0, 1, 2, 3, 4, ...`.

Both `->` and `fby` are right associative, and have a lower precedence than any of the arithmetic, logic, and comparison operators. `pre` is right associative and has the highest precedence of any operator in Mimosa.

### Step Application

A step can be used inside another step through a step application:

```text
step add (a, b) --> c
{
    c = a + b;
}

step add2 x --> y
{
    y = add (x, 2);
}
```

The order of step definitions in a file is irrelevant, the compiler will order
them according to their dependencies. Steps cannot call themselves recursively,
and cyclic dependencies between steps are flagged as an error by the compiler.

### Conditional

The `if ... then ... else ...` construct can be used to express conditional execution.
Different to Lustre, in Mimosa only one branch is evaluated in each cycle.

### Optionals

Optional expressions can be formed through the type constructors `None` and `Some`: 

```text
x = None;
y = Some 2;
```

Optionals can be deconstructed through an `either ... or ...` expression.
This expression tries to evaluate the first sub-expression to `Some v` and then
return `v`, otherwise (in case the first expression is `None`), evaluate the
second expression.

So, for example, `either (Some 2) else 3` evaluates to `2`, while `either None else 3`
evaluates to `3`.

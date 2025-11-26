# Proposals

This directory contains potential directions that the language could go.
The syntax and ideas here are NOT part of the langauge.
They provide an area to iterate on syntax and features without commiting to implementation.
DO NOT reference these examples to understand that official language.

## Ideas

### Traits

Traits are super useful in langauges like `Go` and `Rust`.
They allow code to be structurally typed which is often more expressive than using concreet types.
One area of weakness in the language currently is how returns are handled.
Because varriants need to be explicitly constructed in Manta there's more boilerplate than in languages like `Rust`.
I wonder if traits could be used to reduce this boilerplate and make it easier to bubble errors up the stack?

### Zero Values

One weakness of zero values is that it makes `nil` values difficult to avoid.
For example, what is the default value of the following type expression `var a *i32`?
In a language without `nil` an i32 must be allocated and then pointed to.
This might be fine for something like an int, but could be a lot of wasted cycles for a large type.
For example `var a *[1024]Person` could allocate a lot of memory, only for that memory to be thrown away.

One option is to use controll flow analisys to attempt to prevent unnecessary allocations where possible.
Another option is to forgo default values entierly and just insist that values be inialized before use.
`Go` avoides this by allowing a to simply be `nil`.
`Rust` avoides this by requiring the developer to provide a value value for a before it can be used.
I personally perfer the `Rust` approach.
However, controll flow analisys like this has important implications for the spatial coherence of code as well as compile times.
Also, the `Rust` approach is much more complex to implement.

### Go Style Switch Staements

In `Go` the switch statment can be used like a large `if/else` block.
Each `case` becomes it's own boolean expression.
I like this construction a lot and use it often it go.
`Manta` uses the `match` keyword so that would need to be changed.
There are several changes that would need to be made to the `.ebnf` file as well as the language spec and AST spec.
I'm gonna put that work off for now while I work on finishing the AST spec.
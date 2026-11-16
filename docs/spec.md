# Manta Language Specification

A comprehensive guide to the Manta programming language.

## Design Goals

- Simplicity: Small language with explicit, easy-to-understand constructs.
- Readability: Code is ready much more than often than it's written. Readability therfor takes precident.
- Control: Manual memory management with no hidden allocations or GC pauses.
- Familiarity: Syntax that resonates with systems programmers from Rust, C, Zig, and Go.
- Clarity: Explicit constructs and minimal magic.

## Declarations and Bindings

- `mod`: module declaration
- `use`: import external modules
- `type`: type declaration
- `struct`: struct type declaration
- `enum`: enum type declaration
- `fn`: function declaration
- `mut`: variable bindings (immutable `let` to be added later)

## Control Flow

- `return`: return from a function
- `if`: conditionaly execute code
- `loop`: siple infinite loop
- `break`: break out of a loop
- `continue`: start the next iteration of a loop

## Literals

- `true`: a boolean with the `true` value
- `false`: a boolean with the `false` value
- `nil`: a `0` value with the `ptr` type

## Basic Types

- `i64`/`u64`: signed and unsigned integer types
- `u8`: byte type
- `f64`: float type
- `bool`: boolean type
- `str`: string type
- `ptr`: untyped pointer type

## Composite Types

- `*T`: pointer to type `T`
- `[N]T`: fixed-size array of `N` elements with type `T`
- `@T`: Meta type of type `T`

## Builtins

- `alloc`: allocate new memory
- `free`: free allocated memory

## Prefix Operators

- `-`: negate a numeric value
- `!`: negate a boolean value
- `&`: get the address of a value
- `*`: dereference a pointer value

## Infix Operators

- `(`: function calls are treated as and infix operator with a required closing token `)`.
- `[`: indexing is treated as an infix operator with a required closing token `]`.
- `*`: integer and floating point multiplication.
- `/`: integer and floating point division.
- `%`: integer modulo.
- `+`: integer and floating point addition.
- `-`: integer and floating point subtraction.
- `<`: integer and floating point less-than operator.
- `>`: integer and floating point greater-than operator.
- `<=`: integer and floating point less-than-or-equal operator.
- `>=`: integer and floating point greater-than-or-equal operator.
- `==`: equality check.
- `!=`: inequality check.
- `&&`: logical and operator.
- `||`: logical or operator.
- `&`: integer bitwise AND operator.
- `|`: integer bitwise OR operator.
- `^`: integer bitwise XOR operator.

## Syntax

Below are examples of Manta syntax.
Comments (using `//`) are provided for clarity.

### module declaration

```
mod main // must be the first declaration in every file
```

### use block

```
use (
  "fmt"
)
```

### type declarations

```
// declares a new struct type
type Vec2 struct {
  x f64
  y f64
}

// declares a new enum type, must be an u64
type ExitCode enum {
  Ok = 0
  Fail = 1
}
```

### variable declaration

```
mut x i64 = 10                       // mutable value with explicit type and value
mut v Vec2 = Vec2 { x: 0.0, y: 1.0 } // mutable value with struct type
```

### module-level declaration

```
mut PI f64 = 3.14159 // mutable constant with explicit type and value
```

### function declaration

```
fn add(a i64, b i64) i64 {
  return a + b
}
```

### type casting

```
mut a i64 = 10       // this creates an int64 value
mut p1 ptr = ptr(*a) // this converts *a into a ptr type
```

## Module System

Every Manta source file must begin with a module declaration (see the syntax section above).
All programs need a `main` module with a `main` function which is the entry point of the program.
External modules can be imported using a `use` block (see the syntax section above).

### Import Graph

When compiling the code Manta builds a directed acyclic graph (DAG) of all the modules.
Import cycles are not allowed in this graph.
If modules are imported in a cycle, compilation will fail.

### Mod File

Every Manta project must include a `manta.mod` file which includes the compiler version as well as the project name.
This file will be created by the `manta init [PROJECT_NAME]` command.
At a future point this file will also track dependencies but that system is not yet designed.

### User Modules

Modules that are defined as part of the project can be referenced relative to the project root.
They must also be prefixed by the project name.
If for example the project name is `foo`, and the module to be imported is located in the `./bar` dir, then the use block would look like this:

```
use (
  "foo/bar"
)
```

When using an imported type or function the usage must be qualified:

```
fn main() i64 {
  // use the `baz` function from the `bar` module
  bar::baz("ok")
}
```

The qualified name is based on the name of the module.
The Manta convention is for this name to match the name of the directory that contains the code.
This is not required, but breaking this convention can make it difficult to track imports.

### Standard Library

Manta includes a standard library for common tools that many programs will share.
Standard library imports do not need to include the project name in the `use` block.

```
use (
  "fmt"
  "io"
)
```

### Strings

Unlike in C strings are a core type in the Manta language.
Internally strings are implemented as a struct with 2 fields.

```
type str struct {
  data *u8
  len u64
}
```

The `data` field points to the start of the string data.
The `len` field is the number of `u8` bytes in the string.
Strings in Manta are utf-8 which means that a single character may be more than a single byte.
Importantly, because this struct does not have a capacity field, strings are not mutable.
String literals are compiled directly into the data section of the complied code.
This is acceptable because the strings can not be mutated.

### Identifier

Identifiers in Manta may contain alphanumeric characters (both upper and lower case) as well as numbers and underscores.
The first character of an identifier must not be either an underscore or a number.

## Memory Management

Memory in Manta must be manually managed.
The main builtins used to manage memory are `alloc` and `free`.
`alloc` takes a meta type (see coposite types above) and returns a `ptr` type.
`free` takes a `ptr` value and marks that memory as free.
normally, pointers are typed in Manta so type casting can be used to convert a type into a `ptr`:

```
fn main() i64 {
  // allocate a new i64
  mut a0 ptr = alloc(@i64)
  
  // move from a ptr type into an actual *i64
  mut a1 *i64 = *i64(a0)
  
  // convert a1 back into a ptr value
  mut p ptr = ptr(a1)
  
  // free the allocated i64
  free(p)
}
```

## Operator Precedence

| Lvl | Name           | Tokens               | Description                      |
| --- | -------------- | -------------------- | -------------------------------- |
|   1 | Call           | `(`, `[`             | Function calls and indexing      |
|   2 | Prefix         | `-`, `!`, `&`, `*`   | Prefix Operators                 |
|   3 | Multiplicative | `*`, `/`, `%`        | Left-associative infix operators |
|   4 | Additive       | `+`, `-`             | Left-associateve infix operators |
|   5 | Comparison     | `<`, `>`, `<=`, `>=` | Non-associative infix operators  |
|   6 | Equality       | `==`, `!=`           | Non-associative infix operators  |
|   7 | Logical AND    | `&&`                 | Left-associative infix operator  |
|   8 | Logical OR     | `||`                 | Left-associative infix operator  |
|   9 | Bitwise AND    | `&`                  | Left-associative infix operator  |
|  10 | Bitwise OR     | `|`                  | Left-associative infix operator  |

## Automatic Semicolon Insertion (ASI)

Manta uses automatic semicolon insertion to insert semicolons before line breaks.
Semicolons are only inserted if the token that proceeds the new line is:
- A Manta identifier
- A Manta basic literal including an integer, a floating, a bool, or string.
- The `break` or `continue` keyword.
- One of the following tokens; `)`, `]`, `}`.

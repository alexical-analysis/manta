# Manta Language Specification

A comprehensive guide to the Manta programming language—a systems language with manual memory management, explicit error handling, and strong sum types.

## Overview

Manta is a small, explicit systems language designed for performance and clarity. It features:

- **Manual memory management**: Explicit `new` and `free` primitives, no garbage collector
- **Strong sum types**: Rust-style enums for type-safe error handling and optional values
- **Deferred cleanup**: Lexically-scoped `defer` blocks for resource management
- **Explicit error handling**: Pattern-matching and exception-like `except` clauses for error propagation
- **Module system**: Go style code organize using `mod` declarations and `use` for imports
- **Familiar syntax**: Keywords and type names inspired by Go, Rust, C, and Zig

## Design Goals

- **Simplicity**: Small language with explicit, easy-to-understand constructs
- **Control**: Manual memory management with no hidden allocations or GC pauses
- **Familiarity**: Syntax that resonates with systems programmers from Rust, C, Zig, and Go
- **Clarity**: Explicit constructs and minimal magic; compiler implementation stays tractable

## 1. Keywords and Primitives

### Reserved Keywords

Core language constructs:
- `fn` - function declaration
- `defer` - deferred cleanup block
- `match` - pattern matching on sum types
- `let`, `mut` - variable bindings (immutable and mutable)
- `var`, `const` - module-level variable and constant declarations
- `mod` - module declaration
- `use` - import modules
- `type` - type declarations
- `struct`, `enum` - type definitions (used in type declarations and literals)

Control flow:
- `return` - return from function
- `break` - break out of the current loop
- `continue` - start the next iteration of the current loop
- `if`, `else` - conditional execution
- `for`, `loop` - loops
- `break`, `continue` - loop control

Error handling:
- `or`, `wrap` - error handling alternatives in `except` clauses
- `!` - panic in `except` clauses

### Primitive Types

Integer types:
- Signed: `i8`, `i16`, `i32`, `i64`
- Unsigned: `u8`, `u16`, `u32`, `u64`

Float types:
- `f32`, `f64`

Other types:
- `bool` - boolean (true/false)
- `str` - string type
- `meta` - meta type with runtime metadata on types

Composite type constructors (used in declarations and literals):
- `*T` - pointer/reference to type T
- `[N]T` - fixed-size array of N elements of type T
- `[]T` - slice (dynamic array) of type T
- `struct { ... }` - structure type
- `enum { ... }` - enumeration/sum type

### Variables and Type Annotations

Variable declarations:

```
let x i32              // immutable, explicit type
let x = 10             // immutable, inferred type
mut x i32 = 20         // mutable, explicit type
mut x = 30             // mutable, inferred type
```

Module-level declarations:

```
const PI = 3.14159     // constant
var counter = 0        // mutable variable
```

### Function Syntax

Basic function:

```
fn add(a, b i32) i32 {
    return a + b
}
```

Function with no return type (implicitly returns nothing):

```
fn greet(name str) {
    fmt::println("Hello world!")
}
```

Function returning multiple types via `struct` or `enum` (using sum types):

```
type DivResult enum {
    Ok(i32)
    DivisionByZero
}

fn divide(a, b i32) DivResult {
    if b == 0 {
        return .DivisionByZero
    }
    return .Ok(a / b)
}
```

This is often used to handle potential function errors.

## 2. Module System

### Module Declaration

Every Manta source file begins with a module declaration:

```
mod main
```

This declares the current file as belonging to the `main` module.

### Imports with `use`

Import modules from the standard library or other packages using `use`:

```
use (
    "os"
    "strings"
    "fmt"
)
```

This imports the `os`, `strings`, and `fmt` modules, making their exported symbols available via module-qualified names (e.g., `os::open_file`, `fmt::println`).

### Module-Qualified Names

Access exported symbols from other modules using the `::` operator:

```
fn main() {
    let f = os::open_file("/path/to/file")
    fmt::println("Opened file")
}
```

## 3. Types and Type Declarations

### Type Aliases

Declare a type alias with the `type` keyword:

```
type UserId = u64
type FilePath = str
```

### Struct Types

Define a struct with named fields:

```
type Point struct {
    x i32
    y i32
}
```

Create struct where the values inferred is inferred:

```
let p = Point { x: 10, y: 20 }
```

### Enum Types (Sum Types)

Define an enum, notice that variants that may carry data, this is called the payload:

```
type Result enum {
    Ok(str)
    Err(str)
}
```

Note: the variants can only contain a single type as the payload which differs from Rust.

Enum values can be created using dot notation:

```
fn process() Result {
    return Result.Ok("Success")
}
```

The complier will infer the variants type allowing you to omit the type:

```
fn process() Result {
    return .Ok("Success")
}
```

Note: type inference will not propagate and will only work if the statement can be inferred directly

Match on enum values:

```
let res = process()
match res {
    .Ok(msg) { /* handle success */ }
    .Err(msg) { /* handle error */ }
}
```

## 4. Control Flow and Statements

### Defer

Run cleanup code at the end of the current scope using `defer`:

```
fn copy_file(src str, dst str) {
    let f = os::open_file(src)
    defer { os::close_file(f) }
    
    // ... use f
}
```

Semantics:
- Deferred blocks execute in LIFO order when exiting the current scope (normal return, early return, or error unwinding)
- Variables captured by deferred blocks are captured by reference at defer-execution time
- Multiple defers in a scope form a stack and execute in reverse order

### Return

Return from a function:

```
fn get_value() i32 {
    return 42
}
```

Return with no value (for functions with no return type):

```
fn process() {
    return
}
```

### Match Statements

Match on sum types for exhaustive pattern matching:

```
match value {
    .Some(v) { /* handle v */ }
    .None { /* handle none */ }
    _ { /* catch-all */ }
}
```

Pattern options:
- Enum variant patterns: `.Variant`, `.Variant(binding)`, `ModuleName.Variant(binding)`
- Literal patterns: numbers, strings, booleans
- Variable binding: bare identifier binds the matched value
- Wildcard: `_` matches anything
- Named Wildcard: Any valid identifier. Works like `_` but binds the value to the identifier

Match is not an expression and can not yield a value:

## 5. Error Handling with `except`

Error handling in Manta uses sum types (`enums`) and the `except` clause to provide explicit, type-safe error propagation.

### Except Clause Syntax

Bind a value from an expression

```
let pattern = expression except?
```

Where `except` is one of three forms:

#### 1. Panic Form: `!`

Unwrap the value or panic. Use when you're confident the value exists:

```
let .Ok(content) = read_file(path)!
```

If the result is not `.Ok`, the program panics.

#### 2. Handler Form: `or(...) block`

Handle the error with a block of code. Optionally bind the error:

```
let .Ok(content) = read_file(path) or(err) {
    print("Error: ", err)
    return
}
```

The binding is optional:

```
let .Ok(value) = compute() or {
    return default_error
}
```

#### 3 Wrapper Form: `wrap [TypeSpec].Variant`

Unwrap the value or wrap the error in another variant and return:

```
let .Ok(content) = read_file(path) wrap .ReadError
```

If the result is not `.Ok`, the error is wrapped in the `.ReadError` variant and automatically returned from the function.

### Common Error Type Convention

By convention, Manta programs define error types with an `Ok` variant for success:

```
type FileError enum {
    Ok(str)
    OpenFailed(str)
    ReadFailed(str)
}

fn read_file(path str) FileError {
    let .Ok(f) = os::open_file(path) wrap .OpenFailed
    defer { os::close_file(f) }
    
    let .Ok(content) = os::read_to_string(f) wrap .ReadFailed
    return .Ok(content)
}
```

### Using Except with Defer

Deferred blocks execute before an error is propagated:

```
fn process(path str) FileError {
    let .Ok(f) = os::open_file(path)!
    defer { os::close_file(f) }
    
    let .Ok(data) = os::read(f) or(e) {
        // defer will run here before handler executes
        return .ReadError
    }
    
    return .Ok(data)
}
```

### Examples

Basic error handling:

```
fn read_or_default(path str) str {
    let .Ok(content) = read_file(path) or {
        return "default content"
    }
    return content
}
```

Chaining error handlers:

```

type ComplexOperation enum {
    Ok(str)
    OpenError(OpenFile)
    ReadError(ReadFile)
    ParseError(ParseData)
}

fn complex_operation(path str) ComplexOperation {
    let .Ok(f) = open_file(path) wrap .OpenError
    let .Ok(data) = read_file(f) wrap .ReadError
    let .Ok(parsed) = parse_data(data) wrap .ParseError
    return .Ok(parsed)
}
```

## 6. Operator Precedence

This section describes operator precedence and associativity in the Manta parser.

### Precedence Levels (Highest to Lowest)

1. **Call** (highest)
   - Function/method calls and indexing
   - Tokens: `OpenParen`, `OpenBracket`

2. **Prefix**
   - Unary operators: `-` (negation), `!` (boolean not), `&` (address-of), `*` (dereference)

3. **Multiplicative**
   - `*`, `/`, `%`
   - Left-associative: `a * b / c` → `(a * b) / c`

4. **Additive**
   - `+`, `-` (binary)
   - Left-associative: `a + b - c` → `(a + b) - c`

5. **Comparison**
   - `<`, `>`, `<=`, `>=`
   - Non-associative (chaining not allowed)

6. **Equality**
   - `==`, `!=`
   - Non-associative

7. **Logical AND**
   - `&&`
   - Left-associative: `a && b && c` → `(a && b) && c`

8. **Logical OR**
   - `||`
   - Left-associative: `a || b || c` → `(a || b) || c`

9. **Bitwise AND**
   - `&`
   - Left-associative

10. **Bitwise OR**
    - `|`
    - Left-associative

11. **Assignment** (lowest)
    - `=`
    - Right-associative: `a = b = c` → `a = (b = c)`

### Examples

- `a + b * c` → `a + (b * c)` (multiplication binds tighter than addition)
- `-a * b` → `(-a) * b` (prefix binds before multiplication)
- `a && b || c` → `(a && b) || c` (AND has higher precedence than OR)
- `a = b + c` → `a = (b + c)` (assignment is low precedence)

## 7. Memory Management

Purpose: provide raw allocation primitives for explicit memory management without garbage collection.

### Pointers

Reference types are strongly typed pointers:

```
let p *i32           // pointer to i32, must be initalized before use
let arr *[10]i32     // pointer to array of 10 i32s
```

Dereference with `*`:

```
let val = *p         // dereference p to get i32
*p = 42              // write through pointer
```

Address-of with `&`:

```
let ptr = &variable  // get address of variable
```

For better type safety, prefer sum types (`enums`) for optional values:

```
type Maybe enum {
    Some(i32)
    None
}
```

### Allocation Functions

Allocation functions:

```
new(@T)           // allocate a new value, returns *T
free(ptr)        // deallocate
```

## 8. Examples and Patterns

### File Reading with Error Handling

```
mod main

use (
    "os"
)

type FileError enum {
    Ok(str)
    OpenFailed(str)
    ReadFailed(str)
}

fn read_file(path str) FileError {
    let .Ok(f) = os::open_file(path) wrap .OpenFailed
    defer { os::close_file(f) }
    
    let .Ok(content) = os::read_to_string(f) wrap .ReadFailed
    return .Ok(content)
}

fn main() {
    let .Ok(content) = read_file("/tmp/data.txt") or(err) {
        print("Failed to read file: ", err)
        return
    }
    
    print("File contents: ", content)
}
```

### Pattern Matching and Control Flow

```
type Status enum {
    Success(i32)
    Warning(str)
    Failure(str)
}

fn handle_status(status Status) {
    match status {
        .Success(code) {
            print("Success with code: ", code)
        }
        .Warning(msg) {
            print("Warning: ", msg)
        }
        .Failure(msg) {
            print("Failed: ", msg)
        }
    }
}
```

### Struct and Enum Usage

```
type Point struct {
    x i32
    y i32
}

type Color enum {
    Red
    Green
    Blue
}

fn create_point() {
    let p = Point { x: 10, y: 20 }
    let r = Color.Red
    let Color(g) = .Green
}
```

## 9. Edge Cases and Future Work

### Double-Free Prevention

- Freeing memory allocated by `new` twice is undefined behavior
- The compiler should warn on potential double-frees
- Programmers must ensure `free` is not called both manually and in deferred blocks for the same pointer

### Null Safety

Rather than a `null` value, manta provides sum types to create optional values.
The unsafe package does have a `none` value which can be used for zero value pointers.
However, this should not be used outside of type unsafe code.

### Control Flow

- Conditional expressions (`switch`)
- Loop statements (`loop`, `for`)

### Module System

- Module visibility modifiers (`pub`)

## Appendix: Lexical Tokens

### Identifiers and Keywords

- Identifiers: `[A-Za-z_][A-Za-z0-9_]*` (case-sensitive)
- Keywords: Reserved words listed in section 1

### Literals

- Integers: `[0-9_]+` (base-10)
- Floats: `[0-9]+\.[0-9]+` (decimal)
- Strings: Double-quoted UTF-8 text
- Booleans: `true`, `false`

### Punctuation

- Delimiters: `{`, `}`, `(`, `)`, `[`, `]`
- Operators: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `&&`, `||`, `&`, `|`, `!`, `=`
- Module paths: `::`
- Other: `,`, `.`, `*` (pointer prefix)

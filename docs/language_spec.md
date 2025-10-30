# Manta language - core features draft

This document describes the initial core language features and concrete syntax proposals for the Manta language. It covers:

- Defer keyword
- Manual memory management via `new` and `free`
- Proper Sum types using Rust-style enumerations
- Rust-like keywords and primitive type names (`fn`, `i8`, `f32`, etc.)
- Zig-style `try`/`catch` error propagation syntax

This is a draft intended to guide lexer/parser and AST design.

## Design goals

- Small, explicit systems-language style: manual memory management, no GC, explicit ownership by programmer.
- Familiar look-and-feel for Rust/C/Zig/Golang programmers.
- Simplicity for compiler implementation: explicit constructs with small number of cases.

## 1) Keywords and primitives

Keywords (reserved):

- fn, return, if, else, while, for, break, continue, defer, new, free, try, catch, struct, enum, match, let, const

Primitive integer and float type names:

- i8, i16, i32, i64, isize
- u8, u16, u32, u64, usize
- f32, f64
- bool, char

Type syntax:

- Simple: `let x i32`
- Pointer types: `*T` (raw pointer)

Function syntax:

// notice that the i32 type can be listed only once at the end of the
// parameter list like in Go
fn add(a, b i32) i32 {
    return a + b
}

Short forms:

x := 3    // type inference
let x i32 // explicit type

## 2) Defer

Purpose: run cleanup code at the end of the current scope. Works similarly to Go's `defer` and Rust's `Drop` but explicit and lexically scoped. This is particularly useful for ensuring that allocated resources are cleaned up at the end of the function in which they are used.

Syntax:

defer { /* statements */ }

Semantics:

- When a `defer` statement is executed, the block is pushed onto a per-scope stack of deferred actions.
- Deferred blocks run in LIFO order when exiting the current scope — on normal exit or due to a `return` or due to unwinding from a `try/catch` failure (subject to semantics below).
- Deferred blocks run even if a function returns early.
- If a deferred block uses variables from the surrounding scope, those are captured by reference to the variables as they exist at defer-execution time (i.e., when the deferred block runs). Captures are shallow — care required with pointers.

Examples:

fn copy(dst *u8, src *u8, n usize) {
    defer { free dst }
    // ... use dst
}

Edge cases:
- If `free` is called both in deferred block and manually, this is a double-free bug — runtime should not silently prevent it. The language docs will warn programmers, and the complier should try to warn on this as well.

## 3) Manual memory management: `new` and `free`

Purpose: provide raw allocation primitives. No GC or automatic reference counting.

Syntax:

p := new(i32)        // type is *i32
arr = new([10]i32) // allocate array of 10 i32s. Type is [10]i32
arr1 = new([]i32, 10) // allocate slice of i32s with length and capacity of 10. Type is []i32
arr2 = new([]i32, 10, 0) // allocate slice of i32s with length of 0 and capacity of 10. Type is []i32

free(p)
free(arr)
free(arr1)
free(arr2)

Allocation semantics:
- `new(T)` returns a raw pointer `*T` pointing to newly allocated memory, zero-initialized by default.
- `new([N]T)` allocates N contiguous elements and returns an `[10]i32` which is a distinct type.
- `free(ptr)` deallocates memory previously allocated by `new`.
- Behavior on freeing memory not from `new` or double-free is undefined (program behavior unspecified).

Ownership and pointer safety:
- The language does not enforce ownership at compile time. It's the programmer's responsibility.

Examples:

fn example() {
    p := new(i32)
    *p = 10
    free(p)
}

fn make_slice(n usize) []i32 {
    a = new([]i32, n)
    return a
}

## 4) None value and Option-like semantics

We use Rust-inspired syntax to handle optional or result types using sum types.
Thes sum times can be defined using the enum keyword.

Type-level:
- To keep things simple, the language does not support compile time macros (yet).
- Instead, custom sum types can be easily defined for a given type with Some and None variants
- Syntax is simplified by complier by allowing simple type inference on enum types
- By convention Manta uses `Maybe` as the prefix for Optional types

Syntax:

// here the type def
type Maybei32 enum {
    Some(i32) // variants can have values since they are proper sum types
    None
}

// and the function that will use it
fn maybe_div(a, b i32) Maybei32 {
    if b == 0 {
        // Maybei32.None is also allowed here but the complier is smart enough to infer
        // what should fill the type hole for the expression to be correct.
        // Only simple versions of this are supported however, more complex cases require
        // the code to be explicit
        return .None 
    }
    return .Some(a / b)
}

Unwrapping options:
- `match` is preferred for it's explicitness.
- `if .Some(y) = x` style syntax will be a future addition, but will not be in the first version
- `try/catch` syntax can also be used to unwrap variants

## Match (pattern matching for sum types)

Purpose: provide a structured, exhaustive way to deconstruct sum types (enums) and bind their payloads.

Syntax examples:

match value {
    .Some(v) {
        // use v
    }
    .None {
        // handle none
    }
}

Match is an expression and can yield a value:

result := match maybe_val {
    .Some(v) { v * 2 }
    .None { 0 }
}

Patterns supported initially:

- Enum variant patterns: `.Variant(...)` with optional bindings (e.g. `.Ok(x)`).
- Literal patterns: numbers, strings, booleans.
- Wildcard `_` to match anything.
- Variable binding: a bare identifier binds the matched value.

Semantics:

- Match arms are tested in order. The first matching arm is taken.
- The match expression must be exhaustive. If not all variants/literals are handled, the compiler requires a wildcard arm (`else`) or will emit an error.
- Patterns can bind inner values which are only in scope within the arm block/expression.
- No implicit fallthrough between arms.
- Match can be used both as a statement (for control flow) and as an expression that yields a value.

Guards and nested patterns:

- Pattern guards (`if` conditions) are a planned future addition for more expressive matching; not required in the first version.
- Nested patterns (matching tuples or nested enums) are supported in a limited form (e.g. `.Pair(.Some(x), .None)`), but complex nesting will be incrementally expanded.

Examples:

type Maybei32 enum {
    .Some(i32)
    .None
}

fn describe(m Maybei32) string {
    return match m {
        .Some(v) { "has value" }
        .None { "none" }
    }
}

// Using match as control flow
match res {
    .Ok(val) { process(val) }
    .IOError { log("io error") }
    else { log("other error") }
}

Grammar snippet (pseudo-EBNF):

match_expr := 'match' expression '{' match_arm+ '}'
match_arm := pattern block
pattern := variant_pattern | literal | identifier | 'else'
variant_pattern := '.' identifier ( '(' pattern_list? ')' )?

Integration with `try`/destructuring:

The existing destructuring assignment style (e.g. `.Some(x) := try foo()`) is a convenience that desugars to a `match` or conditional check; the `match` form is the general, explicit mechanism and is preferred for complex cases.

## 5) try/catch error propagation

Design goal: small and ergonomic error propagation similar to Zig's `try` but with a `catch` block for handling failures.

Concepts:
- Like the Optional types above, Result types are explicitly defined for now
- `try` can be used used to unwrap these errors just like with other sum types
- By convention Manta uses `Err` as the previx for Result types.
- `catch` can be used to handle or convert errors.

Standard Syntax:

.Some(x) := try foo() catch (e) {
    // handle error and return from the function
    // This block must return else the value of x
    // would be invalid
}

Semantics:
- `foo()` return a `ErrT`, if the value is a `ErrT.Some` variant, x is unwraped and set to that value.
- If the return value is any other variant, e is set to that value and IS NOT UNWRAPED.

Default Syntax:

.Some(x) := try foo() else 0

Default Semantics:
- `foo()` return a `ErrT`, if the value is a `ErrT.Some` variant, x is unwraped and set to that value.
- If the return value is any other variant, the value of x is set to 0.

Shorthand Syntax:

.Some(x) := try foo()

Shorthand Semantics:
- `foo()` return a `ErrT`, if the value is a `ErrT.Some` variant, x is unwraped and set to that value.
- If the return value is any other variant, that variant is returned from the function (early return).

Combining with `defer`:
- Deferred blocks should run before propagating an error (i.e., when `try` causes an early return, run defers in the current scope first), ensuring cleanup.

Examples:

type ErrStr enum {
    .Ok(str) // proper sum type variant with a value
    .IOError
}

fn read_or_default(path *u8) ErrStr {
    .Ok(f) := try open(path) catch { return .IOError }
    defer { close(f) }

    .Ok(s) = try read_to_string(f) catch { return .IOError }
    return .Ok(s) // the complier infers the correct enum variant
}

.Ok(content) := try read_or_default("/tmp/x.txt") catch (e) {
    // return fallback string
    return ""
}

## 6) Parsing notes and grammar snippets

Tokenization highlights:
- Keywords: listed above.
- Identifiers: [A-Za-z_][A-Za-z0-9_]* (case-sensitive)
- Integers: [0-9_]+ with optional suffixes
- Strings: double-quoted
- Punctuation: { } ( ) [ ] , ; * & | !

Minimal grammar examples (pseudo-EBNF):

function_decl := 'fn' identifier '(' param_list? ')' (type)? block
param_list := param (',' param)*
param := identifier type?
block := '{' statement* '}'
statement := let_stmt | expr_stmt | defer_stmt | return_stmt | ...
let_stmt := identifier (':')? ('=' expression)?
defer_stmt := 'defer' block

expression := try_expr | binary_expr | unary_expr | primary
try_expr := 'try' expression ('catch' '(' identifier ')' block)?

## 7) Examples

Full example showing defer + new/free + try/catch + Option:

type MaybeErri32 enum {
    .Ok(i32)
    .None
    .IOError
    .InvalidInt
}

fn process(path *u8) MaybeErri32 {
    .Ok(f) := try open(path) catch { return .IOError }
    defer { close(f) }

    buf := new([]u8, 1024)
    defer { free(buf) }

    .Ok(n) := try read(f, buf, 1024) catch { return .IOError }
    if n == 0 {
        return .Ok(None)
    }

    // parse integer from buf
    .Ok(val) := try parse_int(buf) catch { return .InvalidInt }

    return .Ok(val)
}

## 8) Edge cases and follow-ups

- Define exact semantics for captures in `defer` blocks (by reference vs copy). Recommendation: capture variables by reference; if the user needs copies, they should explicitly copy into the defer block.
- Decide on typed `free` (should `free` require a `*T` or accept `void*`?). Recommendation: `free(ptr: *any)` is accepted.

## Reference types and `nil`

Overview:
- Reference types in Manta are strongly typed pointers to other types (e.g. `*T` where `T` is any type). They are not untyped `void*` values. This enables better static checking and clearer APIs.
- Initially, reference types can contain the sentinel value `nil` to represent the absence of a value. Long-term, the goal is for control-flow and dataflow analysis combined with sum types (enums/Option) to ensure that `nil` values are eliminated in safe code paths.

Syntax and typing:
- A reference type is written as `*T` where `T` is the pointee type (primitive, struct, enum, etc.). Example: `*i32`, `*Foo`, `*Maybei32`.
- `nil` is a literal with a polymorphic reference type. It can be assigned to any `*T`.

Semantics:
- Strong typing: a value of type `*T` cannot be implicitly assigned to `*U` unless `T == U` or an explicit cast is provided.
- `nil` may be stored into any `*T` variable, but operations that dereference `nil` are undefined/compile-time error where the compiler can prove it; otherwise they are a runtime error.
- The recommended idiom is to prefer `Option`/sum-types for values that may be absent and reserve `nil` references for interoperability and incremental migration.

Control-flow analysis plan:
- The long-term plan is to implement a control-flow and dataflow analysis pass that tracks `nil`ness across branches and ensures that dereferences only occur when statically provable non-nil. This will be similar in spirit to non-null analysis in modern compilers.
- When combined with sum types, the compiler can prefer `MaybeT` for safe absence and use `nil` only where necessary. Over time, we may add language-level annotations or `nonnull` qualifiers to enforce non-nullability.

Examples:

let p *i32 // default value here is nil
if some_condition {
    p = new(i32)
    *p = 10
}

// Later deref: compiler will require proof that p is non-nil here; otherwise programmer must check
if p != nil {
    print(*p)
} else {
    // handle absence
}

Integration with `defer` and `new`/`free`:
- Because references are typed, `free` takes a `*T` and performs deallocation for that typed allocation. `free(nil)` is a no-op.
- `defer` can capture typed references and will run cleanup even if they are `nil` (user code should check before freeing if necessary).

## Next steps

- Convert this draft into concrete grammar rules for the lexer and parser.
- Design token kinds and AST node types for each construct.
- Implement simple interpreter or codegen paths for testing semantics.

---

Document created by GitHub Copilot draft generation.

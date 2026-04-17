# Proposal: Immutable Pointer Types (`~T`)

## Problem

Mutability in Manta exists in two distinct places:

1. **Bindings** — controlled by `mut`. A `mut` binding can be rebound or written through.
   A `let` binding cannot.
2. **Pointer targets** — currently uncontrolled. Any pointer can be written through as long
   as its binding is `mut`. There is no way to express "this pointer target is read-only"
   independently of the binding.

These are genuinely separate concerns. A binding being mutable says something about the
*variable* — whether it can be rebound or written through by the code that holds it. A
pointer target being immutable says something about the *data behind the pointer* — whether
any code receiving this pointer is allowed to write through it. They can vary independently:

- You may want a mutable binding to an immutable pointer: rebind the variable, but never
  write through it.
- You may want an immutable binding to a mutable pointer: the variable can't be rebound,
  but writing through the pointer is fine.

Without a way to express immutable pointer targets, function signatures cannot make
promises about their pointer parameters:

```manta
fn print_value(p *i32) {}
```

A caller reading this signature today cannot know whether `print_value` will write through
`p`. The type provides no guarantee.

### The cost of conflation

Languages like Rust use `mut` in both positions — `let mut x` for bindings and `&mut T`
for references. This is a persistent source of confusion for learners because the same
keyword means two different things depending on where it appears, and the two concepts
interact in non-obvious ways. A developer learning Rust must hold both meanings in mind
simultaneously and figure out from context which one applies.

Manta has an opportunity to avoid this from the start by keeping the two concepts
syntactically separate: one mechanism for binding mutability, a different mechanism for
pointer target mutability, with no keyword overlap between them.

---

## Alternatives Considered

### Option A: `*mut T` / `*T` — mutable/immutable pointer types

Introduce an explicit `mut` qualifier on pointer types. `*mut T` is a pointer you can
write through; `*T` is a pointer you cannot. This is the model used by Rust for raw
pointers.

```manta
fn print_value(p *T) {}    // read-only: cannot write through p
fn set_value(p *mut T) {}  // read-write: can write through p
```

**Why it was set aside:** `mut` now appears in two positions — on bindings (`mut x = ...`)
and inside types (`*mut T`). This is exactly the conflation we are trying to avoid. A
developer has to learn that `mut` on a binding and `mut` inside a type are related ideas
but with different rules and implications. It is a smaller version of the same problem
Rust has.

---

### Option B: `*const T` / `*T` — const-qualified pointer types

Use `const` as the qualifier for read-only pointer targets. `*T` is writable by default;
`*const T` signals that the target cannot be written through. This mirrors C's `const T*`
and has significant prior art.

```manta
fn print_value(p *const i32) {}  // read-only
fn set_value(p *i32) {}          // read-write
```

**Why it was set aside:** `const` is a top-level declaration keyword in Manta
(`const PI = 3.14`). Reusing it inside type expressions for a related but distinct
concept adds a second meaning to a keyword that already has one. It also makes `const`
mean subtly different things depending on where it appears — a declaration-level `const`
is a named immutable value, while a type-level `const` is a pointer qualifier. The
C precedent helps with familiarity but also imports C's own confusion around `const`
placement and meaning.

---

### Option C: Full reference types with exclusivity (`&T` / `&mut T`)

Adopt Rust's full reference model: `&T` is a shared immutable reference, `&mut T` is an
exclusive mutable reference. The exclusivity guarantee — only one `&mut` reference can
exist at a time — is what makes `&mut T` meaningful as a safety tool.

**Why it was set aside:** The exclusivity guarantee requires the compiler to track
aliasing, which adds significant complexity to both the type system and the mental model.
It is also in tension with Manta's target of retro hardware, where single-threaded
execution makes aliasing a less critical safety concern. The full Rust model solves a
problem Manta does not need to solve at this stage, at a complexity cost that is not
justified by the benefit.

---

## Recommendation: `~T` — read-only pointer type

Introduce `~T` as a distinct pointer type meaning "pointer to read-only data." `*T`
remains the read-write pointer. The two are syntactically distinct sigils with no keyword
overlap with `mut`, which stays exclusively on bindings.

```manta
fn print_value(p ~i32) {}   // read-only: compiler rejects writes through p
fn set_value(p *i32) {}     // read-write: writes through p are allowed
```

### Reasoning

**`mut` and `~` do not overlap.** `mut` appears on bindings. `~` appears on pointer
types. A developer learning Manta encounters two clear rules: "`mut` before a name means
the variable can change; `~` before a type means you cannot write through the pointer."
There is no position where both could appear and cause ambiguity.

**The default is ergonomic.** `*T` is the common case — a pointer you intend to use.
`~T` is the constrained case — a pointer you are passing to code that should only read.
This matches the expected frequency: most pointers in systems code are used for both
reading and writing. Read-only pointers are a specific contract you reach for when you
need to make a promise at a function boundary.

**It composes cleanly with binding mutability.** The two axes are orthogonal and can
be combined without ambiguity:

```manta
let p ~i32 = get_ptr()    // immutable binding, read-only pointer
mut p ~i32 = get_ptr()    // mutable binding (can rebind p), read-only pointer
let p *i32 = get_ptr()    // immutable binding, read-write pointer
mut p *i32 = get_ptr()    // mutable binding, read-write pointer
```

**It sidesteps the pointer zoo.** Adding `~T` is a single addition. It does not require
a nullable variant (`~^T`?), an exclusive variant, or any interaction with other planned
pointer types. Nullable pointers are a separate design question and are intentionally
out of scope for this proposal.

---

## Code Examples

### Read-only function parameter

```manta
fn sum(data ~[]i32) i32 {
    mut total = 0
    for v in data {
        total = total + v
    }
    return total
}
```

The caller knows from the signature that `sum` will not modify the slice contents.

### Contrast with a mutating function

```manta
fn zero_out(data *[]i32) {
    for i in 0..<data.len {
        data[i] = 0
    }
}
```

`*[]i32` signals clearly that `zero_out` will write through the pointer.

### Binding mutability and pointer mutability are independent

```manta
fn find_first(haystack ~[]i32, needle i32) ~i32 {
    // haystack binding is immutable, and we cannot write through it
    // returning a ~i32 propagates the read-only contract to the caller
}
```

```manta
// mutable binding, read-only pointer:
// p can be rebound to point elsewhere, but *p cannot be written through
mut p ~i32 = get_value()
p = get_other_value()   // fine: rebinding the binding
*p = 42                 // error: p is a read-only pointer
```

---

## Where Immutable Pointers Matter

Not all uses of `~T` are equally important. In rough priority order:

**1. Function parameters** — by far the most common and most valuable use case. This is
where `~T` does most of its work: a function signature is a public contract, and being
able to say "I will not modify the data behind this pointer" is immediately useful to
every caller. This is the use case that motivated the proposal and should be considered
the minimum viable implementation.

**2. Return types** — a function can return a `~T` to hand the caller a read-only view
into data it owns internally, without copying. Useful for things like returning a pointer
into an internal buffer or data structure where copying would be wasteful but handing out
a writable pointer would be unsafe. Less common than parameters but a natural second step.

**3. Struct fields** — a `~T` field expresses that the struct holds a reference to data
it does not own and will not modify. Useful for things like a renderer holding a read-only
reference to a scene. Can be worked around with conventions in the short term.

A phased implementation is reasonable: shipping `~T` for function parameters first
delivers the majority of the value. Return types and struct fields can follow.

---

## Open Questions

- **Coercion**: Should `*T` coerce implicitly to `~T` at function call sites, or must
  the caller explicitly convert? Implicit coercion (`*T` → `~T`) is ergonomic and safe
  (weakening write permission is always sound). Explicit conversion keeps the type system
  simpler. This mirrors C's implicit `T*` to `const T*` decay, which is widely considered
  the right default.

- **Return types**: If a function returns `~T`, the caller receives a read-only pointer.
  What does it mean to assign that to a `*T` binding? This would be unsound (stripping
  the read-only guarantee) and should likely be a type error.

- **Inference**: When a pointer is never written through in a function body, should the
  compiler infer `~T` automatically, or should the annotation always be explicit? Explicit
  annotation is simpler to implement and makes intent clearer at function boundaries.

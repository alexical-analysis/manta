# Proposal: Generics

## Problem

Generics are a powerful addition to any type system and when used well can significantly improve the
usability and readability of code. Generics are particularly useful in the construction of abstract
data structures like slices, hash maps, linked-lists, and B-trees. A good generic system can even be
leveraged to support seeminly unrelated language features. For example Rust makes excellent use of its
generics system to track lifetimes, and to support it's zero cost closures.

The primay problem that Manta seeks to solve with generics is fairly simple. There are many abstract
data structures and types that any sufficently powerful language needs to support. As mentioned above
generics are a key way to solve this problem. In particular Manta will require 2d and 3d data types
to properly support it's intended use case. Matricies, vectors, and fix-point values are absolute must
haves if Manta is to properly serve it's target audience.

## Goals

There are a few key concerns worth keeping in mind while working through this proposal

Generic types and polymorphic function monomorphization is another key goal. Runtime generic systems like Go's
interfaces are powerful but have CPU and memory costs. Providing a tool that allows developers to move
the more simple polymorphic patterns into a pre-runtime system is another key consideration.

## Non Goals

---

## Background

Generic systems are a load bearing feature of many languages. They range in complexity and usefulness
depending on how they interact with the language but few mature languages get away without them. Go
was notable for a long time as a holdout but even it introduced generics eventually. Generics tend to
be focused on enhancing the capabilities of the type system in the language and look to improve readability
and consiseness of syntax.

Manta could implement a generic system that works similarly to how other system languages like Rust
and Go handle things.


This syntax is both legible and useful in preventing syntax explosion. However, this is deeply tied
to the type system and does little to address other concerns about moving compilation out of the
programs runtime. Generating LUTs and precomputing physics are not common use cases of most generics
systems and trying to move those computations into a generic type system would likely introduce a large
amount of complexity to the type system.

---

## Syntax Aleternatives Considered

## Option A: Angle Brackets 

TODO: this is in progress

```
type Vec3<T> struct {x: T; y: T}

fn main() {
  let f32_vec = Vec3<f32>{x: 3.14, y: 2.718}
  let i64_vec = Vec3<i64>{x: 42, y: 0}
}
```

## Option B: Square Brackets

TODO: this is in progress

```
type Vec3[T] struct {x: T; y: T}

fn main() {
  let f32_vec = Vec3[f32]{x: 3.14, y: 2.718}
  let i64_vec = Vec3[i64]{x: 42, y: 0}
}
```

## Option C: Direct generic datastructure support

TODO: this is in progress

```
fn main() {
  let f32_vec = vec3[f32]{x: 3.14, y: 2.718}
  let i64_vec = vec3[i64]{x: 42, y: 0}
}
```

---

## Implementaiton Alternatives Considered

### Option A: GCStencile generics

### Option B: Fully monomorphized generics

### Option C: Directy generic datastructure support

Rather than support user level generics, instead compiler level generics could be supported instead.
This is essentially the approach that Go took before realeasing generics in 1.18. Essential types like
slices and maps were generic before 1.18 but defining generic types was only possible from within the
compiler itself.

### Option D: Compiler plugin generics

This option sitts somewhere between supporting a developer space generics system and having a fully
internalized generics system as recomended in Option C. Essentially, generics types would still be compiler
only constructs but, developers could provide plugins to easily extend the compiler without needing
direct action on the part of compiler developers.

---

## Recommendation: Direct generic datastructure support

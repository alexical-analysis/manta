# Proposal: Comptime

## Problem

There are many common patterns in programming that require a meta construct beyond direct syntax. For 
example, sometimes coding patterns can become extremely repetitive and verbose to handle the various
variants of a given syntax. Defining a dozen different variants of a Vector or Matrix, or
support printing functionality for a dozen variants in an Enum, are good examples. While these can be written manually 
it is often not desirable to do so both because it is tedious but also because the resulting code becomes
difficult to read and prone to small errors that can be difficult to find in the sea of syntax.

There are also concerns about performance baked into this problem. A developer may reasonably want to
compute some complex expression ahead of runtime to preserve CPU cycles during runtime. Look up tables
are a great example of this and can provide a massive performance gain at the cost of memory. The tradeoff
is often well worth it.

There are also other patterns that become strictly impossible for the developer to implement without
some sort of pre-runtime pattern. This includes things like custom invariant checking, requiring specific
values to be known at compile time, and providing zero-cost abstractions as part of an API or SDK. While
some of these considerations can be baked directly into a compiler, or handled by external code generators,
these systems lose flexibility and security of a dedicated, blessed method provided by the compiler.

### Design Considerations

Given Manta's goals of being a productive language for retro console development there are a few key
aspects of this proposal worth highlighting.

Moving complex calculations from runtime to pre-runtime/compile-time is absolutely essential. Many retro
consoles have much weaker CPUs than modern hardware and require a lot of work to ensure good performance.
Patterns like generating look up tables (LUTs) and pre-computing complex but deterministic physics
or lighting information are a key consideration for any meta-programming or compile-time system.

Generic types and polymorphic function monomorphization is another key goal. Runtime generic systems like Go's
interfaces are powerful but have CPU and memory costs. Providing a tool that allows developers to move
the more simple polymorphic patterns into a pre-runtime system is another key consideration.

Simplifying syntax is an often touted benefit of comptime tools. Quite often there are simple repeated
patterns that show up in code. Sometimes it's fine to just have this code written out and using tools
like nvim macros can make this process less painful at time of writing. However
there are legitimate use cases where compressing syntactic patterns into a generic or comptime can
improve both legibility and usability. Manta seeks to support more readable code as a core goal.

The system should be as simple as possible without sacrificing the other core goals. For ultimate performance
and control languages like Rust, C, or Zig will likely be preferable to Manta. This is fine as the goal
for Manta is to provide the necessary performance while still prioritizing developer productivity. Given 
this fact, it is appropriate to leave some use cases/performance on the table in order to prioritize
simplicity. This however should be done very carefully.

### Non-goals

Covering every possible use case of a generics or macro system is out of scope of this proposal. While
there are a lot of very interesting and powerful things that can be accomplished by a good macro system
or a complex generics system, such possibilities can often add complexity to the language. These sorts
of systems often do lead to some additional complexity which is fine but targeting the 80% of most common
use cases often provides huge benefits at a comparatively low cost.

Preventing every last unnecessary CPU cycle is also not a core requirement. While this system should allow
developers to help their code perform more efficiently, an 80% approach is also preferred here. On many 
retro consoles, true performance requires writing key portions of the code in assembly directly anyway.
Zero cost abstractions can be powerful but often come with a tax in the form of more complex compilation,
a more nuanced type system or both. This makes sense for some languages but is not a price that Manta
is interested in paying.

---

## Alternatives Considered

There are several existing patterns for solving the problems described above. Below we consider several
alternative options for how such systems might be implemented in manta and why we chose not to implement
them.

### Option A: Generics

Generic systems are a load bearing feature of many languages. They range in complexity and usefulness
depending on how they interact with the language but few mature languages get away without them. Go
was notable for a long time as a holdout but even it introduced generics eventually. Generics tend to
be focused on enhancing the capabilities of the type system in the language and look to improve readability
and consiseness of syntax.

Manta could implement a generic system that works similarly to how other system languages like Rust
and Go handle things.

```
type Vec3<T> struct {x: T; y: T}

fn main() {
  let f32_vec = Vec3<f32>{x: 3.14, y: 2.718}
  let i64_vec = Vec3<i64>{x: 42, y: 0}
}
```

This syntax is both legible and useful in preventing syntax explosion. However, this is deeply tied
to the type system and does little to address the other concerns about moving comilation out of the
programs runtime. Generating LUTs and precomuting physics are not common uses cases of most generics
system and trying to move those computations into a generic type system would likely introduce a large
amount of complexity to the type system.

This solution was therefor discarded as there are other, more comprehensive systems that we identify
later on in this proposal.

### Option B: Macros

Macro systems have been another popular option show up in a huge variety of languages including Lisp,
C, and Rust. The variety of languages that support macros are a testimate to thier flexibility and power.
Languages like Rust even leverage Marcos along with their generic type system to create exteamly flexible
syntax and types.

A key advantage of using Macros is that, while they can be used to create generic types. There primary
beneifit is to reduce repeated code patterns. The unless macro is the quitessential example of this.

```
macro unless!(check ast::Expr, body ast::Block) ast::Stmt {
  return `if !${check} ${body}`
}

fn main() {
  let a = 20
  unless a >= 100 {
    print("a is less than 100")
  }
}
```

Despite the flexibility of a well designed macro system there are a few well known drawbacks to implementing
such a system in a given language. Firstly macros are notoriously difficult both the debug and to provide
diagnostics around. Additionally, macro systems tend to quickly become confusing and can be abused quite
easily. Macros operate on the syntax of the language which while powerful, can lead to an explosion 
in syntax for complex use cases. 

While well designed hygenic macro systems are powerful, they don't address the key concerns of this
proposal, therefor we're dismissing this solution for now.

---

## Recommendation: Angle Bracket Comptime Expressions

Zig is the key inspriation in this decision as it has a robust compile time interpreter that it uses
to great effect. There are several benefits that I appreciate about a compile time intepreter.

* The compile time langauge can be a simple super-set of the regualr language reducing added complexity
* Zig has demonstrated that comptime can be a sutible mechanism for replacing many of the uses cases
  generics serve in other language eco-systems
* Comptime is a natural way to move computation out of a program runtime to improve performance.
* Comptime is easier to debug than macro systems as it often looks much like debuging normal code.

### Syntax Examples

Comptime in Manta uses the angle braces (i.e. `<` and `>`) in the same way the normal language usese
parens. So for example the following code forces the expression in the angle brackets to be inteprated
at compile time.

```
let a = < math::sin(math::cos( 2 * math::PI)) * 30 >
```

Because all values in this expression are know to the compiler the full expression can be resolved and
left as a single constant in the resulting binary.

While the above expression may be caught by a powerful enough optimizing compiler, comptime allows for
even more complex expressions that would be unreasonable to expect a compiler to optimize away directly.

```
// sin_lut is a comptime function because it's paramaters are inside angle brackts
// angle bracket paramaters support the interpreters type system which includes passing in `type` expressions
fn sin_lut<T type, n i32> struct{ values [n]T } {
  match T {
    f32 { /* ok */ } 
    f64 { /* ok */ }
    _ { panic!("Sin LUT must be a floating point type") }
  }
  
  return struct {
    values [n]T
    
    fn of(*self, angle T) T {
      let i = int(angle / (2 * math::PI) * n) % n
      if i < 0 { i += n }
      return self.values[i]
    }
  } {
    // nested comptime can be used to ensure certian values are computed as constants
    // closure are useful for this
    values: < fn() [n]T {
      let values = [n]T{n; 0}
      for i in 0..n {
       values[i] = math::sin(i * 2 * math::PI / n)
      }
      
      return values
    }() >
  }
}

// calling a comptime function requires the angle braces
const SIN = sin_lut<f64, 256>

fn main() {
  let s = SIN.of(math::PI / 2)
  assert!(0.9999 < s && s < 1.00001)
}
```

Comptime can also be used as a replacement for generics in many uses cases.

```
// return the type with it's methods
fn Vec<size u8, T type> type {
  return struct{
    data [size]T
    
    fn add(*self, b Self) Self {
      let data [size]T
      for i in 0..size {
        data[i] = self.data[i] + b.data[i]
      }
      
      return Self{ data: data }
    }
  }
}

// construtors need to be their own comptime function but leveraging closures, a function can be returned
fn NewVec<size u8, T type> fn(values ...T) Vec<size, T> {
  return fn(values ...T) Vec<size, T> {
    return Vec<size, T>{ data: values }
  }
}

// a comptime closure (which is what NewVec<3, f64>) returns can be used as the body of a function 
// decl like this
fn new_vec3 NewVec<3, f64>

fn main() {
  // the type of a ad b here are Vec<3, f64>
  let a = new_vec3(1, 2, 3)
  let b = new_vec3(4, 5, 6)
  
  let c = a.add(b)
  
  // the comptime function can be invoked directly
  let d = NewVec<4, f32>(1, 2, 3, 4)
  
  // the type can also be curly brace constructed as long as there are no `new` blocks for the type
  let e = Vec<2, i32>{ data: [2]i32{1, 2} }
}
```

This does have some extra verbosity since a new comptime function is needed both for the type and for 
the constructor. This could be resolved by allowing static functions to appear in the struct definition
but the tradeoff prevents the struct from become a sudo-namespace which is a reasonable tradeoff given
separating structs and constructors is already an expected pattern in Manta

### Weaknesses of this approach

While comptime has many benefits, there are undeniably drawback to incoporating such a system. A key
concern is that the compiler must now support both an interpreted runtime as well as a compiled runtime.
Ensureing these two runtimes have predictably equivilant behavior is a non-trivial undertaking for a
compiler, and yet and absolutly essential requirment to build developer trust.

There are also many tasks that can be accomplished by a sutibly complex macro system of generic type
system which can not be easily reproduced by comptime. A simple example is that a macro system can fully
modify the AST of a program making it nearly as powerful as the compiler itself. Comptime is a more
constrained system and can only support what the interpreter provides. Similarly, generic systems like
Rust's which track lifetimes and traits may be non-trivial to support in a comptime interpreter.

While these concerns are real, we believe they are an acceptable tradeoff for the time being. Importantly
a good comptime system does not preclude the possiblity of later introducing a generics or macro system
if such systems are introduced early while breaking changes are still acceptable. The current phase of
the commpiler allows such ideation.

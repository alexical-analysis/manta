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

Manta could support traditional generics syntax by using angle braces to indicate generic type paramaters.
This syntax is common in many languages such as Rust, TypeScript, and Java making it a execlent choice
for learnability. Developers would quickly understand the syntax as being a generic type.

```
type Vec3<T> struct {x: T; y: T}

fn main() {
  let f32_vec = Vec3<f32>{x: 3.14, y: 2.718}
  let i64_vec = Vec3<i64>{x: 42, y: 0}
}
```

This does have many well know drawbacks however and ends up introducing complexity as the generics system
becomes more complex. An example of this is Rusts trubofish operator. It's a common pitfall that developers
attempt to use the expected generics patterns for things like parsing only to find there are compilation 
errors.

```
// While this looks correct it will not compile
let my_int = other.parse<i32>()
```

Additionally, the C++ compiler has developed a whole host of edge cases that need to be accounted for
which. `vector<vector<int>>` is an example of a tricky syntaxtic pattern that originally was not handled
well since the trailing `>>` is a shift operator symbol. Instead `vector<vector<int> >` untill this
edge case was specifically handled. An example from Typescript is requiring a trailing comma in certian
generic context to prevent parsing ambiguity `<T,>() => x`.

Manta would almost certianlly suffer when trying to correctly parse this syntax and so this will not
be the syntax choice for generics.

## Option B: Square Brackets

Square brackets are not as common for generics but they are prominantly used in Go for generics. Since
Go already has inspired a lot of the syntax in Manta and it's likely that users of the Manta language
will at least be familiar with Go and it's generics. Additionally, becuase the square brackets follow
a similar syntaxtic pattern to more traditional angle brackets (i.e. grouping a list generic type paramaters)
This pattern should be reasonably easy to teach.

```
type Vec3[T] struct {x: T; y: T}

fn main() {
  let f32_vec = Vec3[f32]{x: 3.14, y: 2.718}
  let i64_vec = Vec3[i64]{x: 42, y: 0}
}
```

A major advnatage here is that while there are still ambiguties to resolve around parsing many of the
most tricky edge cases are no longer at play. Neither `[` nor `]]` are valid binary operators in Manta
reducing the parsing ambiguity. It is possible for ambiguous token senquences to occure such as the 
`A[B]` pattern which could be either the generic type `A` instantiated with type `B` or the indexable
type `A` index by the value `B`. However, this is a much easier to disambgiuage and mirrors a existing
gramatical ambiguities such as variant construction `Type.Variant(payload)` and method calls `Type.Method(arg)`.
Simple additions to the noder should easily catch and resolve these ambiguities.

## Option D:  Sigil Prefixed Angle Brackets

Another example is to opt fully into the Rust turbofish syntax ad use `::<>` or some similar sigil for
generics. The main issue that I see with Rusts use of the turbofish syntax is that is does not match
the expected syntax established by the rest of the language. The turbofish syntax does read naturally
and were it used througout the language for generics it would be initutitive in all contexts. 

Another advatage of this approach is it neatly avoid most parsing ambiguity. There case of `>>` will
still present a parsing challenge. However, this is mostly resolved by simply lexing individual `>` rather
than lexing the `>>` token as a single lexeme. Then the responsibility is on the parser to correctly
consume trailing `>` when parsing generics and binary operators will need to be clever enough to recognize
double `>` as a single right shift. These however, should be simple enough to implement.

```
type Vec3::<T> struct {x: T; y: T}

fn main() {
  let f32_vec = Vec3::<f32>{x: 3.14, y: 2.718}
  let i64_vec = Vec3::<i64>{x: 42, y: 0}
}
```

This syntax does still introduce a non-standard generic syntax but does have the beneifit of being a 
known pattern in Rust which should ease learning. It also reads more closely to common generic syntax 
given it's use of angle brackets. It should be easy to teach, intuitive and sidestep many of the issues
present in other systems at the slight cost of a few extra characters. This tradeoff aligns well with
the philosophy behind Manta and would be an excellent choice.

## Option C: Direct generic datastructure support

Directly providing support for common generic data structures in the compiler itself helps to sidestep 
many of the gaps that generics traditionally fill in a language. The most notable example of this strategy
in practice is Go which provided slices and hash maps that could contain any underlying type (including
user defined types). The advantage here is that usecase specific syntax can be provided for types. So
slices get `[]type` while maps get `map[key]value`. These types are more expressive than would be achievable
in a more general generics system which would results in types that look more "generic" `Slice<type>`
and `Map<key, value>`.

```
fn main() {
  let floats = []f32{3.14, 2.718}
  let pets = map[string]Pet{
    "pepper": Pet{type: .Dog, age: 1},
    "booter": Pet{type: .Dog, age: 3},
  }
}
```

In addition to the syntax benefits there are implementation benefits as well but those will be discussed
in more detail below.

## Recomended:  Sigil Prefixed Angle Brackets

I believe that sigle prefixed angel brackets are the correct choice here. Using Go style square brackets
is a close second but the parsing ambiguity, along with the general feeling that square brackets are
an awkward choice lead me to prefer the sigil based approach. This pulls from the Rust ecosystem which
give me confidence that the unknown implications of such a generic syntax are somewhat limited. Rust
generics are very well tested and understood so I can rely on that ecosystem to direct Manta. 

Additionally it has the distinct benefit of being syntaxtically consisten which Rust unfortunatly can
not claim. This solution is not perfect but given my believe that perfect can often be the enemy of 
good, it feels like a reasonable tradeoff.

---

## Implementaiton Alternatives Considered

In addition to simple questions of syntax, concreet implementation plans need to be addressed. There
are many paths for implementing a generics system, each with their own benefits and drawbacks. We'll
explore several of the most promising options below.

### Option A: Memory Stencile generics

Go is a prominant example of this style of generics. Go's dictionary-based monomorphization generics 
reduce the number of uniqe generic variants that need to be generated for a given type, especially when
paired with it's type constraint interfaces. The result is a generic system the prevents the need to
frequently recompile generic code while also providing generics with only a small runtime cost.

Manta could opt into a similar system by leveraging the existing meta-type which serves much the same
purpose of the Go generic dictionary paramater. This would support Manta's goals of providing execelent
develoepr experence through fast compile times. The major downside is that while Go code is often deployed
in contexts where a small runtime overhead is negligable, this is not always true on embedded hardware.
This could result in situations where developers feel like Manta generics are not a viable solution
for their game or project. 

If possible I would like to give greater control to developers here to ballance compile times and runtimes
in a way that properly address the needs of their project.

### Option B: Fully monomorphized generics

Many systems languages choose to fully monomophize their geneirc types and it's clear why this is an 
attractive option. In order to maximally optimize code, generic types need individual implemetations
for every variation. This results in much more code being generated. The oft cited implications here
are slower compile times as well as large final code size. In a language eco-systems like Rust or Zig
the correct choice is clear as both of those eco-systems chase runtime performance as a core selling
point for the language.

The situation in Manta is less clear. As has been stated many times, Manta tried to prioritize excellent 
developer experience as one of it's key goals. Long compile times can greatly hamper a developers productivity
and are often sited as a less desirable aspect of working in Rust and espeically in langauges like C
where compile times can become really painful.

Code size is another really important consideration. Retro hardward often has very limited space and
there are actually examples where reducing code size is more important than raw performance. Kaze Emanuar
has an excellent video covering an example of this for the N64 https://www.youtube.com/watch?v=t_rzYnXEQlE.

Like the above example, this option has drawbacks that would need to be carefully considered and communicated
if it were to be selected.

### Option C: Directy generic datastructure support

Go again servers as the inspration for this option. Before 1.18 go provided a small set of generic style
types that covered some of the major use cases of generic types. Slices and Maps in Go were primative
types built directly into the language rather than being more traditional generic types. This gave the
Go designers the freedom to make much more nuanced choices with those data types specifically. However, 
now that Go has introduced generics there is a divergence in the type system that, while well understood, 
is somewhat awkward.

Manta could elect for a similar system. However, I would like Manta to have a much more robust set of
basic types including slices and maps, but also vectors, matricies, sets, and graphs. This likely means
that a lot of custom code would build up in the compiler and the eventual inclusion of generics may
be even more difficult to nicely integrate into the language. While it makes sense that this path was 
the right call for Go, I don't believe it would work well for Manta. This is espeically true given Go
eventually did end up implementing generics which leads me to believe that Manta would likely follow
in the same path.

### Option D: Generic Support for Memory Stenciling

This option serves as a middle ground between what Rust and Go offers explicit developer control over
the compile time/ runtime tradeoff. Here memory setncil generics use the `@` sigil to communicate that
the generic is based on the meta type, not the type directly. This matches the Go style GC stencil generics
and makes for fast compile times at the slight cost of runtime overhead. Dropping the sigil and providing
a bare identifier for the generic type produces a Rust style generic that is slow to compile but has
no runtime compromizes. 

In addition to compile speed, and runtime speed, stencil generics have implications for code size. Often
retro hardware has not only strict CPU budges but also strict memory requirments. It's well know that
the N64 performance is primarily bound by it's memory transfer speed. Given developers control of the
generic style they use could also help hot code fit more easily in a cache line which may actually obviate
the performance tradoff completely. While power users may dig deeply into the difference to optimize
code for their specific use case the basic tradoff for small size, fast compile times, and slower runtime
vs slow compiles, larger code size and faster runtime are farily easy to comunicate abstractly.

```manta
// MemStenciled - one instantiation per unique {size_of, align_of}
// compiler injects meta at the call site automatically
fn (s *Slice::<@T>) get(i usize) *T {
    let offset = @T.size_of * i
    let item = s.ptr + offset
    return item
}

// Fully monomorphized - one instantiation per concrete type
// use when you need method dispatch or type-specific behavior
fn (s *Slice::<T>) contains(val T) bool {
    // can call methods on T, pattern match, etc.
}
```

standard lib implementations will likely default to MemStenciling generics to support fast compiles
and fully monomorphized generics will be offered to performance sensitive contexts. It should be straightforward
to communicate that stencile generics support fast compiles with a small runtime cost and monomorphic
generics slow compile times to produce faster runtime code.

Note: One possible variation here is to make the switch between stenciled types and monomorphized types
implicit rather than explicit. Debug builds would be fast using memory stenciling and release builds
would be slow and fully monomorphic generic types. Given the risks that generating significantly different
pre-optimized code between debug and release poses, this should not be targeted untill the concreet
differences between these two systems are more deeply understood.

## Recommendation: Generic Support for Memory Stenciling

I believe that for Manta specific use case Option D severs the ecosystem the best. It interacts well
with existing features since the meta-type is an existing pattern in the language. There is of course
some communication and learning overhead introduce by modeling generics this way. However, the abstract
benefits are fairly straightforward to communicate and likely developers will not really need to understand
the differences untill they are working in a larger Manta project.

Additionally, there is an option to potentialy hide this complexity before 1.0 once the system is well
understood. For this reason this is the plan of record for Manta generics going forward.

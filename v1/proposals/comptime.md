# Proposal: Comptime

## Problem

There are several key problems that comptime support in manta could help resolve. The first is bundling
assets into a final binary. For retro consoles like the GBA there is no file system that can be relied
on to load assets. Instead, all palettes, tile-sets, tile-maps, sprite sheets, and audio data need to
be embedded in the final ROM. In many C programs this is solved by using generated literals in header
files but this can really tank compile time performance. Having the ability to embed data directly using
comptime supports Manta's performance as well as its key use case for retro console dev.

Another key use case is handling conditional compilation. Because of the extremely varied nature of
the compile targets that manta seeks to support, excellent conditional compilation is essential. While
larger conditional compilation (i.e swapping out entire modules for specific targets) deserves its
own proposal, smaller swaps (i.e. print on mac vs linux vs windows) could leverage comptime to make
this simpler.

Pre-computing expensive calculations is another key use case of comptime. Systems like the N64 and GBA
often leveraged look up tables (LUT) to maintain stable framerates and reduce the load on the CPU. This
can be achieved by building custom code generation flows but having a built-in comptime concept could
massively improve the standardization and ergonomics of generating and using LUTs.

Generics are another use case that could be covered by comptime. Zig is the prime example of this as
it lets the comptime interpreter access the type system to avoid needing a separate generics system.
Having a single unified system for creating generic types and doing comptime work could help keep manta
simple while also supporting more complex coding patterns.

Comptime could also be used in a similar way to macros. Macros are often conceived as syntax to syntax
functions. If the compiler's interpreter had close enough integration to compilation itself, the comptime
system could manipulate the AST performing user specified checks as well as making modifications and
additions to the AST. Such a robust system would be powerful and also reduce the need for a more traditional
macro system.

### Design Considerations

Given Manta's goals of being a productive language for retro console development there are a few key
aspects of this proposal worth highlighting.

The performance requirements for retro console development are much more strict than most modern targets.
This means that performance tradeoffs need to be carefully considered. For example, Manta forgoes a garbage
collector because, despite the real benefit to developer ergonomics, the performance cost is just not
acceptable for legacy hardware targets.

Developer ergonomics still matter. When the tradeoff makes sense it's actually ok to prioritize the developer
experience over performance. Languages like Zig and Rust provide ecosystems that are extremely focused
on optimizing performance and do an excellent job. Manta should be willing to give up performance for
developer experience when reasonable. Examples of this include Go style interfaces which do have a small
performance cost but provide a lot of developer ergonomics.

Balancing these two goals is a difficult balance as they are often in direct competition. Carefully
considering features with this in mind will hopefully help set Manta apart as a retro development ecosystem.

It's also worth noting that there are separate proposals for generics and macros in development. Collapsing
all functionality into the comptime system feels elegant but it is not a strict requirement. If doing
so does not meaningfully improve the language then separately designed systems should be preferred.

---

## Syntax Alternatives Considered

### Option A: Angle Bracket comptime

Comptime in Manta could use angle braces (i.e. `<` and `>`) in the same way the normal language uses
parens to indicate comptime expressions. So for example the following code forces the expression in the 
angle brackets to be interpreted at compile time.

```
let a = < math::sin(math::cos( 2 * math::PI)) * 30 >
```

Because all values in this expression are known to the compiler the full expression can be resolved and
left as a single constant in the resulting binary. While the above expression may be caught by a powerful 
enough optimizing compiler, comptime allows for even more complex expressions that would be unreasonable 
to expect a compiler to optimize away directly.


While this syntax reads nicely and also makes comptime functions match the form of other languages' generics
it comes with a heavy cost in parsing complexity. For example parsing ambiguity would be introduced
for the expression `my_comptime_fn<256>`. The current parser would read this as the identifier `my_comptime_fn`
is less than the number `256` with a trailing `>`. Special parser logic would be needed to ensure this
expression was parsed correctly. This is why Rust introduced the turbofish syntax `::<>` and why Go
decided against using `< >` for its generics systems. Given Manta's goals around developer experience
and how critical fast parsing can be for efficient compilation, this syntax is not the ideal choice.

### Recommendation: Comptime Keyword

Manta could follow Zig's example and implement a comptime keyword. Likely `comp` would be sufficiently
descriptive for our use cases.

```
let a = comp math::sin(math::cos( 2 * math::PI)) * 30
```

This works exactly the same as the previous example but parsing is trivial as the parser can see the
`comp` keyword prefix and expect an expression to follow. This supports fast parsing and also makes
compile time execution obvious.

---

## Comptime Features

Comptime is an extremely flexible feature and given its proximity to the compiler can be made to be
very powerful. It's worth directly exploring what set of features Manta's comptime should support and
what the implications for the rest of the language might be.

### exact language match

This is the simplest version of comptime, where the interpreter implements a 1:1 copy of the language.
This makes for an easy to teach system as essentially the only additional knowledge that a developer
needs to understand comptime is to wrap their head around the `comp` keyword itself. 

There are some important nuances to consider here. For example, what do types like `usize` become
in the comptime interpreter? Do they match the compilation target or the system on which the code is
being compiled? This has real implications for things like LUTs where the width of values in the table
should match the target. There are other problems with matching the target though as cleaving too closely
to the target system may limit the usefulness of the interpreter. For example, if the interpreter matches
1:1, then bundling assets for targets like the GBA becomes impossible as the GBA comptime interpreter
would not support file operations.

Additionally, ensuring 1:1 compatibility is non-trivial and will require careful work to ensure the
interpreted language does not diverge from the compiled one. This is a real problem in languages like
Go where interpreters exist, but do not map well to the actual binary behavior in many cases. Fortunately,
committing to this early would likely help prevent problematic features from causing the comptime and 
runtime from diverging.

While the challenges are real, the benefits to the ecosystem are too clear to ignore. This seems like
an important base addition to the language.

### conditional compilation

Conditional compilation is another key area that comptime can help developers express. In Zig, target
specific code can be included in the resulting program using the following pattern.

```zig
const std = @import("std");
const builtin = @import("builtin");

pub fn openFile(path: []const u8) !void {
  var open_cmd: []const u8 = "xdg-open";
  if (comptime builtin.os.tag == .macos) {
    open_cmd = "open";
  }
  
  var child = std.process.Child.init(&.{ open_cmd, path }, std.heap.page_allocator);
  _ = try child.spawnAndWait();
}
```

Manta could easily opt into a similar pattern using the following syntax.

```
mod test 

use (
  "compiler"
)

fn my_function(path str) {
  mut open_cmd = "xdg-open"
  if comp compiler.target.os() == .Mac {
    open_cmd = "open"
  }
  
  let cmd = exec.new_command(open_cmd, path)
  let .Ok = cmd.spawn_and_wait() !
}
```

This works great for small differences (i.e. minimal differences in system calls or different constants).
It's also a fairly small addition to the interpreter. The biggest outstanding question introduced by
this addition is what's included in the "compiler" package and what this package does at runtime. Fortunately,
this does not need to be fully fleshed out as simply supporting the basic use case of checking the compile
target can be supported in a backwards compatible way. The most minimal version of "compiler" can be
built to start and future decisions on that API can be pushed forward once more manta code exists.

Because all this data can be statically inserted at compile time this package could even be available
at runtime without any odd implications. This could even be a useful way to inject version information
into a binary, which is something that's often useful in CLIs and other distributable software.

Like the above features, the benefit is clear and the downsides are minimal.

### comptime types

One of the interesting features of Zig's comptime that's worth considering is its types as values approach.
The comptime interpreter can actually treat Zig types as values and manipulate them during comptime.
A simple example of using comptime like a generics system is using comptime to create a generic vec3 type.

```
fn Vec3(comp T type) type {
  // it's valid to declare types inside comptime functions so that generics are possible
  type Vec3 struct {
    x: T,
    y: T,
    z: T,
  }
  
  // Note: that method syntax is still in the proposal process. allowing public decls like this is 
  // the most likely syntax and would need to be supported inside comptime functions
  pub fn (a *Vec3) dot(b: Vec3) Vec3 {
    return a.x * b.x + a.y * b.y + a.z * b.z;
  }
    
  return Vec3
}

// Note: that currently the '=' sign is not required but adding it makes for more clear syntax and it 
// would be a minimal change to the parser at this stage.
type Vec3f = Vec3(f64)

fn main() {
  let a = Vec3f{ x: 0.0, y: 3.14, 2.718 }
  let b = Vec3f{ x: 0.0, y: 3.14, 2.718 }
  a.dot(b)
}
```

Types could even be matched with other comptime values to create more nuanced types

```
// this function creates a generic sine lookup table where n is the number of rows and T is the type
fn SinLUT(comp T type, comp n i32) T {
  match T {
    f32 { /* ok */ } 
    f64 { /* ok */ }
    _ { panic!("Sin LUT must be a floating point type") }
  }
  
  type SinLUT struct {
    values [n]T
  }
  
  pub fn (s *SinLUT) of(angle T) T {
      let i = int(angle / (2 * math::PI) * n) % n
      if i < 0 { i += n }
      return s.values[i]
  }
  
  return SinLUT
}

// this function creates a new sine lookup table at compile time
fn new_sin_lut(comp T type, comp n i32) comp SinLUT(T, n) {
    let values = [n]T{n; 0}
    for i in 0..n {
      values[i] = math::sin(i * 2 * math::PI / n)
    }
      
    return SinLUT(T, n){ values: values }
}

// here the lookup table is computed by the compile time interpreter and represented as a literal constant
// in the final compiled binary. Notice that even though all the parameters are comptime known, in order
// to ensure the function is fully computed during compile time interpretation, an explicit `comp`
// keyword is required. This is because the comptime interpreter is conservative and will do the minimum
// amount of work implied by the comp keyword
const SIN = comp new_sin_lut(f32, 256)

fn main() {
  // this just looks up the angle from the lookup table instead of trying to compute sine directly
  let s = SIN.of(math::PI / 2)
  assert!(0.9999 < s && s < 1.00001)
}
```

While this does have a lot of advantages there are many unknowns with this approach. How does this fit
with Manta's conceptualization of modules? What about C-to-Manta module interoperability? How will types
get monomorphized when comptime functions generate the same type all across a Manta codebase?

There are also important differences between the way that Manta and Zig represent types making type
construction less obvious and elegant in the Manta comptime. Given the significant outstanding questions
that exist this feature will not be included in the initial comptime implementation. Such a system introduces 
more complexity than it's worth given Manta's design goals.

### AST access

AST access through compile time modules could make comptime a real powerhouse. Potentially it could be
a replacement for a macro system and provide the developer with easy access to build custom linting
rules and syntax checks. Consider the following example commonly used to demonstrate macro power.

```
mod test

use (
  "ast"
  "compiler"
)

fn unless(comp condition bool, comp body compiler::Block) ast::Stmt {
  // convert the compiler expression into an ast::Expr
  let condition = comp ast::parse_expr(condition)
  
  // invert the condition
  let condition = ast::Expr(ast::UnaryExpr{
    op: ast::UnaryNot,
    expr: condition,
  })
  
  return ast::Stmt.If(ast::If{
    condition: condition,
    success: compiler::new_ast_block(body),
    failure: .None
  })
}

fn test() {
  // the value of a here can be known at compile time
  let a = 10
  
  // this directly injects the ast into the test function
  comp unless(a > 10, { print("a is not greater than 10") })
  
  // after comptime the tests function includes the following code
  // if !(a > 10) { print("a is not greater than 10") }
}
```

To my understanding this is similar to the way that Jai enables powerful compile time behavioral modifications.

Such a comptime interpreter could also be used to add powerful linting and domain specific compile time
checking of various code. 

```
fn test(count, len i32) {
  // comp blocks are just blocks of code that are passed to the interpreter and disappear after execution 
  comp {
    let .IntLiteral(count) = ast::parse_expr(count) or {
      // this becomes a compile time error
      panic("count must be an int literal")
    }
    
    let .IntLiteral(len) = ast::parse_expr(len) or {
      // this becomes a compile time error
      panic("len must be an int literal")
    }
    
    if count.value >= len.value {
      panic("count must be less than length")
    }
  }
  
  // do the rest of the functions work
}

fn use_test() {
  let a = 10
  let b = 20
  
  // this is a compile time error
  test(a, b)
  
  // this is also a compile time error
  test(10, 5)
  
  // this is not
  test(10, 20)
}
```

While this is powerful there are several major drawbacks that I see to this approach. The first is the significant
spike in complexity that comes along with a system like this. There are many nuances and edge cases
around making a system like this reasonable and manageable. Introducing this amount of power and complexity
makes learning and using the comptime system much more of a burden on developers. It also spikes the
complexity of implementing the compiler which is not a cost that can be fully ignored.

Another problem here is the classic question of how such code would be debuggable. The example simply
panics from inside the comp block but likely a more mature system would need ways to interact with 
the compiler's error reporting system. Also, debuggability for the comptime interpreter becomes even
more complex as now the underlying AST can fully shift as part of the interpreter's work, meaning errors
may not actually have any error site in the original code, nor in the comptime code, but only in the
intermediate representation created in between the two.

This also requires a somewhat stable interface to the internals of the compiler. Developers will expect
a more or less consistent view of the AST and other IRs that are accessible. This would likely result
in either an increased difficulty in implementing compiler features or a split between the public and
private AST types. This is a high cost to accept so early on in the project.

Finally, this has the potential to majorly violate developers' expectations in unusual ways. Forbidding
syntax that would normally be acceptable is fine in theory but I worry that in practice it would be 
abused and contorted into an unmanageable mess by any but the most careful practitioners. It also breaks
developers' ability to pattern match code as specific linting and compiler constraints could be lurking
anywhere in the code.

While powerful, I think this feature should be rejected as it creates too many open questions and ambiguity.
In a sense this would allow developers to be co-authors of the compiler in their own codebase, but this
has serious implications. The performance and flexibility provided by an interpreter that interacts
with the compiler's AST is unmatched but the tradeoffs are just not ones Manta is willing to make.

### Recommendation

We recommend the following features be implemented for the initial version of manta comptime.

First, a minimal 1:1 version of the language should be supported with some caveats. Firstly, the interpreter 
should be consistent across compile targets and should be based on the `native` platform. This means
the interpreter should support file system access, system calls, and other OS specific interactions
in a cross platform way. This is already true of many interpreters like Python, or the Beam VM and so
there is plenty of quality prior art to reference when building this interpreted runtime. This will
cause the comptime and runtime diverge in meaningful ways, but consistent compilation behavior is worth
this drawback.

Additionally, the interpreted runtime should respect the target architecutre for types like `usize`
and `isize`. Again, this does introduce some differences that may be surprizing to developers. However,
given the goals of Manta and it's comptime system it's the better option. This will prevent unusual 
bugs with generated LUTs and make pre-computing important data easier to get right.

Second, conditional compilation is an important inclusion. Especially for more minor differences between
systems this will be critical in supporting cross-compilation and even has some additional benefits like
support for injecting version information.

Additional features like comptime types, AST access and other more powerful features should be rejected
for the comptime interpreters initial implementation. The complexity and risks outweigh of those features
outweight beneifits at this stage of the compilers design. It seems likely to me that a more targeted 
generics or macro system could provide the same benefits at a lower complexity cost. Given both macros 
and generics proposals are in flight, it seems appropriate to move forward without these features.

## Outstanding Questions

There are a few questions that are still outstanding in the proposal. The first is how to handle infinite
loops in comptime code. A compiler that hangs is real hit to developer expreience and there needs to
be real though put into how to handle this. Before this proposal is fully implementing this will need 
to be resolved.

Semantics of comp are another key thing to resolve. Right now most of the semantics are left vauge and
there are only a few examples to go on. We need much stronger plans to ensure implementation is smooth.

Specificing the execution order of comptime code is another key design question that's not directly
addressed. The current idea is to run the inteprater durring noding but that needs significant work 
before it can be fully atriculated here.

## Drawbacks of Comptime

While comptime has many benefits, there are undeniably drawbacks to incorporating comptime into manta. 
A key concern is that the compiler must now support both an interpreted runtime as well as a compiled 
runtime. Ensuring these two runtimes have predictably equivalent behavior is a non-trivial undertaking 
for a compiler, and yet an absolutely essential requirement to build developer trust.

There are also many tasks that can be accomplished by a suitably robust macro system / generic type system 
which cannot be easily reproduced by comptime. A simple example is that a macro system can fully
modify the AST of a program making it nearly as powerful as the compiler itself. Comptime could be made 
to support such capabilities but it may not be as obvious as a well designed template macro system. 
Additionally a simpler and more constrained comptime system can only support what the interpreter 
provides meaning this compatibility is not necessarily going to be supported. Similarly, generic systems 
like Rust's which track lifetimes and traits may be non-trivial to support in a comptime interpreter.

Comptime can also be just as difficult to debug as complex macros or generics. Any time there is some
process running at compile time that can make changes to the underlying code a mismatch between the
plain text of the code and the code's behavior is broken. There is now some intermediate system (in this
case the interpreted manta code) that is making less visible modifications to the system. This can be
powerful but requires meta-textual thinking and introduces some of the complexities of dynamic languages
into the mix.

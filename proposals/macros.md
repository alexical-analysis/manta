# Proposal: Macros

## Problem

This proposal is currently in progress. The problem space around macros in Manta has not yet been
fully defined. For initial context on why macros were considered in relation to Manta's meta-programming
needs, see the background section below.

## Goals

Simplifying syntax is an often touted benefit of comptime tools. Quite often there are simple repeated
patterns that show up in code. Sometimes it's fine to just have this code written out and using tools
like nvim macros can make this process less painful at time of writing. However
there are legitimate use cases where compressing syntactic patterns into a generic or comptime can
improve both legibility and usability. Manta seeks to support more readable code as a core goal.

---

## Background

Macro systems have been another popular option showing up in a huge variety of languages including Lisp,
C, and Rust. The variety of languages that support macros are a testament to their flexibility and power.
Languages like Rust even leverage macros along with their generic type system to create extremely flexible
syntax and types.

A key advantage of using macros is that, while they can be used to create generic types, their primary
benefit is to reduce repeated code patterns. The unless macro is the quintessential example of this.

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
such a system in a given language. Firstly macros are notoriously difficult both to debug and to provide
diagnostics around. Additionally, macro systems tend to quickly become confusing and can be abused quite
easily. Macros operate on the syntax of the language which while powerful, can lead to an explosion
in syntax for complex use cases.

---

## Alternatives Considered

### Option A: Syntax heavy macros

### Option B: String template macros

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

### Option C: Expression only macros

This version of a macro system would try to combat some of the complexity introduced by a macro system
by reducing it's capabilities down to only manipulation expressions. This still supports powerful meta-
programming capabilities but limits the system to be focued primarily on solving the specific problems
Manta needs it to.

---

## Recommendation:

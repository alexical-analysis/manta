# Manta AST Specification

This document describes the Abstract Syntax Tree (AST) structure for the Manta language, mapping language constructs to AST nodes.
It serves as a reference for understanding the relationships between the concrete syntax (defined in `grammar.ebnf`) and the AST representation.

## Overview

The Manta AST represents:
- **Top-level declarations**: Functions, types (structs/enums), imports, and constants
- **Statements**: Variable declarations, expressions, returns, defers, if, if/else, and match statements
 - **Expressions**: Literals, identifiers, binary operations, function calls, try/catch blocks 
- **Types**: Basic types, pointers, arrays, slices, and user-defined types

---

## 1. Top-Level Declarations

### 1.1 Function Declaration (`function_decl`)

**Grammar Reference** (from `grammar.ebnf`):
```ebnf
function_decl := 'fn' identifier '(' param_list? ')' return_type? block
param_list    := param (',' param)*
param         := identifier (type_spec)?
return_type   := type_spec
```

**AST Representation**:
A function declaration consists of:
- **Name**: identifier
- **Parameters**: list of `(name, optional_type)` pairs
- **Return type**: optional type specification
- **Body**: a block of statements

**Example** (from `language_spec.md`):
```manta
fn add(a, b i32) i32 {
    return a + b
}
```

**AST Structure**:
```
FunctionDecl {
    name: "add",
    params: [
        (name: "a", type: None),  // type inference
        (name: "b", type: Some(i32))
    ],
    return_type: Some(i32),
    body: Block { statements: [ReturnStmt(...)] }
}
```

---

### 1.2 Type Declaration (`type_decl`)

**Grammar Reference**:
```ebnf
type_decl := 'type' identifier ( 'enum' '{' enum_variant+ '}' | 'struct' '{' field_decl* '}' )
enum_variant  := identifier ( '(' type_spec ')' )? ; variants optionally carry a single type
field_decl    := identifier type_spec
```

#### 1.2.1 Enum Type

**Purpose**: Defines sum types with named variants, some of which can carry values.

**Example** (from `examples/option_match.manta`):
```manta
type Maybei32 enum {
    Some(i32)
    None
}

type ErrWrite enum {
    Ok
    IOError
}
```

**AST Structure**:
```
TypeDecl {
    name: "Maybei32",
    kind: Enum {
        variants: [
            EnumVariant { name: "Some", payload: Some(i32) },
            EnumVariant { name: "None", payload: None }
        ]
    }
}
```

#### 1.2.2 Struct Type

**Purpose**: Defines product types with named fields.

**Potential Example**:
```manta
type Point struct {
    x i32
    y i32
}
```

**AST Structure**:
```
TypeDecl {
    name: "Point",
    kind: Struct {
        fields: [
            StructField { name: "x", type: i32 },
            StructField { name: "y", type: i32 }
        ]
    }
}
```

---

### 1.3 Const Declaration (`const_decl`)

**Grammar Reference**:
```ebnf
const_decl := 'const' identifier '=' expression
```

**Purpose**: Define compile-time or module-level constants.

**AST Structure**:
```
ConstDecl {
    name: identifier,
    value: expression
}
```

---

### 1.4 Import Declaration (`import_decl`)

**Grammar Reference**:
```ebnf
import_decl := 'import' ( STRING | import_block )
import_block := '(' STRING+ ')'
```

**Purpose**: Include external modules or libraries.

**AST Structure**:
```
ImportDecl {
    modules: Vec<String>
}
```

---

## 2. Statements

### 2.1 Let Statement (`let_stmt` && `decl_stmt`)

**Grammar Reference**:
```ebnf
let_stmt  := 'let' identifier (type_spec)? '=' expression
decl_stmt := identifier ':=' expression
```
Note: both `let_stmt` and `decl_stmt` rules are covered by the same AST node.
    `decl_stmt` is a shorthand version of a full `let_stmt`.

**Purpose**: Declare and initialize a variable with an optional explicit type and value.

**Variants**:
`let a i32`       - a is initalized to the zero value of the `i32` type
`let a = 10`      - a is initalized to 10. The type is infered to be `i64`, the default type for int literals
`let a i16 = 128` - a is initalized to 128 with the type `i16`
`a := 10`         - a is initalized to 10. The type is infered to be `i64`, the default type for int literals

**Examples** (from `language_spec.md`):
```manta
x := 3    // type inference and explicit value
let x i32 // explicit type, value is the zero value of the given type
```

**AST Structure**:
```
LetStmt {
    name: identifier,
    type_annotation: Option<TypeSpec>,
    initializer: Option<expression>
}
```

---

### 2.2 Assignment Statement (`assign_stmt`)

**Grammar Reference**:
```ebnf
assign_stmt := identifier '=' expression
```

**Purpose**: Assignment statements update the value of an existing variable. Assignments are statements and do not evaluate to a value.

**Examples** (from `language_spec.md`):
```manta
x = 3 // x must be declared already
```

**AST Structure**:
```
AssignStmt {
    name: identifier,
    value: expression
}
```

---

### 2.3 Expression Statement (`expr_stmt`)

**Grammar Reference**:
```ebnf
expr_stmt := expression
```

**Purpose**: Execute an expression for side effects (function call, etc.). Assignments are statements and do not return values; see the Assignment Statement section under Statements.

**Examples**:
```manta
x = 5
print("hello")
```

**AST Structure**:
```
ExprStmt {
    expr: expression
}
```

---

### 2.4 Return Statement (`return_stmt`)

**Grammar Reference**:
```ebnf
return_stmt := 'return' expression?
```

**Purpose**: Exit the current function, optionally returning a value.

**Examples**:
```manta
return
return 42
return .None
```

**AST Structure**:
```
ReturnStmt {
    value: Option<expression>
}
```

---

### 2.5 Defer Statement (`defer_stmt`)

**Grammar Reference**:
```ebnf
defer_stmt := 'defer' block
```

**Purpose** (from `language_spec.md` section 2):
- Run cleanup code at the end of the current scope
- Works in LIFO order
- Runs on normal exit, early return, or try/catch unwinding
- Variables are captured by reference

**Example** (from `examples/defer_free.manta`):
```manta
fn write_and_cleanup(path str) ErrWrite {
    .Ok(f) := try open(path) catch { return .IOError }
    defer { close(f) }
    
    buf := new([]u8, 256)
    defer { free(buf) }
    
    .Ok(n) := try write(f, buf, 256) catch { return .IOError }
    return .Ok
}
```

**AST Structure**:
```
DeferStmt {
    block: Block
}
```

**Semantics**:
- Deferred blocks are pushed onto a stack when executed
- Stack is unwound in LIFO order on scope exit
- Captures are shallow (references to variables at defer-execution time)

---

### 2.6 Match Statement (`match_stmt`)

**Grammar Reference**:
```ebnf
match_stmt    := 'match' expression '{' match_arm+ '}'
match_arm     := pattern block
```

**Purpose**: Pattern match on sum type values to destructure and handle variants.

**Example** (from `examples/option_match.manta`):
```manta
match r {
    .Some(v) => { print("result: ", v) }
    .None => { print("no result") }
}
```

**AST Structure**:
```
MatchStmt {
    target: expression,
    arms: Vec<MatchArm>
}

MatchArm {
    pattern: Pattern,
    body: Block
}
```

---

## 3. Expressions

### 3.1 Literals

**Grammar Reference**:
```ebnf
literal := INTEGER | FLOAT | STRING | 'nil' | 'true' | 'false'
```

**Purpose**: Represent concrete values in the program.

**AST Structure**:
```
Expr::IntLiteral(i64)
Expr::FloatLiteral(f64)
Expr::StringLiteral(String)
Expr::BoolLiteral(bool)
Expr::NilLiteral
```

**Examples**:
```manta
42
3.14
"hello"
true
nil
```

---

### 3.2 Identifier

**Purpose**: Reference a variable or function name.

**AST Structure**:
```
Expr::Identifier(String)
```

**Examples**:
```manta
x
result
maybe_div
```

---

### 3.3 Binary Operations

**Purpose**: Combine two expressions with an operator.

**Supported Operators** (from `src/ast.rs`):
```
Add, Subtract, Multiply, Divide
```

**Additional operators** (from `language_spec.md` and grammar):
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`

**AST Structure**:
```
Expr::BinaryOp {
    left: Box<Expr>,
    operator: Operator,
    right: Box<Expr>
}

enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    // ... others
}
```

**Examples**:
```manta
a + b
10 / 2
b == 0
x && y
```

---

### 3.4 Unary Operations

**Grammar Reference**:
```ebnf
unary_expr := unary_op unary_expr | primary_expr
unary_op   := '-' | '!' | '&' | '*'
```

**Purpose**: Apply an operator to a single expression.

**Supported Operators**:
- `-`: Negation (arithmetic or logical)
- `!`: Logical NOT
- `&`: Address-of (borrow/reference)
- `*`: Dereference (pointer dereference)

**AST Structure**:
```
Expr::UnaryOp {
    operator: UnaryOp,
    operand: Box<Expr>
}

enum UnaryOp {
    Negate,      // -
    Not,         // !
    AddressOf,   // &
    Deref        // *
}
```

**Examples**:
```manta
-x           // negation
!condition   // logical NOT
&variable    // address-of (borrow)
*pointer     // dereference
```

---

### 3.5 Assignment (moved to Statements)

Assignment is a statement-level construct and does not belong in the expression grammar. See **2.2 Assignment Statement** for the grammar, examples and the `AssignStmt` AST node. The expression parser will not produce `Expr::Assignment` nodes; assignment is handled by the statement parser and produces `Stmt::Assign` / `AssignStmt` nodes.

---

### 3.6 Try Expression (`try_expr`)

**Grammar Reference**:
```ebnf
try_expr := 'try' primary_expr ('catch' '(' identifier ')' block)?
```

**Purpose** (from `language_spec.md`):
- Attempt to extract a success variant from a sum type
- If extraction fails, execute the `catch` block
- If no `catch` is provided, propagate the error upward

**Example** (from `examples/defer_free.manta`):
```manta
.Ok(f) := try open(path) catch { return .IOError }
```

**AST Structure**:
```
Expr::Try {
    expr: Box<Expr>,
    catch_handler: Option<CatchHandler>
}

CatchHandler {
    error_binding: Option<String>,  // the identifier in catch(identifier)
    body: Block
}
```

**Semantics**:
- Pattern matches on the result of `expr`
- If the pattern succeeds (e.g., `.Ok(value)`), the value is bound
- If it fails, the catch handler is executed
- If no catch handler, control flow propagates (return or continue unwinding)

---

### 3.7 Function Call (`call_expr`)

**Grammar Reference**:
```ebnf
call_expr     := identifier '(' argument_list? ')'
argument_list := expression (',' expression)*
```

**Purpose**: Invoke a function with arguments.

**AST Structure**:
```
Expr::Call {
    function: String,  // identifier name
    arguments: Vec<Expr>
}
```

**Examples**:
```manta
add(2, 3)
print("hello")
new(i32)
write(f, buf, 256)
```

---

### 3.8 Enum Variant Constructor

**Grammar Reference**:
```ebnf
identifier '(' type_spec ')'  ; enum variant constructor / pattern
```

**Purpose**: Construct a sum type value or pattern-match on a variant.

**AST Structure** (as an expression):
```
Expr::EnumVariant {
    type_name: Option<String>,  // e.g., "Maybei32" (optional, inferred)
    variant_name: String,       // e.g., "Some", "None"
    payload: Option<Box<Expr>>  // the value carried by this variant
}
```

**Examples**:
```manta
.Some(42)
.None
.Ok(f)
.IOError
```

---

### 3.9 Memory Operations

#### 3.9.1 New Expression (Allocation)

**Grammar & Semantics** (from `language_spec.md` section 3):
```ebnf
new(T)           // allocate single value, returns *T
new([N]T)        // allocate array of N elements, returns [N]T
new([]T, n)      // allocate slice with length n, returns []T
new([]T, n, cap) // allocate slice with length n and capacity cap, returns []T
```

**Purpose**: Allocate memory on the heap, zero-initialized.

**AST Structure**:
```
Expr::New {
    type_spec: TypeSpec
    len: Option<Box<Expr>
    cap: Option<Box<Expr>
}
```

**Examples**:
```manta
p := new(i32)
arr := new([10]i32)
s := new([]i32, 10)
```

#### 3.9.2 Free Expression (Deallocation)

**Grammar & Semantics** (from `language_spec.md` section 3):
```ebnf
free(ptr)
```

**Purpose**: Deallocate memory previously allocated by `new`. Behavior on double-free or invalid pointers is undefined.

**AST Structure**:
```
Expr::Free {
    expr: Box<Expr>
}
```

**Examples**:
```manta
free(p)
free(buf)
```

---

## 4. Types

### 4.1 Type Specifications

**Grammar Reference**:
```ebnf
type_spec := basic_type | pointer_type | array_type | slice_type | identifier
basic_type    := 'i8' | 'i16' | 'i32' | 'i64' | 'isize' | 'u8' | 'u16' | 'u32' | 'u64' | 'usize' | 'f32' | 'f64' | 'bool' | 'char' | 'str'
pointer_type  := '*' type_spec
array_type    := '[' INTEGER ']' type_spec
slice_type    := '[]' type_spec
```

**AST Structure**:
```
enum TypeSpec {
    Basic(BasicType),
    Pointer(Box<TypeSpec>),
    Array { size: usize, element_type: Box<TypeSpec> },
    Slice { element_type: Box<TypeSpec> },
    Custom(str)  // user-defined type name
}

enum BasicType {
    I8, I16, I32, I64, ISize,
    U8, U16, U32, U64, USize,
    F32, F64,
    Bool, Char, String
}
```

**Examples**:
```manta
i32              // basic integer
*i32             // pointer to i32
[10]i32          // array of 10 i32s
[]i32            // slice of i32s
Maybei32         // user-defined enum type
```

---

## 5. Patterns

### 5.1 Pattern Matching

**Grammar Reference**:
```ebnf
pattern := identifier? '.' identifier ( '(' identifier ')' )? | literal | identifier | '_'
```

**Purpose**: Destructure values in match arms and let bindings, binding names to parts of a value.

**AST Structure**:
```
enum Pattern {
    Literal(Literal),           // 42, "hello", true, nil
    Identifier(String),         // x, y, result
    Wildcard,                   // _
    EnumVariant {
        variant_name: String,   // "Some", "None", "Ok", "IOError"
        payload_pattern: Option<String> // x, y, v
    }
}
```

**Examples**:
```manta
.Some(v) // extract value from Some variant into v
.None    // match None variant (no payload)
_        // match anything, don't bind
x        // match anything, bind to x
42       // match literal 42
```

---

## 6. Blocks

### 6.1 Block Statement

**Grammar Reference**:
```ebnf
block := '{' statement* '}'
```

**Purpose**: Group statements into a scope.

**AST Structure**:
```
Block {
    statements: Vec<Statement>
}
```

**Examples**:
```manta
{
    let x := 5
    print(x)
}

defer { close(f) }

match r {
    .Some(v) => { print(v) }
    .None => { print("none") }
}
```

---

## 7. Program Structure

### 7.1 Program

**Grammar Reference**:
```ebnf
program := { top_level_decl }
```

**AST Structure**:
```
Program {
    declarations: Vec<TopLevelDecl>
}

enum TopLevelDecl {
    Function(FunctionDecl),
    Type(TypeDecl),
    Import(ImportDecl),
    Const(ConstDecl)
}
```

---

## Summary: Expression Hierarchy

```
Expression
├── Literal (int, float, string, bool, nil)
├── Identifier
├── BinaryOp (left op right)
├── UnaryOp (op operand)
├── Try (try expr catch handler)
├── Call (function(args))
├── EnumVariant (.Variant(payload))
├── New (new(type_spec))
└── Free (free(expr))

Statement
├── Let (let x type = expr / x := expr)
├── Assignment (x = expr)
├── Expression (expr)
├── Return (return expr?)
├── Defer (defer block)
├── If (if expr block)
├── Match (match expr { arms })
├── Type Declaration (type Ident struct struct_block / type Ident enum enum_block)
├── Const Declaration (const Ident = expr)
├── Import Declaration (import "module" / import ("mod1" "mod2"))
└── Block ({ statements })
```

---

## References

- **Language Spec**: `docs/language_spec.md` - Detailed feature descriptions and semantics
- **Grammar**: `docs/grammar.ebnf` - Formal grammar definition
- **Examples**: `examples/` - Concrete code samples
- **Current AST**: `src/ast.rs` - Current Rust AST implementation

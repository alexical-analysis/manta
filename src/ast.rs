/// Top-level declarations in a Manta program
pub enum TopLevelDecl {
    Function(FunctionDecl),
    Type(TypeDecl),
    Const(ConstDecl),
    Import(ImportDecl),
}

/// Function declaration
///
/// Example:
/// ```manta
/// fn add(a, b i32) i32 {
///     return a + b
/// }
/// ```
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeSpec>,
    pub body: Block,
}

/// Function parameter
pub struct Parameter {
    pub name: String,
    pub type_spec: Option<TypeSpec>,
}

/// Type declaration (struct or enum)
pub struct TypeDecl {
    pub name: String,
    pub kind: TypeKind,
}

pub enum TypeKind {
    Struct(StructType),
    Enum(EnumType),
}

/// Struct type with named fields
pub struct StructType {
    pub fields: Vec<StructField>,
}

pub struct StructField {
    pub name: String,
    pub type_spec: TypeSpec,
}

/// Enum type with named variants
pub struct EnumType {
    pub variants: Vec<EnumVariant>,
}

pub struct EnumVariant {
    pub name: String,
    pub payload: Option<TypeSpec>,
}

/// Const declaration
///
/// Example:
/// ```manta
/// const PI = 3.14159
/// ```
pub struct ConstDecl {
    pub name: String,
    pub value: Expr,
}

/// Import declaration
///
/// Example:
/// ```manta
/// import "math"
/// import ("std", "io")
/// ```
pub struct ImportDecl {
    pub modules: Vec<String>,
}

/// Type specification
#[derive(Debug)]
pub enum TypeSpec {
    Int32,
    Int16,
    Int8,
    Int64,
    UInt32,
    UInt16,
    UInt8,
    UInt64,
    Float64,
    Float32,
    String,
    Bool,
    Void,
    // User-defined types
    Named(String),
    // Composite types
    Pointer(Box<TypeSpec>),
    Array(Box<TypeSpec>, usize),
    Slice(Box<TypeSpec>),
}

/// A block of statements
#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

/// Statements in a block
#[derive(Debug)]
pub enum Stmt {
    Let(LetStmt),
    Assign(AssignStmt),
    Expr(ExprStmt),
    Return(ReturnStmt),
    Defer(DeferStmt),
    Match(MatchStmt),
}

#[derive(Debug)]
pub struct LetStmt {
    pub name: String,
    pub type_annotation: Option<TypeSpec>,
    pub initializer: Option<Expr>,
}

#[derive(Debug)]
pub struct AssignStmt {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug)]
pub struct DeferStmt {
    pub block: Block,
}

#[derive(Debug)]
pub struct MatchStmt {
    pub target: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
}

#[derive(Debug)]
pub enum Pattern {
    EnumVariant {
        name: String,
        payload_binding: Option<String>,
    },

    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    NilLiteral,

    Default, // _ pattern
}

/// Expressions
#[derive(Debug)]
pub enum Expr {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    NilLiteral,

    // Identifiers and references
    Identifier(String),
    EnumConstructor {
        type_name: Option<String>,
        variant: String,
        payload: Option<Box<Expr>>,
    },

    // Operations
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),

    // Function call
    Call(FunctionCall),

    // Other expressions
    Assignment(Assignment),

    // Indexing and field access
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },

    FieldAccess {
        target: Box<Expr>,
        field: String,
    },

    // Memory operations
    New(NewExpr),
    Free(Box<Expr>),

    // Try/Catch extraction
    Try(TryExpr),

    // Casts
    Cast {
        expr: Box<Expr>,
        target_type: TypeSpec,
    },
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub operand: Box<Expr>,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum UnaryOp {
    Not,
    Negate,
    Positive,
    Dereference,
    AddressOf,
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct Assignment {
    // target is any l-value expression (identifier, deref, index, field access)
    pub target: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug)]
pub struct TryExpr {
    pub expr: Box<Expr>,
    pub catch: Option<CatchHandler>,
}

#[derive(Debug)]
pub struct CatchHandler {
    pub error_binding: Option<String>,
    pub body: Block,
}

/// Valid allocation expressions include new(T), new([N]T), new([]T, len) etc.
#[derive(Debug)]
pub struct NewExpr {
    pub type_spec: TypeSpec,
    pub len: Option<Box<Expr>>,
    pub cap: Option<Box<Expr>>,
}

use serde::{Deserialize, Serialize};

/// Top-level declarations in a Manta program
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Decl {
    Function(FunctionDecl),
    Type(TypeDecl),
    Const(ConstDecl),
    Use(UseDecl),
    Mod(ModDecl),
}

/// Function declaration
///
/// Example:
/// ```manta
/// fn add(a, b i32) i32 {
///     return a + b
/// }
/// ```
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeSpec>,
    pub body: BlockStmt,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct TypeDecl {
    pub name: IdentifierExpr,
    pub type_spec: TypeSpec,
}

/// Function parameter
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub type_spec: TypeSpec,
}

/// Const declaration
///
/// Example:
/// ```manta
/// const PI = 3.14159
/// ```
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ConstDecl {
    pub name: String,
    pub value: Expr,
}

/// Use declaration
///
/// Example:
/// ```manta
/// import "math"
/// import ("std", "io")
/// ```
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct UseDecl {
    pub modules: Vec<String>,
}

/// Mod declaration
///
/// Example:
/// ```manta
/// mod main
/// ```
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct ModDecl {
    pub name: String,
}

/// Type specification
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum TypeSpec {
    Int32,
    Int16,
    Int8,
    Int64,
    UInt32,
    UInt16,
    UInt8,
    UInt64,
    Float32,
    Float64,
    String,
    Bool,
    // User-defined types
    Named {
        module: Option<String>,
        name: String,
    },
    // Composite types
    Pointer(Box<TypeSpec>),
    Slice(Box<TypeSpec>),
    Array(ArrayType),
    Struct(StructType),
    Enum(EnumType),
}

/// MetaType
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct MetaTypeExpr {
    pub type_spec: TypeSpec,
}

/// Array type with size
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct ArrayType {
    pub type_spec: Box<TypeSpec>,
    pub size: usize,
}

/// Struct type with named fields
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructType {
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructField {
    pub name: String,
    pub type_spec: TypeSpec,
}

/// Enum type with named variants
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct EnumType {
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: String,
    pub payload: Option<TypeSpec>,
}

/// A block of statements
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IfStmt {
    pub check: Box<Expr>,
    pub success: BlockStmt,
    pub fail: Option<BlockStmt>,
}

/// Statements in a block
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    Let(LetStmt),
    Assign(AssignStmt),
    Expr(ExprStmt),
    Return(ReturnStmt),
    Defer(DeferStmt),
    Match(MatchStmt),
    Block(BlockStmt),
    If(IfStmt),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct LetStmt {
    pub pattern: Pattern,
    pub value: Expr,
    pub or_binding: Option<Box<IdentifierExpr>>,
    pub except: Option<BlockStmt>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct AssignStmt {
    pub lvalue: Expr,
    pub rvalue: Expr,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct DeferStmt {
    pub block: BlockStmt,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct MatchStmt {
    pub target: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: BlockStmt,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Pattern {
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    FloatLiteral(f64),

    TypeSpec(TypeSpec),

    Payload(PayloadPat),
    ModuleAccess(ModuleAccesPat),
    DotAccess(DotAccessPat),

    Identifier(IdentifierPat),
    Default, // the _ pattern
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ModuleAccesPat {
    pub module: Box<IdentifierPat>,
    pub pat: Box<Pattern>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct DotAccessPat {
    pub target: Option<Box<Pattern>>,
    pub field: IdentifierPat,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct PayloadPat {
    pub pat: Box<Pattern>,
    pub payload: String,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct EnumVariantPat {
    pub type_name: Option<String>,
    pub name: String,
    pub payload_binding: Option<String>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IdentifierPat {
    pub name: String,
}

/// Expressions
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    NilLiteral,

    // Identifiers and references
    Identifier(IdentifierExpr),
    EnumConstructor(EnumConstructorExpr),

    // Operations
    Binary(BinaryExpr),
    Unary(UnaryExpr),

    // Function call
    Call(CallExpr),

    // Other expressions
    Assignment(AssignmentExpr),

    // Indexing and field access
    Index(IndexExpr),

    // Accessing a field for a struct or enum
    DotAccess(DotAccessExpr),

    // Accessing a member of a module
    ModuleAccess(ModuleAccessExpr),

    // Mete Type expression
    MetaType(MetaTypeExpr),

    // Memory operations
    New(NewExpr),
    Free(FreeExpr),

    // Type casting expressions
    Cast(CastExpr),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IdentifierExpr {
    pub name: String,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct EnumConstructorExpr {
    // TODO: should these be IdentifierExpr?
    type_name: Option<String>,
    variant: String,
    payload: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
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
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub operand: Box<Expr>,
}

#[derive(PartialEq, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    Negate,
    Positive,
    Dereference,
    AddressOf,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct CallExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IndexExpr {
    pub target: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct DotAccessExpr {
    // this is an option because this can be infered in some contexts
    pub target: Option<Box<Expr>>,
    pub field: Box<IdentifierExpr>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ModuleAccessExpr {
    pub module: Box<IdentifierExpr>,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct AssignmentExpr {
    // target is any l-value expression (identifier, deref, index, field access)
    pub target: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct CastExpr {
    expr: Box<Expr>,
    target_type: TypeSpec,
}

/// Valid allocation expressions include new(T), new([N]T), new([]T, len) etc.
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct NewExpr {
    pub type_spec: TypeSpec,
    pub len: Option<Box<Expr>>,
    pub cap: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FreeExpr {
    pub expr: Box<Expr>,
}

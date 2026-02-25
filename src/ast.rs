use serde::Serialize;

use crate::parser::lexer::{SourceID, Token};
use crate::str_store::StrID;

/// Top-level declarations in a Manta program
#[derive(Debug, PartialEq, Serialize)]
pub enum Decl {
    Function(FunctionDecl),
    Type(TypeDecl),
    Const(ConstDecl),
    Var(VarDecl),
    Use(UseDecl),
    Mod(ModDecl),
    Invalid,
}

/// Function declaration
///
/// Example:
/// ```manta
/// fn add(a, b i32) i32 {
///     return a + b
/// }
/// ```
#[derive(Debug, PartialEq, Serialize)]
pub struct FunctionDecl {
    pub id: SourceID,
    pub name: StrID,
    pub params: Vec<Parameter>,
    pub body: BlockStmt,
    pub function_type: FunctionType,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct TypeDecl {
    pub id: SourceID,
    pub name: StrID,
    pub type_spec: TypeSpec,
}

/// Function parameter
#[derive(Debug, PartialEq, Serialize)]
pub struct Parameter {
    pub id: SourceID,
    pub name: StrID,
}

/// Const declaration
///
/// Example:
/// ```manta
/// const PI = 3.14159
/// ```
#[derive(Debug, PartialEq, Serialize)]
pub struct ConstDecl {
    pub id: SourceID,
    pub name: StrID,
    pub value: Expr,
}

/// Var declaration
///
/// Example:
/// ```manta
/// var status = "Ok"
/// ```
#[derive(Debug, PartialEq, Serialize)]
pub struct VarDecl {
    pub id: SourceID,
    pub name: StrID,
    pub value: Expr,
}

/// Use declaration
///
/// Example:
/// ```manta
/// import "math"
/// import ("std", "io")
/// ```
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct UseDecl {
    pub modules: Vec<StrID>,
}

/// Mod declaration
///
/// Example:
/// ```manta
/// mod main
/// ```
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ModDecl {
    pub name: StrID,
}

/// Type specification
#[derive(Debug, Clone, PartialEq, Serialize)]
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
    Named { module: Option<StrID>, name: StrID },
    // Composite types
    Pointer(Box<TypeSpec>),
    Slice(Box<TypeSpec>),
    Array(ArrayType),
    Struct(StructType),
    Enum(EnumType),
    Function(FunctionType),
    // UnsafePtr is the intermediary between types used by alloc
    UnsafePtr,
    // Panic is the type used for expressions that panic
    Panic,
}

/// MetaType
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct MetaTypeExpr {
    pub type_spec: TypeSpec,
}

/// Array type with size
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ArrayType {
    pub type_spec: Box<TypeSpec>,
    pub size: usize,
}

/// Struct type with named fields
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct StructType {
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct StructField {
    pub name: StrID,
    pub type_spec: TypeSpec,
}

/// Enum type with named variants
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct EnumType {
    pub variants: Vec<EnumVariant>,
}

/// Function types with arguments
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FunctionType {
    pub params: Vec<TypeSpec>,
    pub return_type: Option<Box<TypeSpec>>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct EnumVariant {
    pub name: StrID,
    pub payload: Option<TypeSpec>,
}

/// A block of statements
#[derive(Debug, PartialEq, Serialize)]
pub struct BlockStmt {
    pub id: SourceID,
    pub statements: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct IfStmt {
    pub check: Box<Expr>,
    pub success: BlockStmt,
    pub fail: Option<BlockStmt>,
}

/// Statements in a block
#[derive(Debug, PartialEq, Serialize)]
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

#[derive(Debug, PartialEq, Serialize)]
pub struct LetStmt {
    pub pattern: Pattern,
    pub value: Expr,
    pub except: LetExcept,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum LetExcept {
    Or {
        token: Token,
        binding: Option<StrID>,
        body: BlockStmt,
    },
    Wrap(Expr),
    Panic,
    None,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct AssignStmt {
    pub lvalue: Expr,
    pub rvalue: Expr,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct DeferStmt {
    pub block: BlockStmt,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct MatchStmt {
    pub target: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct MatchArm {
    pub token: Token,
    pub pattern: Pattern,
    pub body: BlockStmt,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Pattern {
    IntLiteral(i64),
    StringLiteral(StrID),
    BoolLiteral(bool),
    FloatLiteral(f64),

    TypeSpec(TypeSpec),

    Payload(PayloadPat),
    ModuleAccess(ModuleAccesPat),
    DotAccess(DotAccessPat),

    // TODO: should this just wrap a StrID?
    Identifier(IdentifierPat),
    Default, // the _ pattern
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ModuleAccesPat {
    pub module: Box<IdentifierPat>,
    pub pat: Box<Pattern>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct DotAccessPat {
    pub target: Option<Box<Pattern>>,
    pub field: IdentifierPat,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct PayloadPat {
    pub pat: Box<Pattern>,
    pub payload: IdentifierPat,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct EnumVariantPat {
    pub type_name: Option<StrID>,
    pub name: StrID,
    pub payload_binding: Option<StrID>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct IdentifierPat {
    pub token: Token,
    pub name: StrID,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum Expr {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(StrID),
    BoolLiteral(bool),

    // Identifiers and references
    Identifier(IdentifierExpr),

    // Operations
    Binary(BinaryExpr),
    Unary(UnaryExpr),

    // Function call
    Call(CallExpr),

    // Indexing for an expression
    Index(IndexExpr),

    // Range expressions like 1:3
    Range(RangeExpr),

    // Accessing a field for a struct or enum
    DotAccess(DotAccessExpr),

    // Accessing a member of a module
    ModuleAccess(ModuleAccessExpr),

    // Mete Type expression
    MetaType(MetaTypeExpr),

    // Memory operations
    Alloc(AllocExpr),
    Free(FreeExpr),
}

#[derive(Debug, PartialEq, Serialize)]
pub struct IdentifierExpr {
    pub token: Token,
    pub name: StrID,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize)]
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

#[derive(Debug, PartialEq, Serialize)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub operand: Box<Expr>,
}

#[derive(PartialEq, Debug, Clone, Copy, Serialize)]
pub enum UnaryOp {
    Not,
    Negate,
    Positive,
    Dereference,
    AddressOf,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct CallExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct IndexExpr {
    pub target: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct RangeExpr {
    pub start: Box<Expr>,
    pub end: Box<Expr>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct DotAccessExpr {
    // this is an option because this can be infered in some contexts
    pub target: Option<Box<Expr>>,
    pub field: StrID,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct ModuleAccessExpr {
    pub module: StrID,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct CastExpr {
    expr: Box<Expr>,
    target_type: TypeSpec,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct AllocExpr {
    pub meta_type: Box<Expr>,
    pub options: Vec<Expr>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct FreeExpr {
    pub expr: Box<Expr>,
}

use serde::Serialize;

use crate::parser::lexer::SourceID;
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
    Named(NamedType),
    // Composite types
    Pointer(Box<TypeSpec>),
    Slice(Box<TypeSpec>),
    Array(ArrayType),
    Struct(StructType),
    Enum(EnumType),
    Function(FunctionType),
    Unit,
}

/// Used defined named type
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct NamedType {
    pub id: SourceID,
    pub module: Option<StrID>,
    pub name: StrID,
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
    pub fields: Vec<StructTypeField>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct StructTypeField {
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
    pub return_type: Box<TypeSpec>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct EnumVariant {
    pub name: StrID,
    pub payload: Option<TypeSpec>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct StructConstructor {
    pub type_spec: TypeSpec,
    pub fields: Vec<StructValueField>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct StructValueField {
    pub name: StrID,
    pub value: Box<Expr>,
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

#[derive(Debug, PartialEq, Serialize)]
pub struct LoopStmt {
    pub body: BlockStmt,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct WhileStmt {
    pub check: Box<Expr>,
    pub body: BlockStmt,
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
    Loop(LoopStmt),
    While(WhileStmt),
    Break,
    Continue,
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
        id: SourceID,
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
    pub id: SourceID,
    pub pattern: Pattern,
    pub body: BlockStmt,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Pattern {
    IntLiteral(i64),
    StringLiteral(StrID),
    BoolLiteral(bool),
    FloatLiteral(f64),
    TypeSpec(TypeSpecPat),
    EnumVariant(EnumVariantPat),
    ModuleIdentifier(ModuleIdentifierPat),
    Identifier(IdentifierPat),
    Default,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Payload {
    Some(StrID),
    None,
    Default, // the _ identifier
}

impl Payload {
    pub fn is_none(&self) -> bool {
        match self {
            Payload::Some(_) => false,
            Payload::Default => false,
            Payload::None => true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct TypeSpecPat {
    pub id: SourceID,
    pub type_spec: TypeSpec,
    pub payload: Payload,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct EnumVariantPat {
    pub id: SourceID,
    pub enum_name: Option<IdentifierExpr>,
    pub variant: StrID,
    pub payload: Payload,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ModuleIdentifierPat {
    pub id: SourceID,
    pub module: StrID,
    pub name: StrID,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct IdentifierPat {
    pub id: SourceID,
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

    // struct constructors
    StructConstructor(StructConstructor),

    // Indexing for an expression
    Index(IndexExpr),

    // Range expressions like 1:3
    Range(RangeExpr),

    // Accessing a field for a struct or enum
    DotAccess(DotAccessExpr),

    // Mete Type expression
    MetaType(MetaTypeExpr),

    // Memory operations
    Alloc(AllocExpr),
    Free(FreeExpr),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct IdentifierExpr {
    pub id: SourceID,
    pub module: Option<StrID>,
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

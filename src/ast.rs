/// Top-level declarations in a Manta program
#[derive(Debug, PartialEq)]
pub enum Decl {
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
#[derive(Debug, PartialEq)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeSpec>,
    pub body: BlockStmt,
}

/// Function parameter
#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_spec: TypeSpec,
}

/// Type declaration (struct or enum)
#[derive(Debug, PartialEq)]
pub struct TypeDecl {
    pub name: String,
    pub kind: TypeKind,
}

#[derive(Debug, PartialEq)]
pub enum TypeKind {
    Struct(StructType),
    Enum(EnumType),
}

/// Struct type with named fields
#[derive(Debug, PartialEq)]
pub struct StructType {
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq)]
pub struct StructField {
    pub name: String,
    pub type_spec: TypeSpec,
}

/// Enum type with named variants
#[derive(Debug, PartialEq)]
pub struct EnumType {
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq, Clone)]
pub struct ImportDecl {
    pub modules: Vec<String>,
}

/// Type specification
#[derive(Debug, PartialEq, Clone)]
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
    // User-defined types
    Named(String),
    // Composite types
    Pointer(Box<TypeSpec>),
    Array(Box<TypeSpec>, usize),
    Slice(Box<TypeSpec>),
}

/// A block of statements
#[derive(Debug, PartialEq)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct TryStmt {
    pub pattern_enum: Option<Box<IdentifierExpr>>,
    pub pattern_variant: Box<IdentifierExpr>,
    pub decl: Option<Box<IdentifierExpr>>,
    pub expr: Box<Expr>,
    pub catch_binding: Option<Box<IdentifierExpr>>,
    pub catch_body: Option<Box<BlockStmt>>,
}

#[derive(Debug, PartialEq)]
pub struct IfStmt {
    pub check: Box<Expr>,
    pub success: BlockStmt,
    pub fail: Option<BlockStmt>,
}

/// Statements in a block
#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let(LetStmt),
    ShortLet(ShortLetStmt),
    Assign(AssignStmt),
    Expr(ExprStmt),
    Return(ReturnStmt),
    Defer(DeferStmt),
    Match(MatchStmt),
    Block(BlockStmt),
    Try(TryStmt),
    If(IfStmt),
}

#[derive(Debug, PartialEq)]
pub struct LetStmt {
    pub name: String,
    pub type_annotation: Option<TypeSpec>,
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct ShortLetStmt {
    pub name: IdentifierExpr,
    pub value: Expr,
}

#[derive(Debug, PartialEq)]
pub struct AssignStmt {
    pub lvalue: Expr,
    pub rvalue: Expr,
}

#[derive(Debug, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct DeferStmt {
    pub block: BlockStmt,
}

#[derive(Debug, PartialEq)]
pub struct MatchStmt {
    pub target: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: BlockStmt,
}

#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
    FieldAccess(FieldAccessExpr),

    // Memory operations
    New(NewExpr),
    Free(FreeExpr),

    // Type casting expressions
    Cast(CastExpr),
}

#[derive(Debug, PartialEq)]
pub struct IdentifierExpr {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct EnumConstructorExpr {
    // TODO: should these be IdentifierExpr?
    type_name: Option<String>,
    variant: String,
    payload: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq)]
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
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct CallExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct IndexExpr {
    pub target: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct FieldAccessExpr {
    // this is an option because this can be infered in some contexts
    pub target: Option<Box<Expr>>,
    pub field: Box<IdentifierExpr>,
}

#[derive(Debug, PartialEq)]
pub struct AssignmentExpr {
    // target is any l-value expression (identifier, deref, index, field access)
    pub target: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct CastExpr {
    expr: Box<Expr>,
    target_type: TypeSpec,
}

/// Valid allocation expressions include new(T), new([N]T), new([]T, len) etc.
#[derive(Debug, PartialEq)]
pub struct NewExpr {
    pub type_spec: TypeSpec,
    pub len: Option<Box<Expr>>,
    pub cap: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq)]
pub struct FreeExpr {
    pub expr: Box<Expr>,
}

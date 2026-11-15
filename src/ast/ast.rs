use crate::compiler::str_store::MStr;
use crate::index_vec::Indexer;
use crate::types::unchecked_ty::UncheckedTy;

/// Decl is the key type used to index a store of manta declarations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Decl(u32);

impl Indexer for Decl {
    fn index(&self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for Decl {
    fn from(value: usize) -> Self {
        Decl(value as u32)
    }
}

/// Top-level declarations in a Manta program
pub enum DeclValue {
    Func(FuncDecl),
    Ty(TyDecl),
    Const(ConstDecl),
    Let(LetDecl),
    Use(UseDecl),
    Import(ImportDecl),
    Mod(ModDecl),
    Invalid(&'static str),
}

/// Function declaration
///
/// Example:
/// ```manta
/// fn add(a, b, i32) i32 {
///     return a + b
/// }
pub struct FuncDecl {
    pub public: bool,
    pub name: MStr,
    pub params: Vec<MStr>,
    pub body: BlockStmt,
    pub ty: UncheckedTy,
}

/// User defined type declaration
///
/// Example:
/// ```manta
/// type Vec2 struct {
///     x: i32,
///     y: i32,
/// }
/// ```
pub struct TyDecl {
    pub public: bool,
    pub name: MStr,
    pub ty: UncheckedTy,
}

/// Top level constant declaration
///
/// Example:
/// ```manta
/// const PI = 3.1415
/// ```
pub struct ConstDecl {
    pub public: bool,
    pub name: MStr,
    pub value: Expr,
}

/// Top level let declaration
///
/// Example:
/// ```manta
/// pub let client = NewClient()
/// ```
pub struct LetDecl {
    pub public: bool,
    pub pattern: Pattern,
    pub value: Expr,
    pub except: LetExcept,
}

/// The final, optional portion of a let stmt
pub enum LetExcept {
    Or {
        binding: Option<MStr>,
        body: BlockStmt,
    },
    Wrap(Expr),
    Panic,
    None,
}

/// The module use declaration
///
/// Example:
/// ```manta
/// use (
///     "math"
///     "string"
/// )
/// ```
pub struct UseDecl {
    // This contains Decl types instead of Import decls to account for malformed import decls
    pub deps: Vec<Decl>,
}

/// An import line in a use declaration
#[derive(Copy, Clone)]
pub struct ImportDecl {
    pub name: MStr,
    pub import_path: MStr,
    /// If no alias is provided the alias is the same as the name
    pub alias: MStr,
}

/// Module declaration at the top of every file
///
/// Example:
/// ```manta
/// mod main
/// ```
pub struct ModDecl {
    pub name: MStr,
}

/// Stmt is the key type used to index a store of manta statements
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Stmt(u32);

impl Indexer for Stmt {
    fn index(&self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for Stmt {
    fn from(value: usize) -> Self {
        Stmt(value as u32)
    }
}

/// A manta statement
pub enum StmtValue {
    Let(LetStmt),
    Assign(AssignStmt),
    ExprStmt(ExprValue),
    Return(ReturnStmt),
    Defer(DeferStmt),
    Match(MatchStmt),
    Block(BlockStmt),
    If(IfStmt),
    Loop(LoopStmt),
    While(WhileStmt),
    For(ForStmt),
    Break,
    Continue,
    Invalid(&'static str),
}

/// A let statement
///
/// Example:
/// ```manta
/// let a = 10
/// let b = get_value()!
/// ```
pub struct LetStmt {
    pub mutable: bool,
    pub pattern: Pattern,
    pub value: Expr,
    pub except: LetExcept,
}

/// An asignment of a value to an identifier
///
/// Example:
/// ```manta
/// a = 10
/// ```
pub struct AssignStmt {
    pub name: MStr,
    pub value: Expr,
}

/// A return statement
///
/// Example:
/// ```manta
/// return "ok"
/// ```
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

/// A defer statement
///
/// Example:
/// ```manta
/// defer { ... }
/// ```
pub struct DeferStmt {
    pub block: BlockStmt,
}

/// A match statement
///
/// Example:
/// ```manta
/// match try_thing() {
///     .Ok { fmt::print("ok") }
///     .Err { fmt::print("err") }
/// }
pub struct MatchStmt {
    pub target: Expr,
    pub arms: Vec<MatchArm>,
}

/// A single match arm in a match statement
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: BlockStmt,
}

/// A block of statements
///
/// Example:
/// ```manta
/// {
///     fmt::print("hello")
///     let sum = 10 + 20
/// }
/// ```
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

/// An if statement
///
/// Example:
/// ```manta
/// if success {
///     fmt::print("success")
/// }
pub struct IfStmt {
    pub check: Expr,
    pub success: BlockStmt,
    pub fail: Option<BlockStmt>,
}

/// An infinite loop statement
///
/// Example:
/// ```manta
/// loop {
///     fmt::print("loop")
/// }
/// ```
pub struct LoopStmt {
    pub body: BlockStmt,
}

/// A while loop statement
///
/// Example:
/// ```manta
/// while a < 10 {
///     a++
/// }
/// ```
pub struct WhileStmt {
    pub check: Expr,
    pub body: BlockStmt,
}

/// A for loop statement
///
/// Example:
/// ```manta
/// for i = 0; i < 10; i++ {
///     fmt::println("i:", i)
/// }
/// ```
pub struct ForStmt {
    pub prefix_stmt: Stmt,
    pub check: Expr,
    pub postfix_stmt: Stmt,
    pub body: BlockStmt,
}

/// Expr is the key type used to index a store of manta expressions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Expr(u32);

impl Indexer for Expr {
    fn index(&self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for Expr {
    fn from(value: usize) -> Self {
        Expr(value as u32)
    }
}

/// A manta expression
pub enum ExprValue {
    IntLiteral(i64),
    UIntLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(MStr),
    BoolLiteral(bool),
    Identifier(IdentifierExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Call(CallExpr),
    StructConstructor(StructConstructorExpr),
    Index(IndexExpr),
    Range(RangeExpr),
    DotAccess(DotAccesExpr),
    MetaType(MetaTypeExpr),
    Alloc(AllocExpr),
    Free(FreeExpr),
    Invalid(&'static str),
}

/// A manta identifier
///
/// Example:
/// ```manta
/// my_var
/// ```
pub struct IdentifierExpr {
    pub module: Option<MStr>,
    pub name: MStr,
}

/// A binary expression
///
/// Example:
/// ```manta
/// a + b
/// c == d
/// 10 \ 2
/// ```
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: BinaryOp,
    pub right: Expr,
}

/// All the valid binary epxressions
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

/// A unary expression
///
/// Example:
/// ```manta
/// -a
/// !ok
/// ```
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub operand: Expr,
}

/// All the valid unary expressions
pub enum UnaryOp {
    Not,
    Negative,
    Positive,
    Dereference,
    AddressOf,
}

/// A call to a function
///
/// Example:
/// ```manta
/// do(task)
/// ```
pub struct CallExpr {
    pub func: Expr,
    pub args: Vec<Expr>,
}

/// Construct a new instance of a struct
///
/// Example:
/// ```manta
/// Vec2 { x: 10, y: 20 }
/// ```
pub struct StructConstructorExpr {
    pub type_spec: UncheckedTy,
    pub fields: Vec<StructConstructorField>,
}

/// An individual field in a struct constructor
pub struct StructConstructorField {
    pub name: MStr,
    pub value: Expr,
}

/// An index expression
///
/// Example:
/// ```manta
/// my_data[10]
/// ```
pub struct IndexExpr {
    pub collection: Expr,
    pub index: Expr,
}

pub enum RangeOp {
    Inclusive,
    Exclusive,
}

/// A range expression
///
/// Example:
/// ```manta
/// 0..<9
/// 1..=100
/// ```
pub struct RangeExpr {
    pub start: Expr,
    pub op: RangeOp,
    pub end: Expr,
}

/// A dot access expression
///
/// Example:
/// ```manta
/// person.name
/// ```
pub struct DotAccesExpr {
    // this is an option becuase in some cases the target can be inferred, for example enum variants
    pub target: Option<Expr>,
    pub field: MStr,
}

/// A meta type expression
///
/// Example:
/// ```manta
/// @i32
/// ```
pub struct MetaTypeExpr {
    pub type_spec: UncheckedTy,
}

/// A call to the manta allocator
///
/// Example:
/// ```manta
/// alloc(@i32)
/// ```
pub struct AllocExpr {
    pub meta_type: Expr,
    pub options: Vec<Expr>,
}

/// A call to the manta free expression
///
/// Example:
/// ```manta
/// free(my_ptr)
/// ```
pub struct FreeExpr {
    pub expr: Expr,
}

/// A pattern match for a match or let statement
pub enum Pattern {
    IntLiteral(i64),
    UIntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(MStr),
    Identifier(MStr),
    TypeSpec(TypeSpecPat),
    EnumVariant(EnumVariantPat),
    Default,
}

/// A matching patter for type specs
///
/// Example:
/// ```manta
/// let i32(i)= alloc(@i32)!
/// ```
pub struct TypeSpecPat {
    pub type_spec: UncheckedTy,
    pub payload: Payload,
}

/// Payload the payload for a match pattern
pub enum Payload {
    Some(MStr),
    None,
    Default,
}

/// The pattern for matching an enum variant
///
/// Example:
/// ```manta
/// let Result.Ok(got) = test()!
/// ```
pub struct EnumVariantPat {
    pub name: Option<MStr>,
    pub variant: MStr,
    pub payload: Payload,
}

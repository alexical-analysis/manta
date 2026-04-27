use serde::de::Error;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::ast::{BinaryOp, UnaryOp};
use crate::compiler::ModuleID;
use crate::str_store::StrID;

// High-level Intermediate Representation (HIR)
// This is a desugared, simplified version of the AST with a single node type.

/// NodeID is the unique identifier for a gien node in the HIR tree
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct NodeID {
    module_id: ModuleID,
    id: u32,
}

impl Serialize for NodeID {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&format!("{}:{}", self.module_id.id(), self.id))
    }
}

impl<'de> Deserialize<'de> for NodeID {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let raw = String::deserialize(d)?;
        let (module_str, id_str) = raw
            .split_once(':')
            .ok_or_else(|| Error::custom(format!("invalid NodeID: {raw}")))?;
        let module_id = module_str.parse::<u32>().map_err(Error::custom)?;
        let id = id_str.parse::<u32>().map_err(Error::custom)?;
        Ok(NodeID::new(ModuleID::new(module_id), id))
    }
}

impl NodeID {
    pub fn new(module_id: ModuleID, id: u32) -> Self {
        NodeID { module_id, id }
    }

    pub fn module_id(&self) -> ModuleID {
        self.module_id
    }

    pub fn id(&self) -> u32 {
        self.id
    }
}

/// A single node type that can represent any construct in the HIR
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum Node {
    Invalid,
    FunctionDecl {
        public: bool,
        // ident is always an identifier node
        ident: NodeID,
        // params are just VarDecl nodes
        params: Vec<NodeID>,
        body: NodeID,
    },
    TypeDecl {
        // ident is always an identifier node
        ident: NodeID,
    },
    Block {
        statements: Vec<NodeID>,
    },
    VarDecl {
        public: bool,
        // ident is always an identifier node
        ident: NodeID,
        // no value here because HIR declares variables first and then
        // assigns a value in a later node
    },
    Assign {
        target: NodeID,
        value: NodeID,
    },
    Return {
        value: Option<NodeID>,
    },
    Defer {
        block: NodeID,
    },
    // If statement (desugars `if-else` into match-like semantics)
    If {
        condition: NodeID,
        then_block: NodeID,         // Always a Block
        else_block: Option<NodeID>, // Always a Block or None
    },
    // Loop statment (all loops are desugared into this simple loop)
    Loop {
        body: NodeID, // Always a block
    },
    Break,
    Continue,
    // Match statement (all complex patterns are reduced to simple patterns)
    Match {
        target: NodeID,
        arms: Vec<NodeID>,
    },
    MatchArm {
        pattern: NodeID, // Always a Pattern node
        body: NodeID,    // Always a Block
    },

    // Expressions
    IntLiteral(i64),
    UIntLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(StrID),
    BoolLiteral(bool),

    Identifier {
        module: Option<StrID>,
        name: StrID,
    },

    Binary {
        left: NodeID,
        operator: BinaryOp,
        right: NodeID,
    },
    Unary {
        operator: UnaryOp,
        operand: NodeID,
    },

    Call {
        func: NodeID,
        args: Vec<NodeID>,
    },

    EnumConstructor {
        target: Option<NodeID>,
        variant: StrID,
        // TODO: Should actual be a custom enum since we need None, Some, and Default options
        payload: Option<NodeID>,
    },

    StructConstructor {
        // no type_spec field required since the typespecs are stored in a side table
        fields: Vec<NodeID>,
    },

    StructConstructorField {
        name: StrID,
        value: NodeID,
    },

    Index {
        target: NodeID,
        index: NodeID,
    },

    Range {
        start: NodeID,
        end: NodeID,
    },

    FieldAccess {
        target: NodeID,
        field: StrID,
    },

    MetaType,

    Alloc {
        meta_type: NodeID,
        options: Vec<NodeID>,
    },

    Free {
        expr: NodeID,
    },

    Pattern(PatternNode),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum PatternNode {
    IntLiteral(i64),
    UIntLiteral(u64),
    StringLiteral(StrID),
    BoolLiteral(bool),
    FloatLiteral(f64),
    TypeSpec(TypeSpecPat),
    EnumVariant(EnumVariantPat),
    Default(DefaultPat),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct DefaultPat {
    // TODO: Should actual be a custom enum since we need None, Some, and Default options
    pub payload: Option<NodeID>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct EnumVariantPat {
    pub enum_name: Option<NodeID>,
    pub variant: StrID,
    // TODO: Should actual be a custom enum since we need None, Some, and Default options
    pub payload: Option<NodeID>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct TypeSpecPat {
    // type spec information get's stored in the type_map so there's no need to include it here
    // TODO: Should actual be a custom enum since we need None, Some, and Default options
    pub payload: Option<NodeID>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeSpec {
    // These types are no actually concreet yet, instead they all represent partially typed values
    // that can have their types further refined by the context where there appear
    IntLiteral(i64),
    UIntLiteral(u64),
    FloatLiteral(f64),
    InferredEnumExpr(InferredEnumExpr),
    InferredEnumPat(InferredEnumPat),
    Any,

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
    Named(NamedType),
    Pointer(Box<TypeSpec>),
    Slice(Box<TypeSpec>),
    Array(ArrayType),
    Struct(StructType),
    Enum(EnumType),
    Function(FunctionType),
    // UnsafePtr is the intermediary between types used by alloc
    UnsafePtr,
    // Panic is the type used for expressions that panic
    // TODO: should this just be a unit type? Is having a specific panic type useful? I'm not quite
    // sure yet but once we get to blocking I'll have a better idea
    Panic,
    // Unit type for functions that don't return anything
    Unit,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct InferredEnumExpr {
    pub variant_name: StrID,
    // TODO: Should actual be a custom enum since we need None, Some, and Default options
    pub payload: Option<Box<TypeSpec>>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct InferredEnumPat {
    pub variant_name: StrID,
    pub payload: Option<NodeID>,
}

/// Named type including the underalying type information
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct NamedType {
    // This is always an identifier expression
    pub name: NodeID,
    pub type_spec: Box<TypeSpec>,
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
    pub fields: Vec<StructTypeField>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructTypeField {
    pub name: StrID,
    pub type_spec: TypeSpec,
}

/// Enum type with named variants
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct EnumType {
    pub variants: Vec<EnumVariant>,
}

/// Function types with arguments
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct FunctionType {
    pub params: Vec<TypeSpec>,
    pub return_type: Box<TypeSpec>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: StrID,
    // TODO: Should actual be a custom enum since we need None, Some, and Default options
    pub payload: Option<TypeSpec>,
}

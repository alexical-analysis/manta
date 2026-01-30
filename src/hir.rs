use serde::{Deserialize, Serialize};

// High-level Intermediate Representation (HIR)
// This is a desugared, simplified version of the AST with a single node type.
// It removes syntactic sugar and represents all code uniformly as a tree of nodes.
//
type NodeID = usize;

pub struct NodeStore {
    nodes: Vec<Node>,
}

impl NodeStore {
    pub fn new() -> Self {
        NodeStore { nodes: vec![] }
    }

    pub fn add_node(&mut self, node: Node) -> NodeID {
        self.nodes.push(node);
        self.nodes.len() - 1
    }
}

/// A single node type that can represent any construct in the HIR
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Node {
    // Declarations
    FunctionDecl {
        name: String,
        params: Vec<NodeID>,
        return_type: TypeSpec,
        body: NodeID, // Always a Block
    },
    TypeDecl {
        // TODO: update this to a StrID from the StrStore
        name: String,
        type_spec: TypeSpec,
    },
    ConstDecl {
        // TODO: update this to a StrID from the StrStore
        name: String,
        value: NodeID,
    },
    VarDecl {
        // TODO: update this to a StrID from the StrStore
        name: String,
        value: NodeID,
    },
    UseDecl {
        // TODO: update this to a StrID from the StrStore
        modules: Vec<String>,
    },
    ModDecl {
        // TODO: update this to a StrID from the StrStore
        name: String,
    },

    // Statements
    Block {
        statements: Vec<NodeID>,
    },
    Declaration {
        // TODO: update this to a StrID from the StrStore
        name: String,
        // no value here because HIR just declares uninitalized variables
        // the value comes from a later assignment
    },
    Assign {
        target: NodeID,
        value: NodeID,
    },
    Return {
        value: Option<NodeID>,
    },
    Defer {
        block: Box<Node>, // Always a Block
    },
    // If statement (desugars `if-else` into match-like semantics)
    If {
        condition: NodeID,
        then_block: NodeID,         // Always a Block
        else_block: Option<NodeID>, // Always a Block or None
    },
    // Match statement (all complex patterns are reduced to simple patterns)
    Match {
        target: NodeID,
        arms: Vec<NodeID>,
    },
    MatchArm {
        pattern: Pattern,
        body: NodeID, // Always a Block
    },

    // Expressions
    IntLiteral(i64),
    FloatLiteral(f64),
    // TODO: update this to a StrID from the StrStore
    StringLiteral(String),
    BoolLiteral(bool),
    NilLiteral,

    // TODO: update this to a StrID from the StrStore
    Identifier(String),

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

    Index {
        target: NodeID,
        index: NodeID,
    },

    Range {
        start: NodeID,
        end: NodeID,
    },

    DotAccess {
        target: Option<NodeID>,
        field: String,
    },

    ModuleAccess {
        module: String,
        expr: NodeID,
    },

    MetaType {
        // TODO: should this be converted into a concreet struct?
        type_spec: TypeSpec,
    },

    Alloc {
        meta_type: NodeID,
        options: Vec<NodeID>,
    },

    Free {
        expr: NodeID,
    },

    Cast {
        expr: NodeID,
        target_type: TypeSpec,
    },
}

/// Simplified pattern representation
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Pattern {
    /// Literal patterns
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    FloatLiteral(f64),

    /// Type pattern
    TypeSpec(TypeSpec),

    /// Variable binding and "default" case
    Identifier(String),
}

/// Simplified parameter representation
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Param {
    pub name: String,
    pub type_spec: TypeSpec,
}

/// Simplified type specification
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
    Named {
        module: Option<String>,
        name: String,
    },
    Pointer(Box<TypeSpec>),
    Slice(Box<TypeSpec>),
    Array {
        type_spec: Box<TypeSpec>,
        size: usize,
    },
    Struct(Vec<StructField>),
    Enum(Vec<EnumVariant>),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructField {
    pub name: String,
    pub type_spec: TypeSpec,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: String,
    pub payload: Option<TypeSpec>,
}

/// Binary operators
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

/// Unary operators
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    Negate,
    Positive,
    Dereference,
    AddressOf,
}

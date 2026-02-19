use serde::Serialize;

use crate::ast::{BinaryOp, UnaryOp};
use crate::str_store::StrID;

// High-level Intermediate Representation (HIR)
// This is a desugared, simplified version of the AST with a single node type.
// It removes syntactic sugar and represents all code uniformly as a tree of nodes.

/// NodeID is the unique identifier for a gien node in the HIR tree
pub type NodeID = usize;

/// A single node type that can represent any construct in the HIR
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum Node {
    Invalid,
    FunctionDecl {
        name: StrID,
        // params are just VarDecl nodes
        params: Vec<NodeID>,
        body: NodeID,
    },
    TypeDecl {
        name: StrID,
    },
    UseDecl {
        modules: Vec<StrID>,
    },
    ModDecl {
        name: StrID,
    },
    Block {
        statements: Vec<NodeID>,
    },
    VarDecl {
        name: StrID,
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
    // Match statement (all complex patterns are reduced to simple patterns)
    Match {
        target: NodeID,
        arms: Vec<NodeID>,
    },
    // TODO: should this be a node or just a type that match contains?
    // They can't really appear on their own..
    MatchArm {
        pattern: NodeID, // Always a Pattern node
        body: NodeID,    // Always a Block
    },

    // Expressions
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(StrID),
    BoolLiteral(bool),
    NilLiteral,

    Identifier(StrID),

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
        target: Option<StrID>,
        variant: StrID,
        payload: Option<NodeID>,
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
        target: Option<NodeID>,
        field: StrID,
    },

    ModuleAccess {
        module: String,
        expr: NodeID,
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
    StringLiteral(StrID),
    BoolLiteral(bool),
    FloatLiteral(f64),

    TypeSpec,

    Payload {
        pat: NodeID, // Always a Pattern node
        payload: StrID,
    },
    ModuleAccess {
        module: StrID,
        pat: NodeID, // Always a Pattern Node
    },
    DotAccess {
        target: Option<NodeID>, // Always a Pattern node
        field: StrID,
    },

    Identifier(StrID),
    Default, // the _ pattern
}

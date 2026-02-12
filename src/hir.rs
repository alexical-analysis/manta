use serde::{Deserialize, Serialize};

use crate::ast::{BinaryOp, Pattern, TypeSpec, UnaryOp};
use crate::str_store::StrID;

// High-level Intermediate Representation (HIR)
// This is a desugared, simplified version of the AST with a single node type.
// It removes syntactic sugar and represents all code uniformly as a tree of nodes.

/// NodeID is the unique identifier for a gien node in the HIR tree
pub type NodeID = usize;

/// NodeTree contains all the nodes for a given tree as well as tracking the tree roots
#[derive(Serialize, Deserialize)]
pub struct NodeTree {
    nodes: Vec<Node>,
    roots: Vec<NodeID>,
}

impl NodeTree {
    /// Create a new NodeTree
    pub fn new() -> Self {
        NodeTree {
            nodes: vec![],
            roots: vec![],
        }
    }

    /// Add node adds a new node ot the store and returns its unique NodeID
    pub fn add_node(&mut self, node: Node) -> NodeID {
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    /// Adds a root node to the store and returns its unique NodeID
    pub fn add_root_node(&mut self, node: Node) -> NodeID {
        self.nodes.push(node);
        let id = self.nodes.len() - 1;
        self.roots.push(id);
        id
    }

    pub fn _get_node(&self, node_id: NodeID) -> Option<&Node> {
        self.nodes.get(node_id)
    }

    pub fn get_mut_node(&mut self, node_id: NodeID) -> Option<&mut Node> {
        self.nodes.get_mut(node_id)
    }
}

/// A single node type that can represent any construct in the HIR
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Node {
    Invalid,
    FunctionDecl {
        name: StrID,
        // params are just VarDecl nodes
        params: Vec<NodeID>,
        return_type: Option<TypeSpec>,
        body: NodeID,
    },
    TypeDecl {
        name: StrID,
        type_spec: TypeSpec,
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
        // not all declarations are requried to explicitly include a type
        // instead these types are infered
        type_spec: Option<TypeSpec>,
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
        pattern: Pattern,
        body: NodeID, // Always a Block
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

    MetaType {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_store_is_empty() {
        let store = NodeTree::new();
        assert_eq!(store.nodes.len(), 0);
        assert_eq!(store.roots.len(), 0);
    }

    #[test]
    fn test_add_node_returns_correct_id() {
        let mut store = NodeTree::new();
        let node = Node::NilLiteral;
        let id = store.add_node(node);
        assert_eq!(id, 0);
    }

    #[test]
    fn test_add_multiple_nodes() {
        let mut store = NodeTree::new();
        let id1 = store.add_node(Node::NilLiteral);
        let id2 = store.add_node(Node::BoolLiteral(true));
        let id3 = store.add_node(Node::IntLiteral(42));

        assert_eq!(id1, 0);
        assert_eq!(id2, 1);
        assert_eq!(id3, 2);
        assert_eq!(store.nodes.len(), 3);
    }

    #[test]
    fn test_add_node_does_not_add_to_roots() {
        let mut store = NodeTree::new();
        store.add_node(Node::NilLiteral);
        assert_eq!(store.roots.len(), 0);
    }

    #[test]
    fn test_add_root_node_returns_correct_id() {
        let mut store = NodeTree::new();
        let node = Node::NilLiteral;
        let id = store.add_root_node(node);
        assert_eq!(id, 0);
    }

    #[test]
    fn test_add_root_node_adds_to_roots() {
        let mut store = NodeTree::new();
        let id = store.add_root_node(Node::BoolLiteral(true));
        assert_eq!(store.roots.len(), 1);
        assert_eq!(store.roots[0], id);
    }

    #[test]
    fn test_add_multiple_root_nodes() {
        let mut store = NodeTree::new();
        let id1 = store.add_root_node(Node::IntLiteral(1));
        let id2 = store.add_root_node(Node::IntLiteral(2));
        let id3 = store.add_root_node(Node::IntLiteral(3));

        assert_eq!(store.roots.len(), 3);
        assert_eq!(store.roots[0], id1);
        assert_eq!(store.roots[1], id2);
        assert_eq!(store.roots[2], id3);
    }

    #[test]
    fn test_mix_nodes_and_root_nodes() {
        let mut store = NodeTree::new();
        let regular_id = store.add_node(Node::NilLiteral);
        let root_id = store.add_root_node(Node::BoolLiteral(true));
        let another_regular = store.add_node(Node::IntLiteral(42));

        assert_eq!(store.nodes.len(), 3);
        assert_eq!(store.roots.len(), 1);
        assert_eq!(store.roots[0], root_id);
        assert_ne!(regular_id, root_id);
        assert_ne!(another_regular, root_id);
    }

    #[test]
    fn test_add_int_literal() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::IntLiteral(100));
        assert_eq!(store.nodes[id], Node::IntLiteral(100));
    }

    #[test]
    fn test_add_float_literal() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::FloatLiteral(3.45));
        assert_eq!(store.nodes[id], Node::FloatLiteral(3.45));
    }

    #[test]
    fn test_add_string_literal() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::StringLiteral(0));
        assert_eq!(store.nodes[id], Node::StringLiteral(0));
    }

    #[test]
    fn test_add_bool_literal() {
        let mut store = NodeTree::new();
        let id_true = store.add_node(Node::BoolLiteral(true));
        let id_false = store.add_node(Node::BoolLiteral(false));

        assert_eq!(store.nodes[id_true], Node::BoolLiteral(true));
        assert_eq!(store.nodes[id_false], Node::BoolLiteral(false));
    }

    #[test]
    fn test_add_identifier() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Identifier(10));
        assert_eq!(store.nodes[id], Node::Identifier(10));
    }

    #[test]
    fn test_add_block_node() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Block {
            statements: vec![0, 1, 2],
        });
        assert_eq!(
            store.nodes[id],
            Node::Block {
                statements: vec![0, 1, 2],
            }
        );
    }

    #[test]
    fn test_add_binary_operation() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Binary {
            left: 0,
            operator: BinaryOp::Add,
            right: 1,
        });
        assert_eq!(
            store.nodes[id],
            Node::Binary {
                left: 0,
                operator: BinaryOp::Add,
                right: 1,
            }
        );
    }

    #[test]
    fn test_add_unary_operation() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Unary {
            operator: UnaryOp::Negate,
            operand: 0,
        });
        assert_eq!(
            store.nodes[id],
            Node::Unary {
                operator: UnaryOp::Negate,
                operand: 0,
            }
        );
    }

    #[test]
    fn test_add_function_declaration() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::FunctionDecl {
            name: 20,
            params: vec![],
            return_type: Some(TypeSpec::Int32),
            body: 0,
        });
        if let Node::FunctionDecl { name, .. } = &store.nodes[id] {
            assert_eq!(*name, 20);
        } else {
            panic!("Expected FunctionDecl");
        }
    }

    #[test]
    fn test_sequential_ids_are_unique() {
        let mut store = NodeTree::new();
        let mut ids = Vec::new();
        for i in 0..10 {
            let id = store.add_node(Node::IntLiteral(i as i64));
            ids.push(id);
        }

        // All IDs should be unique
        for i in 0..ids.len() {
            for j in i + 1..ids.len() {
                assert_ne!(ids[i], ids[j]);
            }
        }
    }

    #[test]
    fn test_add_many_root_nodes() {
        let mut store = NodeTree::new();
        for i in 0..20 {
            store.add_root_node(Node::IntLiteral(i as i64));
        }
        assert_eq!(store.roots.len(), 20);
        assert_eq!(store.nodes.len(), 20);
    }
}

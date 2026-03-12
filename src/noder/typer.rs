use crate::ast::UnaryOp;
use crate::hir::{Node, NodeID, StructField, StructType, TypeSpec};
use crate::noder::NodeTree;
use crate::str_store;

pub struct Typer {
    return_type: Option<TypeSpec>,
}

impl Typer {
    pub fn new() -> Self {
        Typer { return_type: None }
    }

    pub fn type_node_tree(&mut self, node_tree: &mut NodeTree) {
        for node in &node_tree.roots.clone() {
            self.type_node(node_tree, *node);
        }
    }

    fn type_node(&mut self, node_tree: &mut NodeTree, node_id: NodeID) {
        let node = match node_tree.get_node(node_id) {
            Some(n) => n,
            None => panic!("type checking unknown node"),
        };
        let node = node.clone();

        match node {
            Node::Defer { block } => {
                self.type_node(node_tree, block);
            }
            Node::If { .. } => {}
            Node::Match { .. } => {}
            Node::MatchArm { .. } => {}
            Node::Return { value } => match (value, &self.return_type) {
                (Some(v), Some(ret)) => {
                    let type_spec = node_tree
                        .type_map
                        .get(v)
                        .expect("missing type spec for return value");
                    if type_spec != ret {
                        panic!("return type does not match the expected type")
                    }
                }
                (None, Some(ret)) => {
                    if *ret != TypeSpec::Unit {
                        panic!("function expects a return value but none was given")
                    }
                }
                (_, None) => panic!("can not return a value outside a function context"),
            },
            Node::Free { .. } => {}
            Node::TypeDecl { .. } => {}
            Node::VarDecl { .. } => {}
            Node::Range { .. } => {
                // TODO: need to add a type for ranges
            }
            Node::Pattern(_) => {
                // TODO: need to type check patterns
            }

            Node::Block { statements } => {
                for node in statements {
                    self.type_node(node_tree, node);
                }
            }
            Node::FunctionDecl { body, .. } => {
                // we have to have the type in the type_map already
                let function_type = node_tree
                    .type_map
                    .get(node_id)
                    .expect("missing function declaration type information");

                let return_type = match function_type {
                    TypeSpec::Function(func) => *func.return_type.clone(),
                    _ => panic!("invalid type for function decl"),
                };

                self.return_type = Some(return_type);
                self.type_node(node_tree, body);

                // reset the return type after were done typing the function body since there are
                // other root nodes that might appear outside a function decl and we don't want
                // those to thing they have a valid return type
                self.return_type = None;
            }
            Node::Assign { target, value } => {
                let r_type = match self.type_expr_node(node_tree, value) {
                    Some(t) => t,
                    None => {
                        let r_node = node_tree.get_node(value);

                        panic!(
                            "value is missing a type (probably is a statement) {:?}\n{:?}",
                            value, r_node,
                        )
                    }
                };

                // TODO: make sure l_type is assignale (e.g. has a memory location)
                let l_type = match self.type_expr_node(node_tree, target) {
                    Some(t) => t,
                    None => {
                        if let Some(Node::Identifier { .. }) = node_tree.get_node(target) {
                            // if the left hand side has no type, check if the node is an identifier.
                            // if not this is a type checking bug and we should panic because there is
                            // a problem with the type checker itself.
                            //
                            // If it is an identifier we need to infer the type from the RHS and add
                            // it to the type_map
                            node_tree.type_map.add(target, r_type.clone());
                            r_type.clone()
                        } else {
                            panic!("missing type for assignment target")
                        }
                    }
                };

                if l_type != r_type {
                    panic!("l_type {:?} does not equal r_type {:?}", l_type, r_type)
                }
            }
            Node::Invalid
            | Node::IntLiteral(_)
            | Node::FloatLiteral(_)
            | Node::StringLiteral(_)
            | Node::BoolLiteral(_)
            | Node::Identifier { .. }
            | Node::Unary { .. }
            | Node::Binary { .. }
            | Node::Call { .. }
            | Node::Index { .. }
            | Node::EnumConstructor { .. }
            | Node::FieldAccess { .. }
            | Node::MetaType
            | Node::Alloc { .. } => {
                self.type_expr_node(node_tree, node_id);
            }
        }
    }

    // TODO: this should just return a type spec but I'm leaving it as optional right now for backwards
    // compatibility durring the refactor
    fn type_expr_node(&self, node_tree: &mut NodeTree, node_id: NodeID) -> Option<TypeSpec> {
        // if the type of this node is already known we can just return that, otherwise we need to go
        // through the work of inferring the type
        if let Some(ts) = node_tree.type_map.get(node_id) {
            return Some(ts.clone());
        }

        let node = node_tree
            .get_node(node_id)
            .expect("failed to find node for type checking");
        let node = node.clone();

        match node {
            Node::Invalid => None,
            Node::IntLiteral(_) => Some(TypeSpec::Int64),
            Node::FloatLiteral(_) => Some(TypeSpec::Float64),
            Node::StringLiteral(_) => Some(TypeSpec::String),
            Node::BoolLiteral(_) => Some(TypeSpec::Bool),
            Node::Binary {
                left,
                operator: _,
                right,
            } => {
                let l_type = self.type_expr_node(node_tree, left);
                let r_type = self.type_expr_node(node_tree, right);

                if l_type != r_type {
                    panic!("types do not match in binary expression");
                }

                l_type
            }
            Node::Identifier { .. } => node_tree.type_map.get(node_id).cloned(),
            Node::EnumConstructor { .. } => node_tree.type_map.get(node_id).cloned(),
            Node::FieldAccess { .. } => {
                // TODO: need struct/enum type information to determine field type
                None
            }

            Node::Unary { operator, operand } => {
                let operand_type = self.type_expr_node(node_tree, operand)?;
                match operator {
                    UnaryOp::AddressOf => {
                        // &T -> Pointer(T)
                        Some(TypeSpec::Pointer(Box::new(operand_type)))
                    }
                    UnaryOp::Dereference => {
                        // *Pointer(T) -> T
                        match operand_type {
                            TypeSpec::Pointer(inner) => Some(*inner),
                            _ => panic!("cannot dereference non-pointer type"),
                        }
                    }
                    UnaryOp::Not => {
                        // !T -> T (should be bool, but we don't enforce yet)
                        Some(operand_type)
                    }
                    UnaryOp::Negate => {
                        // -T -> T (should be a numeric type but we don't enforce that yet)
                        Some(operand_type)
                    }
                    UnaryOp::Positive => {
                        // +T -> T (should be a numeric type but we don't enforce that yet)
                        Some(operand_type)
                    }
                }
            }

            Node::Call { func, args: _ } => {
                let func_type = self
                    .type_expr_node(node_tree, func)
                    .expect("missing type for function call");
                match func_type {
                    TypeSpec::Function(ft) => Some(*ft.return_type),
                    _ => panic!("cannot call non-function type"),
                }
            }

            Node::Index { target, index: _ } => {
                let target_type = self
                    .type_expr_node(node_tree, target)
                    .expect("missing type for index target type");
                match target_type {
                    TypeSpec::Array(arr) => Some(*arr.type_spec),
                    TypeSpec::Slice(elem_type) => Some(*elem_type),
                    _ => panic!("cannot index non-array/slice type"),
                }
            }

            Node::MetaType => Some(TypeSpec::Struct(StructType {
                fields: vec![
                    StructField {
                        name: str_store::SIZEOF,
                        // TODO: this should actually be a usize instead of a u64 since we need to support
                        // 32-bit systems as well
                        type_spec: TypeSpec::UInt64,
                    },
                    StructField {
                        name: str_store::ALIGNOF,
                        // TODO: this should actually be a usize instead of a u64 since we need to support
                        // 32-bit systems as well
                        type_spec: TypeSpec::UInt64,
                    },
                    StructField {
                        name: str_store::METAFLAGS,
                        // TODO: this should actually be a usize instead of a u64 since we need to support
                        // 32-bit systems as well
                        type_spec: TypeSpec::UInt64,
                    },
                ],
            })),
            Node::Alloc { .. } => Some(TypeSpec::UnsafePtr),
            Node::Defer { .. }
            | Node::If { .. }
            | Node::VarDecl { .. }
            | Node::Match { .. }
            | Node::MatchArm { .. }
            | Node::Return { .. }
            | Node::Free { .. }
            | Node::TypeDecl { .. }
            | Node::Range { .. }
            | Node::Pattern(_)
            | Node::Block { .. }
            | Node::FunctionDecl { .. }
            | Node::Assign { .. } => panic!("can not type check a statement as an expression"),
        }
    }

    // resolve_type will unwrap named type aliases to find the underlying type
    fn resolve_type<'a>(&self, ts: &'a TypeSpec) -> &'a TypeSpec {
        match ts {
            TypeSpec::Named(t) => self.resolve_type(&*t.type_spec),
            _ => ts,
        }
    }
}

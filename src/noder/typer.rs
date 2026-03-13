use crate::ast::UnaryOp;
use crate::hir::{EnumVariant, Node, NodeID, StructField, StructType, TypeSpec};
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
            Node::If {
                condition,
                then_block,
                else_block,
            } => {
                self.type_expr_node(node_tree, condition);
                self.type_node(node_tree, then_block);
                if let Some(b) = else_block {
                    self.type_node(node_tree, b);
                }
            }
            Node::Match { .. } => {}
            Node::MatchArm { .. } => {}
            Node::Return { value } => match (value, &self.return_type) {
                (Some(v), Some(ret)) => {
                    let type_spec = node_tree.type_map.get(v);
                    // TODO: we are not fully ready to insiste that all values have types here
                    // but when we are we need to add in this line and remove the more generous
                    // test below
                    // .expect("missing type spec for return value");

                    match type_spec {
                        Some(ts) => {
                            if ts != ret {
                                panic!("return type does not match the expected type")
                            }
                        }
                        None => eprintln!("TODO; missing type for return value"),
                    };
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
                            let ts = r_type.clone();
                            node_tree.type_map.add(target, ts.clone());
                            ts
                        } else {
                            panic!(
                                "missing type for assignment target {:?}",
                                node_tree.get_node(target)
                            )
                        }
                    }
                };

                match match_types(&l_type, &r_type) {
                    TypeMatch::ExactType => {}
                    TypeMatch::Inference(ts) => {
                        node_tree.type_map.set(target, ts.clone());
                        node_tree.type_map.set(value, ts);
                    }
                    TypeMatch::Mismatch => {
                        panic!("l_type {:?} does not equal r_type {:?}", l_type, r_type)
                    }
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
            Node::Invalid => {
                let ts = TypeSpec::Panic;
                node_tree.type_map.add(node_id, ts.clone());
                Some(ts)
            }
            Node::IntLiteral(i) => {
                let ts = TypeSpec::IntLiteral(i);
                node_tree.type_map.add(node_id, ts.clone());
                Some(ts)
            }
            Node::FloatLiteral(f) => {
                let ts = TypeSpec::FloatLiteral(f);
                node_tree.type_map.add(node_id, ts.clone());
                Some(ts)
            }
            Node::StringLiteral(_) => {
                let ts = TypeSpec::String;
                node_tree.type_map.add(node_id, ts.clone());
                Some(ts)
            }
            Node::BoolLiteral(_) => {
                let ts = TypeSpec::Bool;
                node_tree.type_map.add(node_id, ts.clone());
                Some(ts)
            }
            Node::Binary {
                left,
                operator: _,
                right,
            } => {
                let l_type = self.type_expr_node(node_tree, left);
                let r_type = self.type_expr_node(node_tree, right);
                if l_type.is_none() || r_type.is_none() {
                    eprintln!("skpping type checking for now since l_type or r_type is missing");
                    return None;
                }

                match match_types(&l_type.clone().expect(""), &r_type.expect("")) {
                    TypeMatch::ExactType => l_type,
                    TypeMatch::Inference(ts) => {
                        node_tree.type_map.set(left, ts.clone());
                        node_tree.type_map.set(right, ts);
                        l_type
                    }
                    TypeMatch::Mismatch => panic!("types do not match in binary expression"),
                }
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
                        let ts = TypeSpec::Pointer(Box::new(operand_type));
                        node_tree.type_map.add(node_id, ts.clone());
                        Some(ts)
                    }
                    UnaryOp::Dereference => {
                        // *Pointer(T) -> T
                        let ts = match operand_type {
                            TypeSpec::Pointer(inner) => *inner,
                            _ => panic!("cannot dereference non-pointer type"),
                        };
                        node_tree.type_map.add(node_id, ts.clone());
                        Some(ts)
                    }
                    UnaryOp::Not => {
                        // !T -> T
                        let ts = operand_type;
                        if !is_bool_type(&ts) {
                            panic!("! can only be used on boolean types")
                        }
                        node_tree.type_map.add(node_id, ts.clone());
                        Some(ts)
                    }
                    UnaryOp::Negate => {
                        // -T -> T
                        let ts = operand_type;
                        if !is_numeric_type(&ts) {
                            panic!("! can only be used on boolean types")
                        }
                        node_tree.type_map.add(node_id, ts.clone());
                        Some(ts)
                    }
                    UnaryOp::Positive => {
                        // +T -> T
                        let ts = operand_type;
                        if !is_numeric_type(&ts) {
                            panic!("! can only be used on boolean types")
                        }
                        node_tree.type_map.add(node_id, ts.clone());
                        Some(ts)
                    }
                }
            }
            Node::Call { func, args } => {
                let func_type = self
                    .type_expr_node(node_tree, func)
                    .expect("missing type for function call");

                let func_type = match func_type {
                    TypeSpec::Function(ft) => ft,
                    _ => panic!("cannot call non-function type"),
                };

                for (param, arg) in func_type.params.iter().zip(args.iter()) {
                    let arg_type = self.type_expr_node(node_tree, *arg);
                    match &arg_type {
                        Some(ts) => match match_types(param, ts) {
                            TypeMatch::ExactType => {}
                            TypeMatch::Inference(_) => node_tree.type_map.set(*arg, param.clone()),
                            TypeMatch::Mismatch => panic!(
                                "argument type does not match expected paramater type {:?} {:?}",
                                param, ts
                            ),
                        },
                        None => eprintln!("TODO: missing type for argument"),
                    }
                }

                Some(*func_type.return_type)
            }
            Node::Index { target, index } => {
                let index_type = self
                    .type_expr_node(node_tree, index)
                    .expect("missing type for index expression");
                if !is_natural_number(&index_type) {
                    panic!("can only index expressions using natural numbers")
                }

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
}

// returns true if the type is any numeric type
// (u8-u64, i8-i64, f32, f64, or any alias of those types)
fn is_numeric_type(ts: &TypeSpec) -> bool {
    let ts = resolve_type(ts);
    matches!(
        ts,
        TypeSpec::Int8
            | TypeSpec::Int16
            | TypeSpec::Int32
            | TypeSpec::Int64
            | TypeSpec::UInt8
            | TypeSpec::UInt16
            | TypeSpec::UInt32
            | TypeSpec::UInt64
            | TypeSpec::Float32
            | TypeSpec::Float64
    )
}

// return true if the type is a boolean or any alias of a boolean type
fn is_bool_type(ts: &TypeSpec) -> bool {
    let ts = resolve_type(ts);
    matches!(ts, TypeSpec::Bool)
}

// returns true if the type contains only natural numbers (u8-64, or any alias of those types)
fn is_natural_number(ts: &TypeSpec) -> bool {
    let ts = resolve_type(ts);
    matches!(
        ts,
        TypeSpec::UInt8 | TypeSpec::UInt16 | TypeSpec::UInt32 | TypeSpec::UInt64
    )
}

// resolve_type will unwrap named type aliases to find the underlying type
fn resolve_type(ts: &TypeSpec) -> &TypeSpec {
    match ts {
        TypeSpec::Named(t) => resolve_type(&t.type_spec),
        _ => ts,
    }
}

enum TypeMatch {
    ExactType,
    Inference(TypeSpec),
    Mismatch,
}

fn match_types(a: &TypeSpec, b: &TypeSpec) -> TypeMatch {
    if a == b {
        return match a {
            TypeSpec::IntLiteral(_) => TypeMatch::Inference(TypeSpec::Int64),
            TypeSpec::FloatLiteral(_) => TypeMatch::Inference(TypeSpec::Float64),
            TypeSpec::InferredEnum(_) => TypeMatch::Mismatch,
            _ => TypeMatch::ExactType,
        };
    }

    match (a, b) {
        // if both the left and right hand side are typed as literals, just conver the type to an
        // int64 instead of trying to propogagte types through the graph
        (TypeSpec::IntLiteral(_), TypeSpec::IntLiteral(_)) => TypeMatch::Inference(TypeSpec::Int64),
        (TypeSpec::IntLiteral(i), inner_b) => match inner_b {
            TypeSpec::Int64 => match_int(i, i64::MIN, i64::MAX, TypeSpec::Int64),
            TypeSpec::Int32 => match_int(i, i32::MIN as i64, i32::MAX as i64, TypeSpec::Int64),
            TypeSpec::Int16 => match_int(i, i16::MIN as i64, i16::MAX as i64, TypeSpec::Int64),
            TypeSpec::Int8 => match_int(i, i8::MIN as i64, i8::MAX as i64, TypeSpec::Int64),
            // TODO: in order to support u64 types we need the IntLiteral type to be a bit more
            // complex because we can actually store the full range of a u64 in the current int
            // literal type
            TypeSpec::UInt32 => match_int(i, u32::MIN as i64, u32::MAX as i64, TypeSpec::Int64),
            TypeSpec::UInt16 => match_int(i, u16::MIN as i64, u16::MAX as i64, TypeSpec::Int64),
            TypeSpec::UInt8 => match_int(i, u8::MIN as i64, u8::MAX as i64, TypeSpec::Int64),
            _ => TypeMatch::Mismatch,
        },
        (inner_a, TypeSpec::IntLiteral(i)) => match inner_a {
            TypeSpec::Int64 => match_int(i, i64::MIN, i64::MAX, TypeSpec::Int64),
            TypeSpec::Int32 => match_int(i, i32::MIN as i64, i32::MAX as i64, TypeSpec::Int64),
            TypeSpec::Int16 => match_int(i, i16::MIN as i64, i16::MAX as i64, TypeSpec::Int64),
            TypeSpec::Int8 => match_int(i, i8::MIN as i64, i8::MAX as i64, TypeSpec::Int64),
            // TODO: in order to support u64 types we need the IntLiteral type to be a bit more
            // complex because we can actually store the full range of a u64 in the current int
            // literal type
            TypeSpec::UInt32 => match_int(i, u32::MIN as i64, u32::MAX as i64, TypeSpec::Int64),
            TypeSpec::UInt16 => match_int(i, u16::MIN as i64, u16::MAX as i64, TypeSpec::Int64),
            TypeSpec::UInt8 => match_int(i, u8::MIN as i64, u8::MAX as i64, TypeSpec::Int64),
            _ => TypeMatch::Mismatch,
        },
        // if both the left and right hand side are typed as literals, just conver the type to a
        // float64 instead of trying to propogagte types through the graph
        (TypeSpec::FloatLiteral(_), TypeSpec::FloatLiteral(_)) => {
            TypeMatch::Inference(TypeSpec::Float64)
        }
        (TypeSpec::FloatLiteral(f), inner_b) => match inner_b {
            TypeSpec::Float64 => match_float(f, f64::MIN, f64::MAX, TypeSpec::Float64),
            TypeSpec::Float32 => {
                match_float(f, f32::MIN as f64, f32::MAX as f64, TypeSpec::Float32)
            }
            _ => TypeMatch::Mismatch,
        },
        (inner_a, TypeSpec::FloatLiteral(f)) => match inner_a {
            TypeSpec::Float64 => match_float(f, f64::MIN, f64::MAX, TypeSpec::Float64),
            TypeSpec::Float32 => {
                match_float(f, f32::MIN as f64, f32::MAX as f64, TypeSpec::Float32)
            }
            _ => TypeMatch::Mismatch,
        },
        (TypeSpec::InferredEnum(inner_a), inner_b) => match_enum(inner_b, inner_a),
        (inner_a, TypeSpec::InferredEnum(inner_b)) => match_enum(inner_a, inner_b),
        _ => TypeMatch::Mismatch,
    }
}

fn match_int(target: &i64, min: i64, max: i64, ts: TypeSpec) -> TypeMatch {
    if *target >= min && *target <= max {
        TypeMatch::Inference(ts)
    } else {
        TypeMatch::Mismatch
    }
}

fn match_float(target: &f64, min: f64, max: f64, ts: TypeSpec) -> TypeMatch {
    // checking a float like this this might have issues at the largest and stmallest
    // values but it should be fine for now
    if *target >= min && *target <= max {
        TypeMatch::Inference(ts)
    } else {
        TypeMatch::Mismatch
    }
}

fn match_enum(known: &TypeSpec, unknown: &Box<EnumVariant>) -> TypeMatch {
    let known = resolve_type(known);
    match known {
        TypeSpec::Enum(e) => {
            let mut found_variant = None;
            for variant in &e.variants {
                if unknown.name == variant.name {
                    found_variant = Some(variant);
                    break;
                }
            }

            match found_variant {
                Some(v) => match (&v.payload, &unknown.payload) {
                    (Some(a), Some(b)) => {
                        if a == b {
                            TypeMatch::Inference(known.clone())
                        } else {
                            TypeMatch::Mismatch
                        }
                    }
                    (None, Some(_)) => TypeMatch::Mismatch,
                    (Some(_), None) => TypeMatch::Mismatch,
                    (None, None) => TypeMatch::Inference(known.clone()),
                },
                None => TypeMatch::Mismatch,
            }
        }
        _ => TypeMatch::Mismatch,
    }
}

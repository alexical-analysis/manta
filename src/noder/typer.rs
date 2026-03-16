use crate::ast::UnaryOp;
use crate::hir::{
    EnumVariant, NamedType, Node, NodeID, PatternNode, StructField, StructType, TypeSpec,
};
use crate::noder::NodeTree;
use crate::str_store;

enum TypeMatch {
    ExactType,
    Inference(TypeSpec),
    InferenceFailed,
    Mismatch,
}

pub struct Typer {
    return_type: Option<TypeSpec>,
    match_type: Vec<TypeSpec>,
}

impl Typer {
    pub fn new() -> Self {
        Typer {
            return_type: None,
            match_type: vec![],
        }
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
                node_tree.type_map.add(node_id, TypeSpec::Unit);

                self.type_node(node_tree, block);
            }
            Node::If {
                condition,
                then_block,
                else_block,
            } => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);

                self.type_expr_node(node_tree, condition);
                self.type_node(node_tree, then_block);
                if let Some(b) = else_block {
                    self.type_node(node_tree, b);
                }
            }
            Node::Match { target, arms } => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);

                let target_type = self
                    .type_expr_node(node_tree, target)
                    .expect("missing type for match expression");
                self.match_type.push(target_type);
                for arm in arms {
                    // TODO: this should base type inference off the match_type
                    let arm_type = self.type_expr_node(node_tree, arm);

                    // let target_type = target_type.clone();
                    // if target_type.is_none() || arm_type.is_none() {
                    //     eprintln!("TODO: target or arm type is missing, skipping for now");
                    //     continue;
                    // }

                    // // make sure the MatchArm has a type that works for the given target type
                    // match self
                    //     .match_types(target_type.as_ref().unwrap(), arm_type.as_ref().unwrap())
                    // {
                    //     TypeMatch::ExactType => {}
                    //     TypeMatch::Inference(ts) => {
                    //         node_tree.type_map.set(target, ts.clone());
                    //         node_tree.type_map.set(arm, ts.clone());
                    //     }
                    //     TypeMatch::InferenceFailed => panic!("failed to infer types for match"),
                    //     TypeMatch::Mismatch => {
                    //         // Enum type inference is not fully completed so this may not actuall
                    //         // represnet a type matching failure. Just mark it as a TODO for now.
                    //         eprintln!(
                    //             "TODO; invalid pattern for given match target \t\n{:?} \t\n{:?}",
                    //             target_type.clone(),
                    //             arm_type.clone(),
                    //         )
                    //     }
                    // }
                }

                self.match_type.pop();
            }
            Node::Return { value } => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);

                match (value, &self.return_type) {
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
                }
            }
            Node::Free { expr } => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);

                let free_type = self.type_expr_node(node_tree, expr);
                if free_type.is_none() {
                    eprintln!("TODO: missing type for free expression");
                    return;
                }

                let free_type = free_type.unwrap();
                let free_type = resolve_type(&free_type);
                match free_type {
                    TypeSpec::Pointer(_) => {}
                    _ => panic!("can not free memory of non-pointer type"),
                }
            }
            Node::TypeDecl { ident } => {
                // TypeDecl nodes must have been pre-typed durring the node-ing phase
                let type_spec = node_tree
                    .type_map
                    .get(node_id)
                    .expect("missing type for type decl");

                // the identifier is the named version of this type
                node_tree.type_map.add(
                    ident,
                    TypeSpec::Named(NamedType {
                        name: ident,
                        type_spec: Box::new(type_spec.clone()),
                    }),
                )
            }
            Node::VarDecl { .. } => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);
                // The identifier in the VarDecl nodes gets typed by it's first assignment or
                // they get pre-typed durring node-ing.
            }
            Node::Block { statements } => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);

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
                node_tree.type_map.add(node_id, TypeSpec::Unit);

                let r_type = match self.type_expr_node(node_tree, value) {
                    Some(t) => t,
                    None => {
                        let r_node = node_tree.get_node(value);
                        eprintln!(
                            "TODO: node is missing a type {:?}, skpping type checking for now",
                            r_node
                        );
                        return;
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

                match self.match_types(&l_type, &r_type) {
                    TypeMatch::ExactType => {}
                    TypeMatch::Inference(ts) => {
                        node_tree.type_map.set(target, ts.clone());
                        node_tree.type_map.set(value, ts);
                    }
                    TypeMatch::InferenceFailed => panic!("failed to infer types for assignment"),
                    TypeMatch::Mismatch => {
                        panic!("l_type {:?} does not equal r_type {:?}", l_type, r_type)
                    }
                }
            }
            Node::Invalid
            | Node::Pattern(_)
            | Node::IntLiteral(_)
            | Node::FloatLiteral(_)
            | Node::StringLiteral(_)
            | Node::BoolLiteral(_)
            | Node::Range { .. }
            | Node::Identifier { .. }
            | Node::MatchArm { .. }
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
    fn type_expr_node(&mut self, node_tree: &mut NodeTree, node_id: NodeID) -> Option<TypeSpec> {
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
                    eprintln!(
                        "TODO: skpping type checking for now since l_type or r_type is missing"
                    );
                    return None;
                }

                match self.match_types(&l_type.clone().expect(""), &r_type.expect("")) {
                    TypeMatch::ExactType => l_type,
                    TypeMatch::Inference(ts) => {
                        node_tree.type_map.set(left, ts.clone());
                        node_tree.type_map.set(right, ts);
                        l_type
                    }
                    TypeMatch::InferenceFailed => {
                        panic!("failed to infer type for binary expression")
                    }
                    TypeMatch::Mismatch => panic!("types do not match in binary expression"),
                }
            }
            Node::Identifier { .. } => {
                // Identifiers should get their types from context (e.g. from assignment or
                // function params)
                node_tree.type_map.get(node_id).cloned()
            }
            Node::EnumConstructor {
                target,
                variant,
                payload,
            } => match target {
                Some(t) => {
                    let target_ts = self.type_expr_node(node_tree, t);
                    if target_ts.is_none() {
                        eprintln!("TODO: missing type for payload identifier");
                        return None;
                    }

                    let target_ts = target_ts.unwrap();
                    match resolve_type(&target_ts) {
                        TypeSpec::Enum(e) => {
                            let mut found_variant = None;
                            for v in &e.variants {
                                if v.name == variant {
                                    found_variant = Some(v);
                                    break;
                                }
                            }

                            match found_variant {
                                Some(found) => match (payload, &found.payload) {
                                    (Some(p), Some(ts)) => {
                                        let pay_id = self.type_expr_node(node_tree, p);
                                        if pay_id.is_none() {
                                            eprintln!("TODO: missing type for variant payload");
                                            return Some(target_ts);
                                        }

                                        let pay_id = pay_id.unwrap();
                                        match self.match_types(&pay_id, ts) {
                                            TypeMatch::ExactType => {
                                                node_tree.type_map.add(node_id, target_ts.clone());
                                                Some(target_ts)
                                            }
                                            TypeMatch::Inference(ts) => {
                                                node_tree.type_map.set(p, ts);
                                                node_tree.type_map.add(node_id, target_ts.clone());
                                                Some(target_ts)
                                            }
                                            TypeMatch::InferenceFailed => {
                                                panic!("invalid type for variant payload")
                                            }
                                            TypeMatch::Mismatch => {
                                                panic!("invalid type for variant payload")
                                            }
                                        }
                                    }
                                    (None, Some(_)) => {
                                        panic!("variant was expecting a payload but none was given")
                                    }
                                    (Some(_), None) => panic!(
                                        "variant was not expecing a payload but one was given"
                                    ),
                                    (None, None) => {
                                        node_tree.type_map.add(node_id, target_ts.clone());
                                        Some(target_ts)
                                    }
                                },
                                None => panic!("failed to find variant for enum type"),
                            }
                        }
                        _ => panic!("target type is not an enum"),
                    }
                }
                None => {
                    let pay = payload.map(|p| self.type_expr_node(node_tree, p));
                    if payload.is_some() && pay.is_none() {
                        eprintln!("TODO: missing type for payload identifier");
                        return None;
                    }

                    let ts = TypeSpec::InferredEnum(Box::new(EnumVariant {
                        name: variant,
                        payload: pay.unwrap(),
                    }));
                    node_tree.type_map.add(node_id, ts.clone());
                    Some(ts)
                }
            },
            Node::FieldAccess { .. } => {
                // TODO: need struct/enum type information to determine field type
                None
            }
            Node::MatchArm { pattern, body } => {
                let pattern_type = self.type_pattern_node(node_tree, pattern);
                self.type_node(node_tree, body);
                node_tree.type_map.add(node_id, pattern_type.clone());
                Some(pattern_type)
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
                        Some(ts) => match self.match_types(param, ts) {
                            TypeMatch::ExactType => {}
                            TypeMatch::Inference(_) => node_tree.type_map.set(*arg, param.clone()),
                            TypeMatch::InferenceFailed => {
                                panic!("failed to infer type for argument")
                            }
                            TypeMatch::Mismatch => panic!(
                                "argument type does not match expected paramater type {:?} {:?}",
                                param, ts
                            ),
                        },
                        None => eprintln!("TODO: missing type for argument"),
                    }
                }

                let ret_type = *func_type.return_type;
                node_tree.type_map.add(node_id, ret_type.clone());
                Some(ret_type)
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
            Node::Range { .. } => todo!("Range expressions are not yet supported"),
            Node::Pattern(_) => panic!("Invalid position for a patter node"),
            Node::Alloc { .. } => Some(TypeSpec::UnsafePtr),
            Node::Defer { .. }
            | Node::If { .. }
            | Node::VarDecl { .. }
            | Node::Match { .. }
            | Node::Return { .. }
            | Node::Free { .. }
            | Node::TypeDecl { .. }
            | Node::Block { .. }
            | Node::FunctionDecl { .. }
            | Node::Assign { .. } => panic!("can not type check a statement as an expression"),
        }
    }

    fn type_pattern_node(&mut self, node_tree: &mut NodeTree, node_id: NodeID) -> TypeSpec {
        let node = node_tree
            .get_node(node_id)
            .expect("failed to find node for type checking");
        let node = node.clone();

        let pat = match node {
            Node::Pattern(pat) => pat,
            _ => panic!("provided node is not a pattern"),
        };

        let target_type = self
            .match_type
            .last()
            .expect("patterns can only appear inside match/ let statments");

        match pat {
            PatternNode::IntLiteral(i) => {
                let ts = TypeSpec::IntLiteral(i);
                match self.match_types(target_type, &ts) {
                    TypeMatch::Inference(ts) => node_tree.type_map.add(node_id, ts.clone()),
                    _ => panic!("invalid type for match pattern"),
                };
                ts
            }
            PatternNode::StringLiteral(_) => {
                let ts = TypeSpec::String;
                match self.match_types(target_type, &ts) {
                    TypeMatch::Inference(ts) => node_tree.type_map.add(node_id, ts.clone()),
                    _ => panic!("invalid type for match pattern"),
                };
                ts
            }
            PatternNode::BoolLiteral(_) => {
                let ts = TypeSpec::Bool;
                match self.match_types(target_type, &ts) {
                    TypeMatch::Inference(ts) => node_tree.type_map.add(node_id, ts.clone()),
                    _ => panic!("invalid type for match pattern"),
                };
                ts
            }
            PatternNode::FloatLiteral(f) => {
                let ts = TypeSpec::FloatLiteral(f);
                match self.match_types(target_type, &ts) {
                    TypeMatch::Inference(ts) => node_tree.type_map.add(node_id, ts.clone()),
                    _ => panic!("invalid type for match pattern"),
                };
                ts
            }
            PatternNode::TypeSpec(ts) => {
                // type must already be set so we check that here to return
                let type_spec = node_tree
                    .type_map
                    .get(node_id)
                    .expect("missing type for type spec pattern")
                    .clone();

                if let Some(pay) = ts.payload {
                    // this is a declaration for this identifier so we can directly set the type
                    // here to be the same as the type_spec itself
                    node_tree.type_map.add(pay, type_spec.clone())
                }

                match self.match_types(target_type, &type_spec) {
                    TypeMatch::ExactType => node_tree.type_map.add(node_id, type_spec.clone()),
                    _ => panic!("invalid type for match pattern"),
                };

                type_spec
            }
            PatternNode::EnumVariant(e) => {
                let ts = match e.enum_name {
                    Some(n) => {
                        let ts = node_tree
                            .type_map
                            .get(n)
                            .expect("missing type for enum name")
                            .clone();

                        if let Some(pay) = e.payload {
                            node_tree.type_map.add(pay, ts.clone());
                        }

                        ts
                    }
                    None => todo!(
                        "use the match type here since that's the only place that patterns are allowed to show up"
                    ),
                };
                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            PatternNode::Default(d) => {
                if let Some(pay) = d.payload {
                    // TODO: this is going to cause problems because we'll infer the type for the
                    // default pattern but that won't update the inner payload. We'll either need a
                    // second pass or to do something more clever...
                    node_tree.type_map.add(pay, TypeSpec::Any);
                }

                let ts = TypeSpec::Any;
                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
        }
    }

    fn match_types(&self, a: &TypeSpec, b: &TypeSpec) -> TypeMatch {
        if a == b {
            return match a {
                TypeSpec::IntLiteral(_) => TypeMatch::Inference(TypeSpec::Int64),
                TypeSpec::FloatLiteral(_) => TypeMatch::Inference(TypeSpec::Float64),
                TypeSpec::InferredEnum(_) => TypeMatch::InferenceFailed,
                TypeSpec::Any => TypeMatch::InferenceFailed,
                _ => TypeMatch::ExactType,
            };
        }

        match (a, b) {
            // if both the left and right hand side are typed as literals, just conver the type to an
            // int64 instead of trying to propogagte types through the graph
            (TypeSpec::IntLiteral(_), TypeSpec::IntLiteral(_)) => {
                TypeMatch::Inference(TypeSpec::Int64)
            }
            (TypeSpec::IntLiteral(i), inner_b) => match inner_b {
                TypeSpec::Int64 => match_int(i, i64::MIN, i64::MAX, TypeSpec::Int64),
                TypeSpec::Int32 => match_int(i, i32::MIN as i64, i32::MAX as i64, TypeSpec::Int32),
                TypeSpec::Int16 => match_int(i, i16::MIN as i64, i16::MAX as i64, TypeSpec::Int16),
                TypeSpec::Int8 => match_int(i, i8::MIN as i64, i8::MAX as i64, TypeSpec::Int8),
                // TODO: in order to support u64 types we need the IntLiteral type to be a bit more
                // complex because we can actually store the full range of a u64 in the current int
                // literal type
                TypeSpec::UInt32 => {
                    match_int(i, u32::MIN as i64, u32::MAX as i64, TypeSpec::UInt32)
                }
                TypeSpec::UInt16 => {
                    match_int(i, u16::MIN as i64, u16::MAX as i64, TypeSpec::UInt16)
                }
                TypeSpec::UInt8 => match_int(i, u8::MIN as i64, u8::MAX as i64, TypeSpec::UInt8),
                _ => TypeMatch::Mismatch,
            },
            (inner_a, TypeSpec::IntLiteral(i)) => match inner_a {
                TypeSpec::Int64 => match_int(i, i64::MIN, i64::MAX, TypeSpec::Int64),
                TypeSpec::Int32 => match_int(i, i32::MIN as i64, i32::MAX as i64, TypeSpec::Int32),
                TypeSpec::Int16 => match_int(i, i16::MIN as i64, i16::MAX as i64, TypeSpec::Int16),
                TypeSpec::Int8 => match_int(i, i8::MIN as i64, i8::MAX as i64, TypeSpec::Int8),
                // TODO: in order to support u64 types we need the IntLiteral type to be a bit more
                // complex because we can actually store the full range of a u64 in the current int
                // literal type
                TypeSpec::UInt32 => {
                    match_int(i, u32::MIN as i64, u32::MAX as i64, TypeSpec::UInt32)
                }
                TypeSpec::UInt16 => {
                    match_int(i, u16::MIN as i64, u16::MAX as i64, TypeSpec::UInt16)
                }
                TypeSpec::UInt8 => match_int(i, u8::MIN as i64, u8::MAX as i64, TypeSpec::UInt8),
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
            (TypeSpec::InferredEnum(inner_a), inner_b) => self.match_enum(inner_b, inner_a),
            (inner_a, TypeSpec::InferredEnum(inner_b)) => self.match_enum(inner_a, inner_b),
            (TypeSpec::Any, inner_b) => TypeMatch::Inference(inner_b.clone()),
            (inner_a, TypeSpec::Any) => TypeMatch::Inference(inner_a.clone()),
            _ => TypeMatch::Mismatch,
        }
    }

    fn match_enum(&self, known: &TypeSpec, unknown: &TypeSpec) -> TypeMatch {
        let known = resolve_type(known);
        match known {
            TypeSpec::Enum(e) => {
                let mut found_variant = None;
                for variant in &e.variants {
                    if unknown.variant_name == variant.name {
                        found_variant = Some(variant);
                        break;
                    }
                }

                match found_variant {
                    Some(v) => match (&v.payload, &unknown.payload) {
                        (Some(a), Some(b)) => {
                            // TODO: Update this logic
                            // 1) Get the type of b
                            // 2) This type could be Inferrable so we'll need to recurse into
                            //    `match_types`
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
            | TypeSpec::IntLiteral(_)
            | TypeSpec::FloatLiteral(_)
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

fn match_int(target: &i64, min: i64, max: i64, ts: TypeSpec) -> TypeMatch {
    if *target >= min && *target <= max {
        TypeMatch::Inference(ts)
    } else {
        TypeMatch::InferenceFailed
    }
}

fn match_float(target: &f64, min: f64, max: f64, ts: TypeSpec) -> TypeMatch {
    // checking a float like this this might have issues at the largest and stmallest
    // values but it should be fine for now
    if *target >= min && *target <= max {
        TypeMatch::Inference(ts)
    } else {
        TypeMatch::InferenceFailed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::{EnumType, NamedType};
    use crate::str_store::StrID;

    // Helper: wrap a TypeSpec in a Named alias
    fn named(ts: TypeSpec) -> TypeSpec {
        TypeSpec::Named(NamedType {
            name: NodeID::from_usize(0),
            type_spec: Box::new(ts),
        })
    }

    // Helper: build a concrete enum TypeSpec from a list of variants
    fn enum_type(variants: Vec<EnumVariant>) -> TypeSpec {
        TypeSpec::Enum(EnumType { variants })
    }

    // Helper: build an EnumVariant with no payload
    fn variant(id: usize) -> EnumVariant {
        EnumVariant {
            name: StrID::from_usize(id),
            payload: None,
        }
    }

    // Helper: build an EnumVariant with a payload
    fn variant_with(id: usize, payload: TypeSpec) -> EnumVariant {
        EnumVariant {
            name: StrID::from_usize(id),
            payload: Some(payload),
        }
    }

    // -------------------------------------------------------------------------
    // resolve_type
    // -------------------------------------------------------------------------

    #[test]
    fn resolve_type_concrete_returns_self() {
        assert_eq!(resolve_type(&TypeSpec::Int64), &TypeSpec::Int64);
        assert_eq!(resolve_type(&TypeSpec::Bool), &TypeSpec::Bool);
        assert_eq!(resolve_type(&TypeSpec::Float32), &TypeSpec::Float32);
        assert_eq!(resolve_type(&TypeSpec::String), &TypeSpec::String);
    }

    #[test]
    fn resolve_type_named_unwraps_once() {
        let ts = named(TypeSpec::Int32);
        assert_eq!(resolve_type(&ts), &TypeSpec::Int32);
    }

    #[test]
    fn resolve_type_named_nested_unwraps_fully() {
        let ts = named(named(named(TypeSpec::Bool)));
        assert_eq!(resolve_type(&ts), &TypeSpec::Bool);
    }

    // -------------------------------------------------------------------------
    // is_numeric_type
    // -------------------------------------------------------------------------

    #[test]
    fn is_numeric_type_all_concrete_numeric_types() {
        for ts in [
            TypeSpec::Int8,
            TypeSpec::Int16,
            TypeSpec::Int32,
            TypeSpec::Int64,
            TypeSpec::UInt8,
            TypeSpec::UInt16,
            TypeSpec::UInt32,
            TypeSpec::UInt64,
            TypeSpec::Float32,
            TypeSpec::Float64,
        ] {
            assert!(is_numeric_type(&ts), "{ts:?} should be numeric");
        }
    }

    #[test]
    fn is_numeric_type_non_numeric_types() {
        for ts in [
            TypeSpec::Bool,
            TypeSpec::String,
            TypeSpec::Unit,
            TypeSpec::UnsafePtr,
            TypeSpec::Panic,
            TypeSpec::Any,
        ] {
            assert!(!is_numeric_type(&ts), "{ts:?} should not be numeric");
        }
    }

    #[test]
    fn is_numeric_type_literals_are_numeric() {
        assert!(is_numeric_type(&TypeSpec::IntLiteral(42)));
        assert!(is_numeric_type(&TypeSpec::IntLiteral(-7)));
        assert!(is_numeric_type(&TypeSpec::FloatLiteral(3.45)));
    }

    #[test]
    fn is_numeric_type_resolves_named_alias() {
        assert!(is_numeric_type(&named(TypeSpec::Int64)));
        assert!(is_numeric_type(&named(TypeSpec::Float32)));
        assert!(!is_numeric_type(&named(TypeSpec::Bool)));
    }

    // -------------------------------------------------------------------------
    // is_bool_type
    // -------------------------------------------------------------------------

    #[test]
    fn is_bool_type_bool_is_true() {
        assert!(is_bool_type(&TypeSpec::Bool));
    }

    #[test]
    fn is_bool_type_non_bool_types() {
        for ts in [
            TypeSpec::Int64,
            TypeSpec::Float64,
            TypeSpec::String,
            TypeSpec::Unit,
            TypeSpec::IntLiteral(1),
        ] {
            assert!(!is_bool_type(&ts), "{ts:?} should not be bool");
        }
    }

    #[test]
    fn is_bool_type_resolves_named_alias() {
        assert!(is_bool_type(&named(TypeSpec::Bool)));
        assert!(!is_bool_type(&named(TypeSpec::Int32)));
    }

    // -------------------------------------------------------------------------
    // is_natural_number
    // -------------------------------------------------------------------------

    #[test]
    fn is_natural_number_unsigned_types() {
        for ts in [
            TypeSpec::UInt8,
            TypeSpec::UInt16,
            TypeSpec::UInt32,
            TypeSpec::UInt64,
        ] {
            assert!(is_natural_number(&ts), "{ts:?} should be a natural number");
        }
    }

    #[test]
    fn is_natural_number_signed_and_float_are_not_natural() {
        for ts in [
            TypeSpec::Int8,
            TypeSpec::Int16,
            TypeSpec::Int32,
            TypeSpec::Int64,
            TypeSpec::Float32,
            TypeSpec::Float64,
            TypeSpec::Bool,
            TypeSpec::String,
        ] {
            assert!(
                !is_natural_number(&ts),
                "{ts:?} should not be a natural number"
            );
        }
    }

    #[test]
    fn is_natural_number_resolves_named_alias() {
        assert!(is_natural_number(&named(TypeSpec::UInt64)));
        assert!(!is_natural_number(&named(TypeSpec::Int64)));
    }

    // -------------------------------------------------------------------------
    // match_int
    // -------------------------------------------------------------------------

    #[test]
    fn match_int_value_within_range_returns_inference() {
        assert!(matches!(
            match_int(&42, 0, 100, TypeSpec::Int64),
            TypeMatch::Inference(TypeSpec::Int64)
        ));
    }

    #[test]
    fn match_int_value_at_min_boundary_returns_inference() {
        assert!(matches!(
            match_int(&0, 0, 255, TypeSpec::UInt8),
            TypeMatch::Inference(TypeSpec::UInt8)
        ));
    }

    #[test]
    fn match_int_value_at_max_boundary_returns_inference() {
        assert!(matches!(
            match_int(&255, 0, 255, TypeSpec::UInt8),
            TypeMatch::Inference(TypeSpec::UInt8)
        ));
    }

    #[test]
    fn match_int_value_below_min_returns_inference_failed() {
        assert!(matches!(
            match_int(&-1, 0, 255, TypeSpec::UInt8),
            TypeMatch::InferenceFailed
        ));
    }

    #[test]
    fn match_int_value_above_max_returns_inference_failed() {
        assert!(matches!(
            match_int(&256, 0, 255, TypeSpec::UInt8),
            TypeMatch::InferenceFailed
        ));
    }

    // -------------------------------------------------------------------------
    // match_float
    // -------------------------------------------------------------------------

    #[test]
    fn match_float_value_within_range_returns_inference() {
        assert!(matches!(
            match_float(&1.5, f64::MIN, f64::MAX, TypeSpec::Float64),
            TypeMatch::Inference(TypeSpec::Float64)
        ));
    }

    #[test]
    fn match_float_value_out_of_f32_range_returns_inference_failed() {
        let too_large = f32::MAX as f64 * 2.0;
        assert!(matches!(
            match_float(
                &too_large,
                f32::MIN as f64,
                f32::MAX as f64,
                TypeSpec::Float32
            ),
            TypeMatch::InferenceFailed
        ));
    }

    // -------------------------------------------------------------------------
    // match_types
    // -------------------------------------------------------------------------

    #[test]
    fn match_types_same_concrete_type_is_exact() {
        for ts in [
            TypeSpec::Int64,
            TypeSpec::Bool,
            TypeSpec::String,
            TypeSpec::Float32,
        ] {
            assert!(
                matches!(match_types(&ts, &ts.clone()), TypeMatch::ExactType),
                "{ts:?} == {ts:?} should be ExactType"
            );
        }
    }

    #[test]
    fn match_types_same_int_literal_infers_int64() {
        assert!(matches!(
            match_types(&TypeSpec::IntLiteral(5), &TypeSpec::IntLiteral(5)),
            TypeMatch::Inference(TypeSpec::Int64)
        ));
    }

    #[test]
    fn match_types_different_int_literals_infer_int64() {
        assert!(matches!(
            match_types(&TypeSpec::IntLiteral(5), &TypeSpec::IntLiteral(10)),
            TypeMatch::Inference(TypeSpec::Int64)
        ));
    }

    #[test]
    fn match_types_float_literals_infer_float64() {
        assert!(matches!(
            match_types(&TypeSpec::FloatLiteral(1.5), &TypeSpec::FloatLiteral(2.5)),
            TypeMatch::Inference(TypeSpec::Float64)
        ));
    }

    #[test]
    fn match_types_int_literal_with_int64_in_range() {
        assert!(matches!(
            match_types(&TypeSpec::IntLiteral(42), &TypeSpec::Int64),
            TypeMatch::Inference(TypeSpec::Int64)
        ));
    }

    #[test]
    fn match_types_int_literal_with_uint8_in_range() {
        assert!(matches!(
            match_types(&TypeSpec::IntLiteral(200), &TypeSpec::UInt8),
            TypeMatch::Inference(TypeSpec::UInt8)
        ));
    }

    #[test]
    fn match_types_int_literal_with_uint8_out_of_range() {
        assert!(matches!(
            match_types(&TypeSpec::IntLiteral(300), &TypeSpec::UInt8),
            TypeMatch::InferenceFailed
        ));
    }

    #[test]
    fn match_types_int_literal_with_int8_negative_in_range() {
        assert!(matches!(
            match_types(&TypeSpec::IntLiteral(-100), &TypeSpec::Int8),
            TypeMatch::Inference(TypeSpec::Int8)
        ));
    }

    #[test]
    fn match_types_int_literal_with_int8_out_of_range() {
        assert!(matches!(
            match_types(&TypeSpec::IntLiteral(200), &TypeSpec::Int8),
            TypeMatch::InferenceFailed
        ));
    }

    #[test]
    fn match_types_reversed_int_literal_and_int64() {
        // (Int64, IntLiteral) should behave the same as (IntLiteral, Int64)
        assert!(matches!(
            match_types(&TypeSpec::Int64, &TypeSpec::IntLiteral(42)),
            TypeMatch::Inference(TypeSpec::Int64)
        ));
    }

    #[test]
    fn match_types_float_literal_with_float64() {
        assert!(matches!(
            match_types(&TypeSpec::FloatLiteral(1.5), &TypeSpec::Float64),
            TypeMatch::Inference(TypeSpec::Float64)
        ));
    }

    #[test]
    fn match_types_float_literal_with_float32() {
        assert!(matches!(
            match_types(&TypeSpec::FloatLiteral(1.5), &TypeSpec::Float32),
            TypeMatch::Inference(TypeSpec::Float32)
        ));
    }

    #[test]
    fn match_types_float_literal_against_int_is_mismatch() {
        assert!(matches!(
            match_types(&TypeSpec::FloatLiteral(1.5), &TypeSpec::Int64),
            TypeMatch::Mismatch
        ));
    }

    #[test]
    fn match_types_any_with_concrete_infers_concrete() {
        assert!(matches!(
            match_types(&TypeSpec::Any, &TypeSpec::Int64),
            TypeMatch::Inference(TypeSpec::Int64)
        ));
        assert!(matches!(
            match_types(&TypeSpec::Bool, &TypeSpec::Any),
            TypeMatch::Inference(TypeSpec::Bool)
        ));
    }

    #[test]
    fn match_types_both_any_is_inference_failed() {
        assert!(matches!(
            match_types(&TypeSpec::Any, &TypeSpec::Any),
            TypeMatch::InferenceFailed
        ));
    }

    #[test]
    fn match_types_inferred_enum_same_is_inference_failed() {
        let ts = TypeSpec::InferredEnum(Box::new(variant(1)));
        assert!(matches!(
            match_types(&ts, &ts.clone()),
            TypeMatch::InferenceFailed
        ));
    }

    #[test]
    fn match_types_concrete_mismatch() {
        assert!(matches!(
            match_types(&TypeSpec::Int64, &TypeSpec::Bool),
            TypeMatch::Mismatch
        ));
        assert!(matches!(
            match_types(&TypeSpec::String, &TypeSpec::Float64),
            TypeMatch::Mismatch
        ));
    }

    #[test]
    fn match_types_inferred_enum_with_known_enum() {
        let v_id = 1;
        let known = enum_type(vec![variant(v_id)]);
        let unknown = TypeSpec::InferredEnum(Box::new(variant(v_id)));
        // (InferredEnum, known) -> match_enum
        assert!(matches!(
            match_types(&unknown, &known),
            TypeMatch::Inference(_)
        ));
    }

    // -------------------------------------------------------------------------
    // match_enum
    // -------------------------------------------------------------------------

    #[test]
    fn match_enum_matching_variant_no_payload() {
        let v_id = 1;
        let known = enum_type(vec![variant(v_id)]);
        let unknown = variant(v_id);
        assert!(matches!(
            match_enum(&known, &unknown),
            TypeMatch::Inference(_)
        ));
    }

    #[test]
    fn match_enum_unknown_variant_is_mismatch() {
        let known = enum_type(vec![variant(1)]);
        let unknown = variant(99);
        assert!(matches!(match_enum(&known, &unknown), TypeMatch::Mismatch));
    }

    #[test]
    fn match_enum_matching_variant_with_matching_payload() {
        let v_id = 1;
        let known = enum_type(vec![variant_with(v_id, TypeSpec::Int64)]);
        let unknown = variant_with(v_id, TypeSpec::Int64);
        assert!(matches!(
            match_enum(&known, &unknown),
            TypeMatch::Inference(_)
        ));
    }

    #[test]
    fn match_enum_mismatched_payload_is_mismatch() {
        let v_id = 1;
        let known = enum_type(vec![variant_with(v_id, TypeSpec::Int64)]);
        let unknown = variant_with(v_id, TypeSpec::Bool);
        assert!(matches!(match_enum(&known, &unknown), TypeMatch::Mismatch));
    }

    #[test]
    fn match_enum_expected_payload_but_none_given_is_mismatch() {
        let v_id = 1;
        let known = enum_type(vec![variant_with(v_id, TypeSpec::Int64)]);
        let unknown = variant(v_id); // no payload
        assert!(matches!(match_enum(&known, &unknown), TypeMatch::Mismatch));
    }

    #[test]
    fn match_enum_unexpected_payload_is_mismatch() {
        let v_id = 1;
        let known = enum_type(vec![variant(v_id)]); // no payload on known variant
        let unknown = variant_with(v_id, TypeSpec::Int64);
        assert!(matches!(match_enum(&known, &unknown), TypeMatch::Mismatch));
    }

    #[test]
    fn match_enum_non_enum_known_is_mismatch() {
        let unknown = variant(1);
        assert!(matches!(
            match_enum(&TypeSpec::Int64, &unknown),
            TypeMatch::Mismatch
        ));
        assert!(matches!(
            match_enum(&TypeSpec::Bool, &unknown),
            TypeMatch::Mismatch
        ));
    }

    #[test]
    fn match_enum_resolves_named_enum_alias() {
        let v_id = 1;
        let known = named(enum_type(vec![variant(v_id)]));
        let unknown = variant(v_id);
        assert!(matches!(
            match_enum(&known, &unknown),
            TypeMatch::Inference(_)
        ));
    }
}

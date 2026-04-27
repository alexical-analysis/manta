use std::ops::Deref;

use crate::ast::{BinaryOp, UnaryOp};
use crate::hir::{
    EnumVariant, InferredEnumExpr, InferredEnumPat, NamedType, Node, NodeID, PatternNode,
    StructType, StructTypeField, TypeSpec,
};
use crate::noder::NodeTree;
use crate::str_store::{self, StrID};

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

    pub fn type_node_tree(&mut self, node_tree: &mut NodeTree, roots: &[NodeID]) {
        for node in roots {
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
            Node::Loop { body } => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);

                self.type_node(node_tree, body);
            }
            Node::Break => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);
            }
            Node::Continue => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);
            }
            Node::Match { target, arms } => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);

                let target_type = self.type_expr_node(node_tree, target).clone();

                self.match_type.push(target_type.clone());
                for arm in arms {
                    self.type_node(node_tree, arm);
                }
                self.match_type.pop();
            }
            Node::MatchArm { pattern, body } => {
                let pattern_type = self.type_pattern_node(node_tree, pattern);
                node_tree.type_map.add(node_id, pattern_type.clone());

                self.type_node(node_tree, body);
            }
            Node::Return { value } => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);

                match (value, &self.return_type.clone()) {
                    (Some(v), Some(ret)) => {
                        let type_spec = self.type_expr_node(node_tree, v);
                        match match_types(ret, &type_spec) {
                            TypeMatch::ExactType => {}
                            TypeMatch::Inference(ts) => {
                                node_tree.type_map.set(v, ts);
                            }
                            TypeMatch::InferenceFailed => {
                                panic!("can not infer type in this context")
                            }
                            TypeMatch::Mismatch => panic!(
                                "invalid return value with type {:?} in a function with the return type {:?}",
                                type_spec, ret
                            ),
                        }
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
                match resolve_type(&free_type) {
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
            Node::FunctionDecl { params, body, .. } => {
                // we have to have the type in the type_map already
                let function_type = node_tree
                    .type_map
                    .get(node_id)
                    .expect("missing function declaration type information");

                let return_type = match function_type {
                    TypeSpec::Function(func) => *func.return_type.clone(),
                    _ => panic!("invalid type for function decl"),
                };

                let params = params.clone();
                self.return_type = Some(return_type);
                for param in &params {
                    self.type_node(node_tree, *param);
                }
                self.type_node(node_tree, body);

                // reset the return type after were done typing the function body since there are
                // other root nodes that might appear outside a function decl and we don't want
                // those to thing they have a valid return type
                self.return_type = None;
            }
            Node::Assign { target, value } => {
                node_tree.type_map.add(node_id, TypeSpec::Unit);

                let r_type = self.type_expr_node(node_tree, value);
                let l_type = self.type_expr_node(node_tree, target);

                // TODO: make sure l_type is assignale (e.g. has a concrete location in memory)
                match match_types(&l_type, &r_type) {
                    TypeMatch::ExactType => {}
                    TypeMatch::Inference(ts) => {
                        node_tree.type_map.set(value, ts.clone());
                        node_tree.type_map.set(target, ts);
                    }
                    TypeMatch::InferenceFailed => panic!("could not infere a type in this context"),
                    TypeMatch::Mismatch => {
                        panic!("invalid type for assignment {:?} = {:?}", &l_type, &r_type)
                    }
                }
            }
            Node::Invalid
            | Node::Pattern(_)
            | Node::IntLiteral(_)
            | Node::UIntLiteral(_)
            | Node::FloatLiteral(_)
            | Node::StringLiteral(_)
            | Node::BoolLiteral(_)
            | Node::Range { .. }
            | Node::Identifier { .. }
            | Node::Unary { .. }
            | Node::Binary { .. }
            | Node::Call { .. }
            | Node::Index { .. }
            | Node::EnumConstructor { .. }
            | Node::StructConstructor { .. }
            | Node::StructConstructorField { .. }
            | Node::FieldAccess { .. }
            | Node::MetaType
            | Node::Alloc { .. } => {
                self.type_expr_node(node_tree, node_id);
            }
        }
    }

    fn type_expr_node(&mut self, node_tree: &mut NodeTree, node_id: NodeID) -> TypeSpec {
        let node = node_tree
            .get_node(node_id)
            .expect("failed to find node for type checking");
        let node = node.clone();

        match node {
            Node::Invalid => {
                let ts = TypeSpec::Panic;
                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            Node::IntLiteral(i) => {
                let ts = TypeSpec::IntLiteral(i);
                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            Node::UIntLiteral(i) => {
                let ts = TypeSpec::UIntLiteral(i);
                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            Node::FloatLiteral(f) => {
                let ts = TypeSpec::FloatLiteral(f);
                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            Node::StringLiteral(_) => {
                let ts = TypeSpec::String;
                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            Node::BoolLiteral(_) => {
                let ts = TypeSpec::Bool;
                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            Node::Binary {
                left,
                operator,
                right,
            } => {
                let l_type = self.type_expr_node(node_tree, left);
                let r_type = self.type_expr_node(node_tree, right);

                let operand_type = match match_types(&l_type, &r_type) {
                    TypeMatch::ExactType => l_type,
                    TypeMatch::Inference(ts) => {
                        node_tree.type_map.set(left, ts.clone());
                        node_tree.type_map.set(right, ts.clone());
                        ts
                    }
                    TypeMatch::InferenceFailed => {
                        panic!("failed to infer type for binary expression")
                    }
                    TypeMatch::Mismatch => panic!(
                        "types {:?} and {:?} do not match in binary expression",
                        l_type, r_type
                    ),
                };

                let result_type = match operator {
                    BinaryOp::Equal
                    | BinaryOp::NotEqual
                    | BinaryOp::LessThan
                    | BinaryOp::LessThanOrEqual
                    | BinaryOp::GreaterThan
                    | BinaryOp::GreaterThanOrEqual
                    | BinaryOp::LogicalAnd
                    | BinaryOp::LogicalOr => TypeSpec::Bool,
                    _ => operand_type,
                };

                node_tree.type_map.add(node_id, result_type.clone());
                result_type
            }
            Node::Identifier { .. } => {
                // Identifiers should get their types from context (e.g. from assignment or
                // function params) if we don't have a type yet, assume any type could be valid
                match node_tree.type_map.get(node_id) {
                    Some(ts) => ts.clone(),
                    None => {
                        node_tree.type_map.add(node_id, TypeSpec::Any);
                        TypeSpec::Any
                    }
                }
            }
            Node::EnumConstructor {
                target,
                variant,
                payload,
            } => match target {
                Some(t) => {
                    let target_ts = self.type_expr_node(node_tree, t);
                    match find_variant(&target_ts, variant) {
                        FoundVariant::Some(v) => match (payload, &v.payload) {
                            (Some(p), Some(ts)) => {
                                let pay_id = self.type_expr_node(node_tree, p);
                                match match_types(&pay_id, ts) {
                                    TypeMatch::ExactType => {
                                        node_tree.type_map.add(node_id, target_ts.clone());
                                        target_ts
                                    }
                                    TypeMatch::Inference(ts) => {
                                        node_tree.type_map.set(p, ts);
                                        node_tree.type_map.add(node_id, target_ts.clone());
                                        target_ts
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
                            (Some(_), None) => {
                                panic!("variant was not expecing a payload but one was given")
                            }
                            (None, None) => {
                                node_tree.type_map.add(node_id, target_ts.clone());
                                target_ts
                            }
                        },
                        FoundVariant::None => panic!("failed to find variant for enum type"),
                        FoundVariant::NotEnum => panic!("target type is not an enum"),
                    }
                }
                None => {
                    let payload = payload.map(|p| Box::new(self.type_expr_node(node_tree, p)));
                    let ts = TypeSpec::InferredEnumExpr(InferredEnumExpr {
                        variant_name: variant,
                        payload,
                    });
                    node_tree.type_map.add(node_id, ts.clone());
                    ts
                }
            },
            Node::StructConstructor { fields } => {
                let type_spec = node_tree
                    .type_map
                    .get(node_id)
                    .expect("failed to get type for struct")
                    .clone();
                let base_type = resolve_type(&type_spec);
                let struct_type = match base_type {
                    TypeSpec::Struct(ts) => ts,
                    _ => panic!("type spec for struct constructor must be a struct type"),
                };

                for (field, field_type) in fields.iter().zip(struct_type.fields.iter()) {
                    let got_type = self.type_expr_node(node_tree, *field);
                    let expect_type = &field_type.type_spec;
                    match match_types(expect_type, &got_type) {
                        TypeMatch::ExactType => {
                            // need to update the type for the inner field value as well
                            let value_id = match node_tree.get_node(*field).unwrap() {
                                Node::StructConstructorField { value, .. } => value,
                                _ => panic!("expected StructConstructorField"),
                            };
                            node_tree.type_map.set(*value_id, expect_type.clone());
                            node_tree.type_map.add(*field, expect_type.clone());
                        }
                        TypeMatch::Inference(ts) => {
                            // need to update the type for the inner field value as well
                            let value_id = match node_tree.get_node(*field).unwrap() {
                                Node::StructConstructorField { value, .. } => value,
                                _ => panic!("expected StructConstructorField"),
                            };
                            node_tree.type_map.set(*value_id, expect_type.clone());
                            node_tree.type_map.add(*field, ts);
                        }
                        TypeMatch::InferenceFailed => {
                            panic!("failed to infer type for field value")
                        }
                        TypeMatch::Mismatch => panic!("field type was incorrect"),
                    }
                }

                type_spec.clone()
            }
            Node::StructConstructorField { value, .. } => self.type_expr_node(node_tree, value),
            Node::FieldAccess { target, field } => {
                let target_type = self.type_expr_node(node_tree, target);
                match resolve_type(&target_type) {
                    TypeSpec::Struct(ts) => {
                        let mut found_field = None;
                        for f in &ts.fields {
                            if f.name == field {
                                found_field = Some(f);
                                break;
                            }
                        }

                        match found_field {
                            Some(ok) => {
                                node_tree.type_map.add(node_id, ok.type_spec.clone());
                                ok.type_spec.clone()
                            }
                            None => panic!("field does not exist on this type"),
                        }
                    }
                    // This is only technically true till we implement methods on type at with
                    // point I probably need to rethink more than just this code.
                    _ => panic!("can not access a field on a non struct type"),
                }
            }
            Node::Unary { operator, operand } => {
                let operand_type = self.type_expr_node(node_tree, operand);
                match operator {
                    UnaryOp::AddressOf => {
                        // &T -> Pointer(T)
                        let ts = TypeSpec::Pointer(Box::new(operand_type));
                        node_tree.type_map.add(node_id, ts.clone());
                        ts
                    }
                    UnaryOp::Dereference => {
                        // *Pointer(T) -> T
                        let ts = match operand_type {
                            TypeSpec::Pointer(inner) => *inner,
                            _ => panic!("cannot dereference non-pointer type"),
                        };
                        node_tree.type_map.add(node_id, ts.clone());
                        ts
                    }
                    UnaryOp::Not => {
                        // !T -> T
                        let ts = operand_type;
                        if !is_bool_type(&ts) {
                            panic!("! can only be used on boolean types")
                        }
                        node_tree.type_map.add(node_id, ts.clone());
                        ts
                    }
                    UnaryOp::Negate => {
                        // -T -> T
                        let ts = operand_type;
                        if !is_numeric_type(&ts) {
                            panic!("! can only be used on boolean types")
                        }
                        node_tree.type_map.add(node_id, ts.clone());
                        ts
                    }
                    UnaryOp::Positive => {
                        // +T -> T
                        let ts = operand_type;
                        if !is_numeric_type(&ts) {
                            panic!("! can only be used on boolean types")
                        }
                        node_tree.type_map.add(node_id, ts.clone());
                        ts
                    }
                }
            }
            Node::Call { func, args } => {
                let func_type = self.type_expr_node(node_tree, func);

                let func_type = match func_type {
                    TypeSpec::Function(ft) => ft,
                    _ => panic!("cannot call non-function type"),
                };

                if func_type.params.len() != args.len() {
                    panic!(
                        "expected {} argument(s) but got {}",
                        func_type.params.len(),
                        args.len()
                    );
                }

                for (param, arg) in func_type.params.iter().zip(args.iter()) {
                    let arg_type = self.type_expr_node(node_tree, *arg);
                    match match_types(param, &arg_type) {
                        TypeMatch::ExactType => {}
                        TypeMatch::Inference(ts) => node_tree.type_map.set(*arg, ts),
                        TypeMatch::InferenceFailed => {
                            panic!("failed to infer type for argument")
                        }
                        TypeMatch::Mismatch => panic!(
                            "argument type does not match expected paramater type {:?} {:?}",
                            param, arg_type
                        ),
                    }
                }

                let ret_type = *func_type.return_type;
                node_tree.type_map.add(node_id, ret_type.clone());
                ret_type
            }
            Node::Index { target, index } => {
                let index_type = self.type_expr_node(node_tree, index);
                if !is_natural_number(&index_type) {
                    panic!("can only index expressions using natural numbers")
                }

                let target_type = self.type_expr_node(node_tree, target);
                match target_type {
                    TypeSpec::Array(arr) => *arr.type_spec,
                    TypeSpec::Slice(elem_type) => *elem_type,
                    _ => panic!("cannot index non-array/slice type"),
                }
            }
            Node::MetaType => TypeSpec::Struct(StructType {
                fields: vec![
                    StructTypeField {
                        name: str_store::SIZEOF,
                        // TODO: this should actually be a usize instead of a u64 since we need to support
                        // 32-bit systems as well
                        type_spec: TypeSpec::UInt64,
                    },
                    StructTypeField {
                        name: str_store::ALIGNOF,
                        // TODO: this should actually be a usize instead of a u64 since we need to support
                        // 32-bit systems as well
                        type_spec: TypeSpec::UInt64,
                    },
                    StructTypeField {
                        name: str_store::METAFLAGS,
                        // TODO: this should actually be a usize instead of a u64 since we need to support
                        // 32-bit systems as well
                        type_spec: TypeSpec::UInt64,
                    },
                ],
            }),
            Node::Range { .. } => todo!("Range expressions are not yet supported"),
            Node::Pattern(_) => panic!("Invalid position for a patter node"),
            Node::Alloc { meta_type, .. } => {
                let type_spec = self.type_expr_node(node_tree, meta_type);
                match type_spec {
                    // TODO: meta type's should actually probably be a named type at some point.
                    // For now though I'll just leave them as these sort of weird internal types
                    TypeSpec::Struct(_) => {
                        eprintln!("TODO: need to validate this is actually the meta type")
                    }
                    _ => panic!("alloc requires a meta type expression"),
                }

                TypeSpec::UnsafePtr
            }
            Node::Defer { .. }
            | Node::If { .. }
            | Node::Loop { .. }
            | Node::Break
            | Node::Continue
            | Node::VarDecl { .. }
            | Node::Match { .. }
            | Node::Return { .. }
            | Node::MatchArm { .. }
            | Node::Free { .. }
            | Node::TypeDecl { .. }
            | Node::Block { .. }
            | Node::FunctionDecl { .. }
            | Node::Assign { .. } => panic!("can not type check a statement as an expression"),
        }
    }

    fn type_pattern_node(&self, node_tree: &mut NodeTree, node_id: NodeID) -> TypeSpec {
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
            .expect("patterns can only appear inside match statments")
            .clone();

        match pat {
            PatternNode::IntLiteral(i) => {
                let ts = TypeSpec::IntLiteral(i);
                let ts = match match_types(&target_type, &ts) {
                    TypeMatch::ExactType => ts,
                    TypeMatch::Inference(ts) => ts,
                    TypeMatch::InferenceFailed => {
                        panic!(
                            "Inference Failed: invalid type for pattern {:?} vs {:?}",
                            target_type, ts
                        )
                    }
                    TypeMatch::Mismatch => {
                        panic!(
                            "Type Missmatch: invalid type for pattern {:?} vs {:?}",
                            target_type, ts
                        )
                    }
                };

                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            PatternNode::UIntLiteral(i) => {
                let ts = TypeSpec::UIntLiteral(i);
                let ts = match match_types(&target_type, &ts) {
                    TypeMatch::ExactType => ts,
                    TypeMatch::Inference(ts) => ts,
                    TypeMatch::InferenceFailed => {
                        panic!(
                            "Inference Failed: invalid type for pattern {:?} vs {:?}",
                            target_type, ts
                        )
                    }
                    TypeMatch::Mismatch => {
                        panic!(
                            "Type Missmatch: invalid type for pattern {:?} vs {:?}",
                            target_type, ts
                        )
                    }
                };

                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            PatternNode::StringLiteral(_) => {
                let ts = TypeSpec::String;
                let ts = match match_types(&target_type, &ts) {
                    TypeMatch::ExactType => ts,
                    TypeMatch::Inference(ts) => ts,
                    TypeMatch::InferenceFailed => {
                        panic!("invalid type for pattern {:?} vs {:?}", target_type, ts)
                    }
                    TypeMatch::Mismatch => {
                        panic!("invalid type for pattern {:?} vs {:?}", target_type, ts)
                    }
                };

                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            PatternNode::BoolLiteral(_) => {
                let ts = TypeSpec::Bool;
                let ts = match match_types(&target_type, &ts) {
                    TypeMatch::ExactType => ts,
                    TypeMatch::Inference(ts) => ts,
                    TypeMatch::InferenceFailed => panic!("invalid type for pattern"),
                    TypeMatch::Mismatch => panic!("invalid type for patter"),
                };

                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            PatternNode::FloatLiteral(f) => {
                let ts = TypeSpec::FloatLiteral(f);
                let ts = match match_types(&target_type, &ts) {
                    TypeMatch::ExactType => ts,
                    TypeMatch::Inference(ts) => ts,
                    TypeMatch::InferenceFailed => panic!("invalid type for pattern"),
                    TypeMatch::Mismatch => panic!("invalid type for patter"),
                };

                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            PatternNode::TypeSpec(ts) => {
                // type must already be set so we check that here to return
                let type_spec = node_tree
                    .type_map
                    .get(node_id)
                    .expect("missing type for type spec pattern")
                    .clone();

                match match_types(&target_type, &type_spec) {
                    TypeMatch::ExactType => {}
                    TypeMatch::Inference(_) => panic!(
                        "this should not occur since neither type spec patterns, nor match expressions are allowed to be inferrable"
                    ),
                    TypeMatch::InferenceFailed | TypeMatch::Mismatch => panic!(
                        "type spec pattern {:?} is not compatible with match target type {:?}",
                        type_spec, target_type
                    ),
                }

                if let Some(pay) = ts.payload {
                    // the payload of a type spec get's that same typespec
                    node_tree.type_map.add(pay, type_spec.clone())
                }

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

                        // If we know the exact type of the enum then we need to make sure we type
                        // the payload based on the variant that matches
                        if let Some(pay) = e.payload {
                            match find_variant(&ts, e.variant) {
                                FoundVariant::Some(v) => match &v.payload {
                                    Some(p) => node_tree.type_map.add(pay, p.clone()),
                                    None => panic!("this variant does not have a payload"),
                                },
                                FoundVariant::None => panic!(
                                    "unknown variant name on enum type, this should never happen"
                                ),
                                FoundVariant::NotEnum => panic!("invalid type inferred for enum"),
                            }
                        }

                        ts
                    }
                    None => TypeSpec::InferredEnumPat(InferredEnumPat {
                        variant_name: e.variant,
                        payload: e.payload,
                    }),
                };

                let ts = match match_types(&target_type, &ts) {
                    TypeMatch::ExactType => ts,
                    TypeMatch::Inference(ts) => {
                        // if we inferred a type for the enum and the variant has a payload we need
                        // to make sure we extract the variant and set the type on the payload here
                        if let Some(pay) = e.payload {
                            match find_variant(&ts, e.variant) {
                                FoundVariant::Some(v) => match &v.payload {
                                    Some(p) => node_tree.type_map.add(pay, p.clone()),
                                    None => panic!("this variant does not have a payload"),
                                },
                                FoundVariant::None => panic!(
                                    "unknown variant name on enum type, this should never happen"
                                ),
                                FoundVariant::NotEnum => panic!("invalid type inferred for enum"),
                            }
                        }

                        ts
                    }
                    TypeMatch::InferenceFailed => panic!("invalid type for pattern"),
                    TypeMatch::Mismatch => panic!("invalid type for patter"),
                };
                node_tree.type_map.add(node_id, ts.clone());
                ts
            }
            PatternNode::Default(d) => {
                node_tree.type_map.add(node_id, target_type.clone());

                if let Some(pay) = d.payload {
                    // The payload on a default type gets the same type as the target
                    node_tree.type_map.add(pay, target_type.clone());
                }
                target_type
            }
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
            | TypeSpec::UIntLiteral(_)
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
pub fn resolve_type(ts: &TypeSpec) -> &TypeSpec {
    match ts {
        TypeSpec::Named(t) => resolve_type(&t.type_spec),
        _ => ts,
    }
}

enum FoundVariant<'a> {
    Some(&'a EnumVariant),
    None,
    NotEnum,
}

fn find_variant<'a>(type_spec: &'a TypeSpec, variant_name: StrID) -> FoundVariant<'a> {
    let type_spec = resolve_type(type_spec);
    match type_spec {
        TypeSpec::Enum(e) => {
            for variant in &e.variants {
                if variant_name == variant.name {
                    return FoundVariant::Some(variant);
                }
            }
        }
        _ => return FoundVariant::NotEnum,
    }
    FoundVariant::None
}

enum TypeMatch {
    ExactType,
    Inference(TypeSpec),
    InferenceFailed,
    Mismatch,
}

fn match_types(a: &TypeSpec, b: &TypeSpec) -> TypeMatch {
    if a == b {
        return match a {
            TypeSpec::IntLiteral(_) => TypeMatch::Inference(TypeSpec::Int64),
            TypeSpec::FloatLiteral(_) => TypeMatch::Inference(TypeSpec::Float64),
            TypeSpec::InferredEnumExpr(_) => TypeMatch::InferenceFailed,
            TypeSpec::InferredEnumPat(_) => TypeMatch::InferenceFailed,
            TypeSpec::Any => TypeMatch::InferenceFailed,
            _ => TypeMatch::ExactType,
        };
    }

    if a == &TypeSpec::Panic || b == &TypeSpec::Panic {
        // Because panics cause the full stack to unwind and normal controll flow to terminate we
        // consider the panic type to match all other types exactly.
        return TypeMatch::ExactType;
    }

    match (a, b) {
        // if both the left and right hand side are typed as literals, just conver the type to an
        // sensible concereet type
        (TypeSpec::IntLiteral(_), TypeSpec::IntLiteral(_)) => TypeMatch::Inference(TypeSpec::Int64),
        (TypeSpec::UIntLiteral(_), TypeSpec::UIntLiteral(_)) => {
            TypeMatch::Inference(TypeSpec::UInt64)
        }
        (TypeSpec::FloatLiteral(_), TypeSpec::FloatLiteral(_)) => {
            TypeMatch::Inference(TypeSpec::Float64)
        }
        (TypeSpec::InferredEnumExpr(_), TypeSpec::InferredEnumExpr(_)) => {
            TypeMatch::InferenceFailed
        }
        (TypeSpec::InferredEnumPat(_), TypeSpec::InferredEnumPat(_)) => TypeMatch::InferenceFailed,

        (TypeSpec::IntLiteral(_), TypeSpec::Any) => TypeMatch::Inference(TypeSpec::Int64),
        (TypeSpec::Any, TypeSpec::IntLiteral(_)) => TypeMatch::Inference(TypeSpec::Int64),
        (TypeSpec::UIntLiteral(_), TypeSpec::Any) => TypeMatch::Inference(TypeSpec::UInt64),
        (TypeSpec::Any, TypeSpec::UIntLiteral(_)) => TypeMatch::Inference(TypeSpec::UInt64),
        // an integer literal matched against a float literal resolves to float64
        (TypeSpec::IntLiteral(_), TypeSpec::FloatLiteral(_)) => {
            TypeMatch::Inference(TypeSpec::Float64)
        }
        (TypeSpec::FloatLiteral(_), TypeSpec::IntLiteral(_)) => {
            TypeMatch::Inference(TypeSpec::Float64)
        }
        (TypeSpec::IntLiteral(i), inner_b) => match inner_b {
            // i is an i64 here so it can never fail inference
            TypeSpec::Int64 => TypeMatch::Inference(TypeSpec::Int64),
            TypeSpec::Int32 => match_int(i, i32::MIN as i64, i32::MAX as i64, TypeSpec::Int32),
            TypeSpec::Int16 => match_int(i, i16::MIN as i64, i16::MAX as i64, TypeSpec::Int16),
            TypeSpec::Int8 => match_int(i, i8::MIN as i64, i8::MAX as i64, TypeSpec::Int8),
            TypeSpec::UInt64 => match *i >= 0 {
                true => TypeMatch::Inference(TypeSpec::UInt64),
                false => TypeMatch::InferenceFailed,
            },
            TypeSpec::UInt32 => match_int(i, 0, u32::MAX as i64, TypeSpec::UInt32),
            TypeSpec::UInt16 => match_int(i, 0, u16::MAX as i64, TypeSpec::UInt16),
            TypeSpec::UInt8 => match_int(i, 0, u8::MAX as i64, TypeSpec::UInt8),
            // integer literals can be coerced into float types
            TypeSpec::Float64 => match_float(&(*i as f64), f64::MIN, f64::MAX, TypeSpec::Float64),
            TypeSpec::Float32 => match_float(
                &(*i as f64),
                f32::MIN as f64,
                f32::MAX as f64,
                TypeSpec::Float32,
            ),
            TypeSpec::UIntLiteral(u) => {
                if *u < i64::MAX as u64 && *i >= 0 {
                    TypeMatch::Inference(TypeSpec::Int64)
                } else {
                    TypeMatch::InferenceFailed
                }
            }
            _ => TypeMatch::Mismatch,
        },
        (inner_a, TypeSpec::IntLiteral(i)) => match inner_a {
            // i is an i64 here so it can never fail inference
            TypeSpec::Int64 => TypeMatch::Inference(TypeSpec::Int64),
            TypeSpec::Int32 => match_int(i, i32::MIN as i64, i32::MAX as i64, TypeSpec::Int32),
            TypeSpec::Int16 => match_int(i, i16::MIN as i64, i16::MAX as i64, TypeSpec::Int16),
            TypeSpec::Int8 => match_int(i, i8::MIN as i64, i8::MAX as i64, TypeSpec::Int8),
            TypeSpec::UInt64 => match *i >= 0 {
                true => TypeMatch::Inference(TypeSpec::UInt64),
                false => TypeMatch::InferenceFailed,
            },
            TypeSpec::UInt32 => match_int(i, 0, u32::MAX as i64, TypeSpec::UInt32),
            TypeSpec::UInt16 => match_int(i, 0, u16::MAX as i64, TypeSpec::UInt16),
            TypeSpec::UInt8 => match_int(i, 0, u8::MAX as i64, TypeSpec::UInt8),
            // integer literals can be coerced into float types
            TypeSpec::Float64 => match_float(&(*i as f64), f64::MIN, f64::MAX, TypeSpec::Float64),
            TypeSpec::Float32 => match_float(
                &(*i as f64),
                f32::MIN as f64,
                f32::MAX as f64,
                TypeSpec::Float32,
            ),
            TypeSpec::UIntLiteral(u) => {
                if *u < i64::MAX as u64 && *i >= 0 {
                    TypeMatch::Inference(TypeSpec::Int64)
                } else {
                    TypeMatch::InferenceFailed
                }
            }
            _ => TypeMatch::Mismatch,
        },
        (TypeSpec::UIntLiteral(i), inner_b) => match inner_b {
            TypeSpec::Int64 => match *i <= i64::MAX as u64 {
                true => TypeMatch::Inference(TypeSpec::Int64),
                false => TypeMatch::InferenceFailed,
            },
            // these are technically unreachable but they are included for correctness
            TypeSpec::Int32 => match_uint(i, i32::MAX as u64, TypeSpec::Int32),
            TypeSpec::Int16 => match_uint(i, i16::MAX as u64, TypeSpec::Int16),
            TypeSpec::Int8 => match_uint(i, i8::MAX as u64, TypeSpec::Int8),
            // UIntLiterals will always fit in a u64
            TypeSpec::UInt64 => TypeMatch::Inference(TypeSpec::UInt64),
            TypeSpec::UInt32 => match_uint(i, u32::MAX as u64, TypeSpec::UInt32),
            TypeSpec::UInt16 => match_uint(i, u16::MAX as u64, TypeSpec::UInt16),
            TypeSpec::UInt8 => match_uint(i, u8::MAX as u64, TypeSpec::UInt8),
            // integer literals can be coerced into float types
            TypeSpec::Float64 => match_float(&(*i as f64), f64::MIN, f64::MAX, TypeSpec::Float64),
            TypeSpec::Float32 => match_float(
                &(*i as f64),
                f32::MIN as f64,
                f32::MAX as f64,
                TypeSpec::Float32,
            ),
            TypeSpec::IntLiteral(other) => {
                if *i < i64::MAX as u64 && *other >= 0 {
                    TypeMatch::Inference(TypeSpec::Int64)
                } else {
                    TypeMatch::InferenceFailed
                }
            }
            _ => TypeMatch::Mismatch,
        },
        (inner_a, TypeSpec::UIntLiteral(i)) => match inner_a {
            TypeSpec::Int64 => match *i <= i64::MAX as u64 {
                true => TypeMatch::Inference(TypeSpec::Int64),
                false => TypeMatch::InferenceFailed,
            },
            // these are technically unreachable but they are included for correctness
            TypeSpec::Int32 => match_uint(i, i32::MAX as u64, TypeSpec::Int32),
            TypeSpec::Int16 => match_uint(i, i16::MAX as u64, TypeSpec::Int16),
            TypeSpec::Int8 => match_uint(i, i8::MAX as u64, TypeSpec::Int8),
            // UIntLiterals will always fit in a u64
            TypeSpec::UInt64 => TypeMatch::Inference(TypeSpec::UInt64),
            TypeSpec::UInt32 => match_uint(i, u32::MAX as u64, TypeSpec::UInt32),
            TypeSpec::UInt16 => match_uint(i, u16::MAX as u64, TypeSpec::UInt16),
            TypeSpec::UInt8 => match_uint(i, u8::MAX as u64, TypeSpec::UInt8),
            // integer literals can be coerced into float types
            TypeSpec::Float64 => match_float(&(*i as f64), f64::MIN, f64::MAX, TypeSpec::Float64),
            TypeSpec::Float32 => match_float(
                &(*i as f64),
                f32::MIN as f64,
                f32::MAX as f64,
                TypeSpec::Float32,
            ),
            TypeSpec::IntLiteral(other) => {
                if *i < i64::MAX as u64 && *other >= 0 {
                    TypeMatch::Inference(TypeSpec::Int64)
                } else {
                    TypeMatch::InferenceFailed
                }
            }
            _ => TypeMatch::Mismatch,
        },
        (TypeSpec::FloatLiteral(_), TypeSpec::Any) => TypeMatch::Inference(TypeSpec::Float64),
        (TypeSpec::Any, TypeSpec::FloatLiteral(_)) => TypeMatch::Inference(TypeSpec::Float64),
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
        (TypeSpec::InferredEnumExpr(inner_a), inner_b) => match_enum_expr(inner_b, inner_a),
        (inner_a, TypeSpec::InferredEnumExpr(inner_b)) => match_enum_expr(inner_a, inner_b),
        (TypeSpec::InferredEnumPat(inner_a), inner_b) => match_enum_pat(inner_b, inner_a),
        (inner_a, TypeSpec::InferredEnumPat(inner_b)) => match_enum_pat(inner_a, inner_b),
        (TypeSpec::Any, inner_b) => TypeMatch::Inference(inner_b.clone()),
        (inner_a, TypeSpec::Any) => TypeMatch::Inference(inner_a.clone()),
        // unsafe pointers can be pattern matched into any pointer type so we consider this to be
        // an exact match for the purposes of the type checker
        (TypeSpec::UnsafePtr, TypeSpec::Pointer(_)) => TypeMatch::ExactType,
        _ => TypeMatch::Mismatch,
    }
}

fn match_int(target: &i64, min: i64, max: i64, ts: TypeSpec) -> TypeMatch {
    if *target >= min && *target <= max {
        TypeMatch::Inference(ts)
    } else {
        TypeMatch::InferenceFailed
    }
}
fn match_uint(target: &u64, max: u64, ts: TypeSpec) -> TypeMatch {
    if *target <= max {
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

fn match_enum_expr(known: &TypeSpec, unknown: &InferredEnumExpr) -> TypeMatch {
    match find_variant(known, unknown.variant_name) {
        FoundVariant::Some(v) => match (&v.payload, &unknown.payload) {
            (Some(a), Some(b)) => {
                if a == b.deref() {
                    TypeMatch::Inference(known.clone())
                } else {
                    TypeMatch::Mismatch
                }
            }
            (None, Some(_)) => TypeMatch::Mismatch,
            (Some(_), None) => TypeMatch::Mismatch,
            (None, None) => TypeMatch::Inference(known.clone()),
        },
        FoundVariant::None | FoundVariant::NotEnum => TypeMatch::Mismatch,
    }
}

fn match_enum_pat(known: &TypeSpec, unknown: &InferredEnumPat) -> TypeMatch {
    match find_variant(known, unknown.variant_name) {
        FoundVariant::Some(v) => match (&v.payload, &unknown.payload) {
            (Some(_), Some(_)) => TypeMatch::Inference(known.clone()),
            (Some(_), None) => TypeMatch::Mismatch,
            (None, Some(_)) => TypeMatch::Mismatch,
            (None, None) => TypeMatch::Inference(known.clone()),
        },
        FoundVariant::None | FoundVariant::NotEnum => TypeMatch::Mismatch,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::{EnumType, EnumVariant, NamedType};
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

    // Helper: build an InferredEnumExpr without a payload
    fn inferred_enum_expr(id: usize) -> InferredEnumExpr {
        InferredEnumExpr {
            variant_name: StrID::from_usize(id),
            payload: None,
        }
    }

    // Helper: built an InferredEnumExpr with a payload
    fn inferred_enum_expr_with(id: usize, payload: TypeSpec) -> InferredEnumExpr {
        InferredEnumExpr {
            variant_name: StrID::from_usize(id),
            payload: Some(Box::new(payload)),
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
    fn match_types_int_literal_with_float64_infers_float64() {
        assert!(matches!(
            match_types(&TypeSpec::IntLiteral(42), &TypeSpec::Float64),
            TypeMatch::Inference(TypeSpec::Float64)
        ));
        assert!(matches!(
            match_types(&TypeSpec::Float64, &TypeSpec::IntLiteral(42)),
            TypeMatch::Inference(TypeSpec::Float64)
        ));
    }

    #[test]
    fn match_types_int_literal_with_float32_infers_float32() {
        assert!(matches!(
            match_types(&TypeSpec::IntLiteral(42), &TypeSpec::Float32),
            TypeMatch::Inference(TypeSpec::Float32)
        ));
        assert!(matches!(
            match_types(&TypeSpec::Float32, &TypeSpec::IntLiteral(42)),
            TypeMatch::Inference(TypeSpec::Float32)
        ));
    }

    #[test]
    fn match_types_int_literal_and_float_literal_infer_float64() {
        assert!(matches!(
            match_types(&TypeSpec::IntLiteral(5), &TypeSpec::FloatLiteral(1.5)),
            TypeMatch::Inference(TypeSpec::Float64)
        ));
        assert!(matches!(
            match_types(&TypeSpec::FloatLiteral(1.5), &TypeSpec::IntLiteral(5)),
            TypeMatch::Inference(TypeSpec::Float64)
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
        let ts = TypeSpec::InferredEnumExpr(inferred_enum_expr(1));
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
        let unknown = TypeSpec::InferredEnumExpr(inferred_enum_expr(v_id));
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
        let unknown = inferred_enum_expr(v_id);
        assert!(matches!(
            match_enum_expr(&known, &unknown),
            TypeMatch::Inference(_)
        ));
    }

    #[test]
    fn match_enum_unknown_variant_is_mismatch() {
        let known = enum_type(vec![variant(1)]);
        let unknown = inferred_enum_expr(99);
        assert!(matches!(
            match_enum_expr(&known, &unknown),
            TypeMatch::Mismatch
        ));
    }

    #[test]
    fn match_enum_matching_variant_with_matching_payload() {
        let v_id = 1;
        let known = enum_type(vec![variant_with(v_id, TypeSpec::Int64)]);
        let unknown = inferred_enum_expr_with(v_id, TypeSpec::Int64);
        assert!(matches!(
            match_enum_expr(&known, &unknown),
            TypeMatch::Inference(_)
        ));
    }

    #[test]
    fn match_enum_mismatched_payload_is_mismatch() {
        let v_id = 1;
        let known = enum_type(vec![variant_with(v_id, TypeSpec::Int64)]);
        let unknown = inferred_enum_expr_with(v_id, TypeSpec::Bool);
        assert!(matches!(
            match_enum_expr(&known, &unknown),
            TypeMatch::Mismatch
        ));
    }

    #[test]
    fn match_enum_expected_payload_but_none_given_is_mismatch() {
        let v_id = 1;
        let known = enum_type(vec![variant_with(v_id, TypeSpec::Int64)]);
        let unknown = inferred_enum_expr(v_id); // no payload
        assert!(matches!(
            match_enum_expr(&known, &unknown),
            TypeMatch::Mismatch
        ));
    }

    #[test]
    fn match_enum_unexpected_payload_is_mismatch() {
        let v_id = 1;
        let known = enum_type(vec![variant(v_id)]); // no payload on known variant
        let unknown = inferred_enum_expr_with(v_id, TypeSpec::Int64);
        assert!(matches!(
            match_enum_expr(&known, &unknown),
            TypeMatch::Mismatch
        ));
    }

    #[test]
    fn match_enum_non_enum_known_is_mismatch() {
        let unknown = inferred_enum_expr(1);
        assert!(matches!(
            match_enum_expr(&TypeSpec::Int64, &unknown),
            TypeMatch::Mismatch
        ));
        assert!(matches!(
            match_enum_expr(&TypeSpec::Bool, &unknown),
            TypeMatch::Mismatch
        ));
    }

    #[test]
    fn match_enum_resolves_named_enum_alias() {
        let v_id = 1;
        let known = named(enum_type(vec![variant(v_id)]));
        let unknown = inferred_enum_expr(v_id);
        assert!(matches!(
            match_enum_expr(&known, &unknown),
            TypeMatch::Inference(_)
        ));
    }
}

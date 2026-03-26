mod builder;

use std::collections::BTreeMap;

use builder::FunctionBuilder;

use crate::ast::{BinaryOp, UnaryOp};
use crate::hir::{self, Node, NodeID, PatternNode};
use crate::mir::{
    BlockId, ConstValue, Global, GlobalId, MirFunction, MirModule, Place, PlaceBase, Projection,
    SwitchArm, TagSize, Terminator, TypeSpec, ValueId,
};
use crate::noder::{NodeTree, typer};
use crate::str_store::{self, StrID};

// Blocker lowers an HIR tree in it's entirety into a valid MirModule
pub struct Blocker<'a> {
    globals: Vec<Global>, // Indexed by GlobalId
    global_map: BTreeMap<NodeID, GlobalId>,
    node_tree: &'a NodeTree,
    fn_builder: FunctionBuilder,
}

impl<'a> Blocker<'a> {
    pub fn new(node_tree: &'a NodeTree) -> Self {
        // create the init function builder to start
        let fn_builder = FunctionBuilder::new(str_store::INIT, TypeSpec::Unit);
        Blocker {
            globals: vec![],
            global_map: BTreeMap::new(),
            node_tree,
            fn_builder,
        }
    }

    pub fn build_module(mut self) -> MirModule {
        let mut init_block_id = self.fn_builder.add_block();

        for node_id in &self.node_tree.roots {
            match self.block_init_statement(*node_id, init_block_id) {
                Some(block) => init_block_id = block,
                None => break,
            }
        }

        // make sure we close the init block
        self.fn_builder
            .set_terminator(init_block_id, Terminator::Return { value: None });
        let init = self.fn_builder.build_mir_function();

        // block the rest of the functions
        let mut functions = vec![];
        for node_id in &self.node_tree.roots {
            if let Some(f) = self.block_function(*node_id) {
                functions.push(f);
            }
        }

        MirModule::new(self.globals, init, functions)
    }

    fn block_function(&mut self, node_id: NodeID) -> Option<MirFunction> {
        let node = match self.node_tree.get_node(node_id) {
            Some(n) => n,
            None => panic!("type checking unknown node"),
        };
        let node = node.clone();

        match node {
            Node::FunctionDecl {
                ident,
                params,
                body,
            } => {
                let name = self.get_ident_name(ident);
                let return_type = match self.node_tree.get_type(node_id) {
                    Some(ts) => lower_type_spec(ts),
                    None => panic!("missing type for function decl"),
                };

                let mut fn_builder = FunctionBuilder::new(name, return_type);
                for param in params {
                    let param = self
                        .node_tree
                        .get_node(param)
                        .expect("missing node for param");

                    let ident = match param {
                        Node::VarDecl { ident } => ident,
                        _ => panic!("param was not a VarDecl"),
                    };

                    let name = self.get_ident_name(*ident);

                    let ts = match self.node_tree.get_type(*ident) {
                        Some(ts) => lower_type_spec(ts),
                        None => panic!("missing type for function param"),
                    };

                    fn_builder.add_param(*ident, name, ts);
                }

                self.fn_builder = fn_builder;

                let block_id = self.fn_builder.add_block();
                self.block_statement(body, block_id);

                let mir_function = self.fn_builder.build_mir_function();
                Some(mir_function)
            }
            _ => None,
        }
    }

    fn get_ident_name(&self, node_id: NodeID) -> StrID {
        let node = self
            .node_tree
            .get_node(node_id)
            .expect("missing identifier node");

        match node {
            Node::Identifier { name, .. } => *name,
            _ => panic!("node was not an identifier"),
        }
    }

    fn block_init_statement(&mut self, node_id: NodeID, block_id: BlockId) -> Option<BlockId> {
        if self.fn_builder.is_block_closed(block_id) {
            // if the block is closed just skip all the remaining instructions as they are no longer
            // reachable, trying to adding them would cause a panic
            return None;
        }

        let node = match self.node_tree.get_node(node_id) {
            Some(n) => n,
            None => panic!("type checking unknown node"),
        };
        let node = node.clone();

        match node {
            // Function declarations are skipped in the init function
            Node::FunctionDecl { .. } => Some(block_id),
            Node::TypeDecl { .. } => {
                // type decls do not need to be represented in the MIR
                Some(block_id)
            }
            Node::VarDecl { ident } => {
                let name = self.get_ident_name(ident);
                let ts = self
                    .node_tree
                    .get_type(ident)
                    .expect("missing type for identifier");
                let ts = lower_type_spec(ts);

                self.add_global(ident, name, ts);

                Some(block_id)
            }
            Node::Assign { target, value } => {
                let global_id = *self
                    .global_map
                    .get(&target)
                    .expect("assignment target is not a known global");
                let val = self.block_expression(block_id, value);
                self.fn_builder
                    .emit_store(block_id, Place::global(global_id), val);
                Some(block_id)
            }
            _ => panic!("invalid statement in global scope {:?}", node),
        }
    }

    fn add_global(&mut self, node: NodeID, name: StrID, type_spec: TypeSpec) {
        let global = Global { name, type_spec };
        self.globals.push(global);

        let global_id = GlobalId::from_usize(self.globals.len());
        self.global_map.insert(node, global_id);
    }

    /// Compute the Place for an lvalue expression — the storage location the node refers to.
    /// Does not emit any instructions; projections like Deref are added by the caller.
    fn block_lvalue(&mut self, block_id: BlockId, node_id: NodeID) -> Place {
        let node = self
            .node_tree
            .get_node(node_id)
            .expect("missing lvalue node")
            .clone();

        match node {
            Node::Identifier { .. } => {
                let base = match self.fn_builder.find_local(node_id) {
                    Some(local) => PlaceBase::Local(local),
                    None => {
                        let global_id = *self
                            .global_map
                            .get(&node_id)
                            .expect("lvalue is not a local or a global");
                        PlaceBase::Global(global_id)
                    }
                };
                Place {
                    base,
                    projections: vec![],
                }
            }
            Node::Unary {
                operator: UnaryOp::Dereference,
                operand,
            } => {
                let mut inner = self.block_lvalue(block_id, operand);
                inner.projections.push(Projection::Deref);
                inner
            }
            Node::FieldAccess { target, field } => {
                let mut inner = self.block_lvalue(block_id, target);
                let ts = self
                    .node_tree
                    .get_type(target)
                    .expect("missing type for field access target");

                let fields = match ts {
                    hir::TypeSpec::Struct(s) => &s.fields,
                    _ => panic!("invalid target type for field access"),
                };

                for (i, f) in fields.iter().enumerate() {
                    if field == f.name {
                        inner.projections.push(Projection::Field(i));
                        return inner;
                    }
                }

                panic!("unknown field in struct")
            }
            Node::Index { target, index } => {
                let mut inner = self.block_lvalue(block_id, target);
                let index = self.block_expression(block_id, index);
                inner.projections.push(Projection::Index(index));
                inner
            }
            _ => panic!("invalid lvalue node: {:?}", node),
        }
    }

    fn block_statement(&mut self, node_id: NodeID, block_id: BlockId) -> Option<BlockId> {
        if self.fn_builder.is_block_closed(block_id) {
            // if the block is closed just skip all the remaining instructions as they are no longer
            // reachable, trying to adding them would cause a panic
            return None;
        }

        let node = match self.node_tree.get_node(node_id) {
            Some(n) => n,
            None => panic!("type checking unknown node"),
        };
        let node = node.clone();

        match node {
            Node::Invalid => {
                // TODO: what are the args here? Should the 'Node::Invalid' have associated error
                // info so that we panic with a syntax error or something?
                self.fn_builder
                    .emit_call(block_id, str_store::PANIC, vec![], TypeSpec::Unit);
                self.fn_builder
                    .set_terminator(block_id, Terminator::Unreachable);

                // return a None because this block is closed and there's no more blocks that we know
                // about at this level
                None
            }
            Node::Block { statements } => {
                let mut current_block = block_id;
                for stmt in statements {
                    match self.block_statement(stmt, current_block) {
                        Some(b) => current_block = b,
                        None => return None,
                    }
                }

                Some(current_block)
            }
            Node::VarDecl { ident } => {
                let name = self.get_ident_name(ident);
                let ts = self
                    .node_tree
                    .get_type(ident)
                    .expect("missing type for identifier");
                let ts = lower_type_spec(ts);

                self.fn_builder.get_local(ident, name, ts);

                Some(block_id)
            }
            Node::Assign { target, value } => {
                let val = self.block_expression(block_id, value);
                let place = self.block_lvalue(block_id, target);
                self.fn_builder.emit_store(block_id, place, val);
                Some(block_id)
            }
            Node::Return { value } => {
                let ret = if let Some(v) = value {
                    let ret = self.block_expression(block_id, v);
                    Some(ret)
                } else {
                    None
                };

                self.fn_builder
                    .set_terminator(block_id, Terminator::Return { value: ret });

                None
            }
            Node::Defer { block } => {
                // TODO:
                Some(block_id)
            }
            Node::If {
                condition,
                then_block,
                else_block,
            } => {
                // the condition needs to be computed in the previous block, expressions can trigger
                // control flow so this should never create a new block
                let expr_value = self.block_expression(block_id, condition);

                let true_block_id = self.fn_builder.add_block();
                let merge_block_id = self.fn_builder.add_block();

                let block = self.block_statement(then_block, true_block_id);
                if let Some(b) = block {
                    self.fn_builder.set_terminator(
                        b,
                        Terminator::Jump {
                            target: merge_block_id,
                        },
                    )
                }

                let mut false_block_id = merge_block_id;
                if let Some(e) = else_block {
                    false_block_id = self.fn_builder.add_block();
                    let block = self.block_statement(e, false_block_id);
                    if let Some(b) = block {
                        self.fn_builder.set_terminator(
                            b,
                            Terminator::Jump {
                                target: merge_block_id,
                            },
                        );
                    }
                }

                self.fn_builder.set_terminator(
                    block_id,
                    Terminator::Branch {
                        cond: expr_value,
                        true_target: true_block_id,
                        false_target: false_block_id,
                    },
                );

                Some(merge_block_id)
            }
            Node::Match { target, arms } => {
                // check the type of the discriminant to figure out if we need to build a switch or a
                // series of if checks and jumps
                let discriminant_type = self
                    .node_tree
                    .get_type(target)
                    .expect("missing type for discriminant");

                let discriminant_type = typer::resolve_type(discriminant_type);
                match discriminant_type {
                    hir::TypeSpec::UInt8
                    | hir::TypeSpec::UInt16
                    | hir::TypeSpec::UInt32
                    | hir::TypeSpec::UInt64
                    | hir::TypeSpec::Int8
                    | hir::TypeSpec::Int16
                    | hir::TypeSpec::Int32
                    | hir::TypeSpec::Int64 => {
                        let merge_block = self.block_int_match(block_id, target, arms);
                        Some(merge_block)
                    }
                    hir::TypeSpec::Enum(_) => {
                        let merge_block = self.block_enum_match(block_id, target, arms);
                        Some(merge_block)
                    }
                    hir::TypeSpec::UnsafePtr => {
                        let merge_block = self.block_type_match(block_id, target, arms);
                        Some(merge_block)
                    }
                    _ => {
                        eprintln!("TODO: other expressions need to be converted into if blocks");
                        Some(block_id)
                    }
                }
            }
            Node::MatchArm { pattern, body } => {
                // TODO:
                Some(block_id)
            }
            Node::Call { .. } => {
                // Calls are technically expressions but because they can result in side effects they
                // need to be handled as statements as well
                self.block_expression(block_id, node_id);
                Some(block_id)
            }
            _ => panic!("node is not a valid statement {:?}", node),
        }
    }

    fn block_int_match(&mut self, block_id: BlockId, target: NodeID, arms: Vec<NodeID>) -> BlockId {
        let merge_block = self.fn_builder.add_block();
        let discriminant = self.block_expression(block_id, target);

        let mut match_arms = vec![];
        let mut default_arm = None;
        for arm in arms {
            let arm_block = self.fn_builder.add_block();
            let arm_node = self.node_tree.get_node(arm).expect("missing arm node");

            let (pattern, body) = match arm_node {
                Node::MatchArm { pattern, body } => (pattern, body),
                _ => panic!("expect a match arm"),
            };

            let pattern = self
                .node_tree
                .get_node(*pattern)
                .expect("missing pattern for match arm");
            let pattern = match pattern {
                Node::Pattern(p) => p,
                _ => panic!("pattern was an invalid node"),
            };

            match pattern {
                PatternNode::IntLiteral(i) => {
                    // Int literal patterns can never have a payload so this is pretty easy
                    let body = *body;
                    let i = *i;
                    let block = self.block_statement(body, arm_block);
                    if let Some(b) = block {
                        self.fn_builder.set_terminator(
                            b,
                            Terminator::Jump {
                                target: merge_block,
                            },
                        );
                    }

                    match_arms.push(SwitchArm {
                        target: ConstValue::ConstInt(i),
                        jump: arm_block,
                    });
                }
                PatternNode::Default(pat) => {
                    let body = *body;
                    if let Some(p) = pat.payload {
                        let name = self.get_ident_name(p);

                        let target_ts = self
                            .node_tree
                            .get_type(target)
                            .expect("missing type for match target");
                        let ts = lower_type_spec(target_ts);

                        let payload_local = self.fn_builder.get_local(p, name, ts);
                        self.fn_builder.emit_store(
                            arm_block,
                            Place::local(payload_local),
                            discriminant,
                        );
                    }

                    let block = self.block_statement(body, arm_block);
                    if let Some(b) = block {
                        self.fn_builder.set_terminator(
                            b,
                            Terminator::Jump {
                                target: merge_block,
                            },
                        );
                    }

                    default_arm = Some(arm_block);
                }
                _ => panic!("invalid pattern for match statement"),
            };

            if default_arm.is_some() {
                // once we have the default arm then no other patterns will ever match
                break;
            }
        }

        let default = match default_arm {
            Some(d) => d,
            None => {
                eprintln!(
                    "TODO: currently we don't always insist that pattern matching is exaustive"
                );
                merge_block
            }
        };

        self.fn_builder.set_terminator(
            block_id,
            Terminator::SwitchVariant {
                discriminant,
                default,
                arms: match_arms,
            },
        );

        merge_block
    }

    fn block_enum_match(
        &mut self,
        block_id: BlockId,
        target: NodeID,
        arms: Vec<NodeID>,
    ) -> BlockId {
        let merge_block = self.fn_builder.add_block();
        let target_id = self.block_expression(block_id, target);

        let target_ts = self
            .node_tree
            .get_type(target)
            .expect("missing type for match target");
        let ts = lower_type_spec(target_ts);

        let discriminant = self
            .fn_builder
            .emit_variant_get_tag(block_id, target_id, ts);

        let mut match_arms = vec![];
        let mut default_arm = None;
        for arm in arms {
            let arm_block = self.fn_builder.add_block();
            let arm_node = self.node_tree.get_node(arm).expect("missing arm node");

            let (pattern, body) = match arm_node {
                Node::MatchArm { pattern, body } => (pattern, body),
                _ => panic!("expected a match arm"),
            };

            let pattern = self
                .node_tree
                .get_node(*pattern)
                .expect("missing pattern for match arm");
            let pattern = match pattern {
                Node::Pattern(p) => p,
                _ => panic!("pattern was an invalid node"),
            };

            match pattern {
                PatternNode::EnumVariant(pat) => {
                    let variant_id = get_variant_id(target_ts, pat.variant);

                    let body = *body;
                    match pat.payload {
                        Some(p) => {
                            let ts = self
                                .node_tree
                                .get_type(p)
                                .expect("missing type spec for enum variant");
                            let ts = lower_type_spec(ts);

                            let payload_value = self.fn_builder.emit_variant_get_payload(
                                arm_block,
                                target_id,
                                variant_id.clone(),
                                ts.clone(),
                            );

                            let name = self.get_ident_name(p);
                            let payload_local = self.fn_builder.get_local(p, name, ts);
                            self.fn_builder.emit_store(
                                arm_block,
                                Place::local(payload_local),
                                payload_value,
                            );
                        }
                        None => {
                            // nothing to do because there's no payload to set up into a local
                        }
                    }

                    let block = self.block_statement(body, arm_block);
                    if let Some(b) = block {
                        self.fn_builder.set_terminator(
                            b,
                            Terminator::Jump {
                                target: merge_block,
                            },
                        )
                    }

                    match_arms.push(SwitchArm {
                        target: variant_id,
                        jump: arm_block,
                    });
                }
                PatternNode::Default(pat) => {
                    let body = *body;
                    if let Some(p) = pat.payload {
                        let name = self.get_ident_name(p);
                        let ts = lower_type_spec(target_ts);
                        let payload_local = self.fn_builder.get_local(p, name, ts);
                        self.fn_builder.emit_store(
                            arm_block,
                            Place::local(payload_local),
                            target_id,
                        );
                    }

                    let block = self.block_statement(body, arm_block);
                    if let Some(b) = block {
                        self.fn_builder.set_terminator(
                            b,
                            Terminator::Jump {
                                target: merge_block,
                            },
                        );
                    }

                    default_arm = Some(arm_block);
                }
                _ => panic!("can not convert pattern into a switch terminator"),
            }

            if default_arm.is_some() {
                // once we have the deafult arm then no other pattern will ever match
                break;
            }
        }

        let default = match default_arm {
            Some(d) => d,
            None => {
                eprintln!(
                    "TODO: currently we don't always insist that pattern matching is exaustive"
                );
                merge_block
            }
        };

        self.fn_builder.set_terminator(
            block_id,
            Terminator::SwitchVariant {
                discriminant,
                default,
                arms: match_arms,
            },
        );

        merge_block
    }

    fn block_type_match(
        &mut self,
        block_id: BlockId,
        target: NodeID,
        arms: Vec<NodeID>,
    ) -> BlockId {
        let merge_block = self.fn_builder.add_block();
        let target_id = self.block_expression(block_id, target);

        let target_ts = self
            .node_tree
            .get_type(target)
            .expect("missing type for match target");

        let mut success_arm = None;
        let mut default_arm = None;
        for arm in arms {
            let arm_block;
            let arm_node = self.node_tree.get_node(arm).expect("missing arm node");

            let (pattern_id, body) = match arm_node {
                Node::MatchArm { pattern, body } => (pattern, body),
                _ => panic!("expected a match arm"),
            };

            let pattern = self
                .node_tree
                .get_node(*pattern_id)
                .expect("missing pattern for match arm");
            let pattern = match pattern {
                Node::Pattern(p) => p,
                _ => panic!("pattern was an invalid node"),
            };

            match pattern {
                PatternNode::TypeSpec(ts) => {
                    if success_arm.is_some() {
                        // The first type spec will always match before other type specs so the only
                        // successful match that's possible after the first type spec is a default
                        continue;
                    }
                    arm_block = self.fn_builder.add_block();

                    let body = *body;
                    let pattern_id = *pattern_id;
                    if let Some(p) = ts.payload {
                        let name = self.get_ident_name(p);

                        let ts = self
                            .node_tree
                            .get_type(pattern_id)
                            .expect("missing type for type spec pattern");
                        let ts = lower_type_spec(ts);

                        // the target type will be UnsafePtr but we need this payload to change the
                        // type into whatever the target match is
                        let payload_local = self.fn_builder.get_local(p, name, ts);
                        self.fn_builder.emit_store(
                            arm_block,
                            Place::local(payload_local),
                            target_id,
                        );
                    }

                    let block = self.block_statement(body, arm_block);
                    if let Some(b) = block {
                        self.fn_builder.set_terminator(
                            b,
                            Terminator::Jump {
                                target: merge_block,
                            },
                        );
                    }

                    success_arm = Some(arm_block);
                }
                PatternNode::Default(pat) => {
                    arm_block = self.fn_builder.add_block();
                    let body = *body;
                    if let Some(p) = pat.payload {
                        let name = self.get_ident_name(p);
                        let ts = lower_type_spec(target_ts);
                        let payload_local = self.fn_builder.get_local(p, name, ts);
                        self.fn_builder.emit_store(
                            arm_block,
                            Place::local(payload_local),
                            target_id,
                        );
                    }

                    let block = self.block_statement(body, arm_block);
                    if let Some(b) = block {
                        self.fn_builder.set_terminator(
                            b,
                            Terminator::Jump {
                                target: merge_block,
                            },
                        );
                    }

                    default_arm = Some(arm_block);
                }
                _ => panic!("can not convert pattern into a switch terminator"),
            }

            if default_arm.is_some() {
                // once we have the deafult arm then no other pattern will ever match
                break;
            }
        }

        // TODO: this could potentially fail where match expressions contain only a single default arm.
        // We probably don't want that to be an error (even though it's weird) because it's a valid
        // state that might exist durring development. We need to update this logic to handle that
        let true_target = match success_arm {
            Some(d) => d,
            None => panic!("missing target for unsafe pointer match"),
        };

        // TODO: we enfoce that both exist here because using the `let` statement will ensure this is
        // set up correctly for now. We need this to be more robust in the future or we need to
        // establish some semantic rules that prevents users from constructing weird match structures
        let false_target = match default_arm {
            Some(d) => d,
            None => panic!("missing false arm for unsafe pointer match"),
        };

        self.fn_builder.set_terminator(
            block_id,
            Terminator::Branch {
                // target is an unsafe pointer that will get truncated to an i1
                // durring codegen. For now, any non-nil pointer is considered
                // truthy and any nil pointer is considered falsey.
                cond: target_id,
                true_target,
                false_target,
            },
        );

        merge_block
    }

    fn block_expression(&mut self, block_id: BlockId, node_id: NodeID) -> ValueId {
        if self.fn_builder.is_block_closed(block_id) {
            // if the block is closed just skip all the remaining instructions as they are no longer
            // reachable, trying to adding them would cause a panic
            panic!("can not block expressions on a closed block");
        }

        let node = match self.node_tree.get_node(node_id) {
            Some(n) => n,
            None => panic!("type checking unknown node"),
        };
        let node = node.clone();

        let ts = match self.node_tree.get_type(node_id) {
            Some(ts) => lower_type_spec(ts),
            None => panic!("missing type for expression node"),
        };

        match node {
            Node::IntLiteral(i) => {
                self.fn_builder
                    .emit_const(block_id, ts, ConstValue::ConstInt(i))
            }
            Node::FloatLiteral(f) => {
                self.fn_builder
                    .emit_const(block_id, ts, ConstValue::ConstFloat(f))
            }
            Node::BoolLiteral(b) => {
                self.fn_builder
                    .emit_const(block_id, ts, ConstValue::ConstBool(b))
            }
            Node::StringLiteral(s) => {
                self.fn_builder
                    .emit_const(block_id, ts, ConstValue::ConstString(s))
            }
            Node::Identifier { .. } => {
                let place = self.block_lvalue(block_id, node_id);
                self.fn_builder.emit_load(block_id, place, ts)
            }
            Node::Binary {
                left,
                operator,
                right,
            } => {
                let lhs = self.block_expression(block_id, left);
                let rhs = self.block_expression(block_id, right);

                match operator {
                    BinaryOp::Add => self.fn_builder.emit_add(block_id, lhs, rhs),
                    BinaryOp::Subtract => self.fn_builder.emit_sub(block_id, lhs, rhs),
                    BinaryOp::Multiply => self.fn_builder.emit_mul(block_id, lhs, rhs),
                    BinaryOp::Divide => self.fn_builder.emit_div(block_id, lhs, rhs),
                    BinaryOp::Modulo => self.fn_builder.emit_mod(block_id, lhs, rhs),
                    BinaryOp::Equal => self.fn_builder.emit_equal(block_id, lhs, rhs),
                    BinaryOp::NotEqual => self.fn_builder.emit_not_equal(block_id, lhs, rhs),
                    BinaryOp::LessThan => self.fn_builder.emit_less_than(block_id, lhs, rhs),
                    BinaryOp::LessThanOrEqual => {
                        self.fn_builder.emit_less_or_equal(block_id, lhs, rhs)
                    }
                    BinaryOp::GreaterThan => self.fn_builder.emit_greater_than(block_id, lhs, rhs),
                    BinaryOp::GreaterThanOrEqual => {
                        self.fn_builder.emit_greater_or_equal(block_id, lhs, rhs)
                    }
                    BinaryOp::LogicalAnd => self.fn_builder.emit_logical_and(block_id, lhs, rhs),
                    BinaryOp::LogicalOr => self.fn_builder.emit_logical_or(block_id, lhs, rhs),
                    BinaryOp::BitwiseAnd => self.fn_builder.emit_bitwise_and(block_id, lhs, rhs),
                    BinaryOp::BitwiseOr => self.fn_builder.emit_bitwise_or(block_id, lhs, rhs),
                    BinaryOp::BitwiseXor => self.fn_builder.emit_bitwise_xor(block_id, lhs, rhs),
                }
            }
            Node::Unary { operator, operand } => match operator {
                UnaryOp::Not => {
                    let value = self.block_expression(block_id, operand);
                    self.fn_builder.emit_bool_not(block_id, value)
                }
                UnaryOp::Negate => {
                    let value = self.block_expression(block_id, operand);
                    self.fn_builder.emit_negate(block_id, value)
                }
                UnaryOp::Positive => self.block_expression(block_id, operand),
                UnaryOp::Dereference => {
                    let mut place = self.block_lvalue(block_id, operand);
                    place.projections.push(Projection::Deref);
                    self.fn_builder.emit_load(block_id, place, ts)
                }
                UnaryOp::AddressOf => {
                    let place = self.block_lvalue(block_id, operand);
                    self.fn_builder.emit_address_of(block_id, place, ts)
                }
            },
            Node::Call { func, args } => {
                // TODO:
                ValueId::nil()
            }
            Node::EnumConstructor {
                target,
                variant,
                payload,
            } => {
                // TODO:
                ValueId::nil()
            }
            Node::Index { .. } => {
                // TODO: slice indexing should be desugared to a core lib call before this point
                let place = self.block_lvalue(block_id, node_id);
                self.fn_builder.emit_load(block_id, place, ts)
            }
            Node::Range { .. } => todo!("range expressions are not yet supported"),
            Node::FieldAccess { .. } => {
                let place = self.block_lvalue(block_id, node_id);
                self.fn_builder.emit_load(block_id, place, ts)
            }
            Node::MetaType => {
                // TODO:
                ValueId::nil()
            }
            Node::Alloc { .. } => {
                // TODO:
                ValueId::nil()
            }
            Node::Free { .. } => {
                // TODO:
                ValueId::nil()
            }
            _ => panic!("not a valid expression node"),
        }
    }
}

fn lower_type_spec(hir_ts: &hir::TypeSpec) -> TypeSpec {
    match hir_ts {
        hir::TypeSpec::Int8 => TypeSpec::I8,
        hir::TypeSpec::Int16 => TypeSpec::I16,
        hir::TypeSpec::Int32 => TypeSpec::I32,
        hir::TypeSpec::Int64 => TypeSpec::I64,
        hir::TypeSpec::UInt8 => TypeSpec::U8,
        hir::TypeSpec::UInt16 => TypeSpec::U16,
        hir::TypeSpec::UInt32 => TypeSpec::U32,
        hir::TypeSpec::UInt64 => TypeSpec::U64,
        hir::TypeSpec::Float32 => TypeSpec::F32,
        hir::TypeSpec::Float64 => TypeSpec::F64,
        hir::TypeSpec::Bool => TypeSpec::Bool,
        // panic types become unit types because the CFG lets us explicitly represent the control
        // flow of a panic.
        hir::TypeSpec::Unit | hir::TypeSpec::Panic => TypeSpec::Unit,
        hir::TypeSpec::String => TypeSpec::String,
        hir::TypeSpec::Pointer(inner) => TypeSpec::Ptr(Box::new(lower_type_spec(inner))),
        hir::TypeSpec::UnsafePtr => TypeSpec::OpaquePtr,
        hir::TypeSpec::Slice(inner) => TypeSpec::Slice(Box::new(lower_type_spec(inner))),
        hir::TypeSpec::Array(at) => TypeSpec::Array {
            elem: Box::new(lower_type_spec(&at.type_spec)),
            len: at.size,
        },
        hir::TypeSpec::Struct(st) => TypeSpec::Struct(
            st.fields
                .iter()
                .map(|f| lower_type_spec(&f.type_spec))
                .collect(),
        ),
        hir::TypeSpec::Enum(et) => TypeSpec::Enum {
            tag_size: tag_size_for(et.variants.len()),
            variants: et
                .variants
                .iter()
                .map(|v| v.payload.as_ref().map(lower_type_spec))
                .collect(),
        },
        hir::TypeSpec::Named(nt) => lower_type_spec(&nt.type_spec),
        // For function types we lower to the return type, since MirFunction tracks params
        // separately and mir::TypeSpec has no Function variant.
        hir::TypeSpec::Function(ft) => lower_type_spec(&ft.return_type),
        hir::TypeSpec::Any
        | hir::TypeSpec::IntLiteral(_)
        | hir::TypeSpec::FloatLiteral(_)
        | hir::TypeSpec::InferredEnumExpr(_)
        | hir::TypeSpec::InferredEnumPat(_) => {
            panic!("unresolved type {:?} reached MIR lowering", hir_ts)
        }
    }
}

fn tag_size_for(variant_count: usize) -> TagSize {
    if variant_count <= u8::MAX as usize + 1 {
        TagSize::U8
    } else if variant_count <= u16::MAX as usize + 1 {
        TagSize::U16
    } else if variant_count <= u32::MAX as usize + 1 {
        TagSize::U32
    } else {
        TagSize::U64
    }
}

fn get_variant_id(type_spec: &hir::TypeSpec, variant_name: StrID) -> ConstValue {
    match typer::resolve_type(type_spec) {
        hir::TypeSpec::Enum(e) => {
            for (i, v) in e.variants.iter().enumerate() {
                if variant_name == v.name {
                    return ConstValue::ConstUInt(i as u64);
                }
            }
            panic!("missing variant!")
        }
        _ => panic!("invalid type spec, not an enum"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::collections::BTreeMap;
    use std::fs;
    use std::path::Path;

    use crate::mir::{BasicBlock, Instruction, MirModule};
    use crate::noder::SideTable;
    use crate::noder::node_module;
    use crate::parser::Parser;
    use crate::str_store::{StrID, StrStore};

    fn assert_file_path_eq(path: &std::path::Path, blocker_dir: &Path) {
        let ext = path.extension().expect("Failed to get file extension");
        if ext != "manta" {
            return;
        }

        let file_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => panic!("Failed to read {}", path.display()),
        };

        let mut str_store = StrStore::new();
        let parser = Parser::new(source);
        let module = parser.parse_module(&mut str_store);

        let node_tree = node_module(module);
        let blocker = Blocker::new(&node_tree);
        let mir_module = blocker.build_module();

        let json_output = serde_json::to_string_pretty(&mir_module)
            .expect("Failed to serialize MirModule to JSON");

        let blocker_file = blocker_dir.join(format!("{}.json", file_name));

        if blocker_file.exists() {
            let expected_json = match fs::read_to_string(&blocker_file) {
                Ok(s) => s,
                Err(_) => panic!("Failed to read {}", blocker_file.display()),
            };

            assert_eq!(
                json_output, expected_json,
                "Blocker output mismatch for {}",
                file_name
            );
        } else {
            fs::create_dir_all(blocker_dir).expect("Failed to create blocker test directory");

            match fs::write(&blocker_file, &json_output) {
                Ok(_) => (),
                Err(_) => panic!("Failed to write blocker output to {:?}", blocker_file),
            };

            panic!(
                "Generated new blocker output file: {:?}. Please verify its correctness.",
                blocker_file
            );
        }
    }

    include!(concat!(env!("OUT_DIR"), "/generated_blocker_tests.rs"));

    macro_rules! test_blocker_function {
        ( $( $case:ident { got: $got:expr, want: $want:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let node_tree = $got;
                    let blocker = Blocker::new(&node_tree);
                    let mir_module = blocker.build_module();
                    assert_eq!(mir_module, $want)

                }
            )*
        };
    }

    test_blocker_function!(
        test_blocker_const_int {
            got: NodeTree {
                nodes: vec![
                    Node::Identifier {
                        name: StrID::from_usize(1),
                        module: None
                    }, // NodeID(0)
                    Node::IntLiteral(42), // NodeID(1)
                    Node::Return {
                        value: Some(NodeID::from_usize(1))
                    }, // NodeID(2)
                    Node::Block {
                        statements: vec![NodeID::from_usize(2)]
                    }, // NodeID(3)
                    Node::FunctionDecl {
                        // NodeID(4)
                        ident: NodeID::from_usize(0),
                        params: vec![],
                        body: NodeID::from_usize(3),
                    },
                ],
                roots: vec![NodeID::from_usize(4)],
                type_map: SideTable {
                    keys: BTreeMap::from([(NodeID::from_usize(1), 0), (NodeID::from_usize(4), 1),]),
                    values: vec![hir::TypeSpec::Int32, hir::TypeSpec::Int32],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![],
                },
            },
            want: MirModule {
                globals: vec![],
                init: MirFunction {
                    blocks: vec![BasicBlock {
                        block_args: vec![],
                        instructions: vec![],
                        terminator: Terminator::Return { value: None },
                    }],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![],
                    name: str_store::INIT,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    params: vec![],
                    type_spec: TypeSpec::Unit,
                    value_types: vec![],
                },
                functions: vec![MirFunction {
                    name: StrID::from_usize(1),
                    params: vec![],
                    type_spec: TypeSpec::I32,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![BasicBlock {
                        block_args: vec![],
                        instructions: vec![ValueId::from_usize(1)],
                        terminator: Terminator::Return {
                            value: Some(ValueId::from_usize(1)),
                        },
                    },],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![Instruction::Const {
                        value: ConstValue::ConstInt(42),
                    },],
                    value_types: vec![TypeSpec::I32],
                }],
            },
        },
        test_blocker_const_bool_true {
            got: NodeTree {
                nodes: vec![
                    Node::Identifier {
                        name: StrID::from_usize(1),
                        module: None
                    }, // NodeID(0)
                    Node::BoolLiteral(true), // NodeID(1)
                    Node::Return {
                        value: Some(NodeID::from_usize(1))
                    }, // NodeID(2)
                    Node::Block {
                        statements: vec![NodeID::from_usize(2)]
                    }, // NodeID(3)
                    Node::FunctionDecl {
                        // NodeID(4)
                        ident: NodeID::from_usize(0),
                        params: vec![],
                        body: NodeID::from_usize(3),
                    },
                ],
                roots: vec![NodeID::from_usize(4)],
                type_map: SideTable {
                    keys: BTreeMap::from([(NodeID::from_usize(1), 0), (NodeID::from_usize(4), 1)]),
                    values: vec![hir::TypeSpec::Bool, hir::TypeSpec::Bool],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![]
                },
            },
            want: MirModule {
                globals: vec![],
                init: MirFunction {
                    name: str_store::INIT,
                    params: vec![],
                    type_spec: TypeSpec::Unit,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![BasicBlock {
                        block_args: vec![],
                        instructions: vec![],
                        terminator: Terminator::Return { value: None },
                    }],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![],
                    value_types: vec![],
                },
                functions: vec![MirFunction {
                    name: StrID::from_usize(1),
                    params: vec![],
                    type_spec: TypeSpec::Bool,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![BasicBlock {
                        block_args: vec![],
                        instructions: vec![ValueId::from_usize(1)],
                        terminator: Terminator::Return {
                            value: Some(ValueId::from_usize(1))
                        },
                    }],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![Instruction::Const {
                        value: ConstValue::ConstBool(true)
                    }],
                    value_types: vec![TypeSpec::Bool],
                }],
            },
        },
        test_blocker_const_bool_false {
            got: NodeTree {
                nodes: vec![
                    Node::Identifier {
                        name: StrID::from_usize(1),
                        module: None
                    }, // NodeID(0)
                    Node::BoolLiteral(false), // NodeID(1)
                    Node::Return {
                        value: Some(NodeID::from_usize(1))
                    }, // NodeID(2)
                    Node::Block {
                        statements: vec![NodeID::from_usize(2)]
                    }, // NodeID(3)
                    Node::FunctionDecl {
                        // NodeID(4)
                        ident: NodeID::from_usize(0),
                        params: vec![],
                        body: NodeID::from_usize(3),
                    },
                ],
                roots: vec![NodeID::from_usize(4)],
                type_map: SideTable {
                    keys: BTreeMap::from([(NodeID::from_usize(1), 0), (NodeID::from_usize(4), 1)]),
                    values: vec![hir::TypeSpec::Bool, hir::TypeSpec::Bool],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![]
                },
            },
            want: MirModule {
                globals: vec![],
                init: MirFunction {
                    name: str_store::INIT,
                    params: vec![],
                    type_spec: TypeSpec::Unit,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![BasicBlock {
                        block_args: vec![],
                        instructions: vec![],
                        terminator: Terminator::Return { value: None },
                    }],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![],
                    value_types: vec![],
                },
                functions: vec![MirFunction {
                    name: StrID::from_usize(1),
                    params: vec![],
                    type_spec: TypeSpec::Bool,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![BasicBlock {
                        block_args: vec![],
                        instructions: vec![ValueId::from_usize(1)],
                        terminator: Terminator::Return {
                            value: Some(ValueId::from_usize(1))
                        },
                    }],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![Instruction::Const {
                        value: ConstValue::ConstBool(false)
                    }],
                    value_types: vec![TypeSpec::Bool],
                }],
            },
        },
        test_blocker_const_float {
            got: NodeTree {
                nodes: vec![
                    Node::Identifier {
                        name: StrID::from_usize(1),
                        module: None
                    }, // NodeID(0)
                    Node::FloatLiteral(3.45), // NodeID(1)
                    Node::Return {
                        value: Some(NodeID::from_usize(1))
                    }, // NodeID(2)
                    Node::Block {
                        statements: vec![NodeID::from_usize(2)]
                    }, // NodeID(3)
                    Node::FunctionDecl {
                        // NodeID(4)
                        ident: NodeID::from_usize(0),
                        params: vec![],
                        body: NodeID::from_usize(3),
                    },
                ],
                roots: vec![NodeID::from_usize(4)],
                type_map: SideTable {
                    keys: BTreeMap::from([(NodeID::from_usize(1), 0), (NodeID::from_usize(4), 1)]),
                    values: vec![hir::TypeSpec::Float64, hir::TypeSpec::Float64],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![]
                },
            },
            want: MirModule {
                globals: vec![],
                init: MirFunction {
                    name: str_store::INIT,
                    params: vec![],
                    type_spec: TypeSpec::Unit,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![BasicBlock {
                        block_args: vec![],
                        instructions: vec![],
                        terminator: Terminator::Return { value: None },
                    }],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![],
                    value_types: vec![],
                },
                functions: vec![MirFunction {
                    name: StrID::from_usize(1),
                    params: vec![],
                    type_spec: TypeSpec::F64,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![BasicBlock {
                        block_args: vec![],
                        instructions: vec![ValueId::from_usize(1)],
                        terminator: Terminator::Return {
                            value: Some(ValueId::from_usize(1))
                        },
                    }],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![Instruction::Const {
                        value: ConstValue::ConstFloat(3.45)
                    }],
                    value_types: vec![TypeSpec::F64],
                }],
            },
        },
        test_blocker_const_string {
            got: NodeTree {
                nodes: vec![
                    Node::Identifier {
                        name: StrID::from_usize(1),
                        module: None
                    }, // NodeID(0)
                    Node::StringLiteral(StrID::from_usize(99)), // NodeID(1)
                    Node::Return {
                        value: Some(NodeID::from_usize(1))
                    }, // NodeID(2)
                    Node::Block {
                        statements: vec![NodeID::from_usize(2)]
                    }, // NodeID(3)
                    Node::FunctionDecl {
                        // NodeID(4)
                        ident: NodeID::from_usize(0),
                        params: vec![],
                        body: NodeID::from_usize(3),
                    },
                ],
                roots: vec![NodeID::from_usize(4)],
                type_map: SideTable {
                    keys: BTreeMap::from([(NodeID::from_usize(1), 0), (NodeID::from_usize(4), 1)]),
                    values: vec![hir::TypeSpec::String, hir::TypeSpec::String],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![]
                },
            },
            want: MirModule {
                globals: vec![],
                init: MirFunction {
                    name: str_store::INIT,
                    params: vec![],
                    type_spec: TypeSpec::Unit,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![BasicBlock {
                        block_args: vec![],
                        instructions: vec![],
                        terminator: Terminator::Return { value: None },
                    }],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![],
                    value_types: vec![],
                },
                functions: vec![MirFunction {
                    name: StrID::from_usize(1),
                    params: vec![],
                    type_spec: TypeSpec::String,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![BasicBlock {
                        block_args: vec![],
                        instructions: vec![ValueId::from_usize(1)],
                        terminator: Terminator::Return {
                            value: Some(ValueId::from_usize(1))
                        },
                    }],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![Instruction::Const {
                        value: ConstValue::ConstString(StrID::from_usize(99))
                    }],
                    value_types: vec![TypeSpec::String],
                }],
            },
        },
    );
}

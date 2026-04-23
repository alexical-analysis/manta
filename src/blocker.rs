mod builder;

use std::collections::BTreeMap;

use builder::{Cfg, FunctionBuilder};

use crate::ast::{BinaryOp, UnaryOp};
use crate::hir::{self, Node, NodeID, PatternNode};
use crate::mir::{
    BlockId, ConstValue, Global, GlobalId, MirFunction, MirModule, Place, PlaceBase, Projection,
    SwitchArm, TagSize, Terminator, TypeSpec, ValueId,
};
use crate::noder::typer::resolve_type;
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
                self.block_statement(block_id, body);

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
                let type_spec = self
                    .node_tree
                    .get_type(target)
                    .expect("missing type for field access target");

                let base_type = resolve_type(type_spec);
                let fields = match base_type {
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

    fn block_statement(&mut self, block_id: BlockId, node_id: NodeID) -> Option<BlockId> {
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
                // TODO: this string should contain the error that cause the invalid node to appear
                // in the first place
                let value = self.fn_builder.emit_const(
                    block_id,
                    TypeSpec::String,
                    ConstValue::String(str_store::UNDERSCORE),
                );
                self.fn_builder
                    .set_terminator(block_id, Terminator::Panic { msg: value });

                None
            }
            Node::Block { statements } => {
                self.fn_builder.open_scope(block_id);

                let mut current_block = block_id;
                for stmt in statements {
                    match self.block_statement(current_block, stmt) {
                        Some(b) => current_block = b,
                        None => break,
                    }
                }

                let merge_block = self.fn_builder.close_scope();

                let cfg = Cfg::new(&mut self.fn_builder, block_id);
                if cfg.all_blocks_terminate(&self.fn_builder) {
                    None
                } else {
                    Some(merge_block)
                }
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
                let defer_block = self.fn_builder.add_defer_block();
                self.block_statement(defer_block, block);

                // need to return the original block here because defer blocks are only added to
                // the CFG graph when we build the final MIR function
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

                let block = self.block_statement(true_block_id, then_block);
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
                    let block = self.block_statement(false_block_id, e);
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
            Node::MatchArm { .. } => panic!("out of place match arm"),
            Node::Call { func, args } => {
                let name = self.get_ident_name(func);

                match name {
                    str_store::PANIC => {
                        if args.len() != 1 {
                            panic!("panic requires exactly 1 argument")
                        }
                        let arg = args.first().expect("missing argument for panic");
                        let value = self.block_expression(block_id, *arg);

                        self.fn_builder
                            .set_terminator(block_id, Terminator::Panic { msg: value });
                        None
                    }
                    _ => {
                        // Other calls are technically expressions but because they can result
                        // in side effects they need to be handled as statements as well
                        self.block_expression(block_id, node_id);
                        Some(block_id)
                    }
                }
            }
            Node::Free { .. } => {
                // Same as call
                self.block_expression(block_id, node_id);
                Some(block_id)
            }
            Node::Loop { body } => {
                // create a new block for the loop so we have something we can jump back to
                let loop_start = self.fn_builder.add_block();
                self.fn_builder
                    .set_terminator(block_id, Terminator::Jump { target: loop_start });

                let loop_end = self.block_statement(loop_start, body);
                if let Some(loop_end) = loop_end {
                    self.fn_builder
                        .set_terminator(loop_end, Terminator::Jump { target: loop_start });
                }

                let loop_cfg = Cfg::new(&mut self.fn_builder, loop_start);
                loop_cfg.continue_to(&mut self.fn_builder, loop_start);

                if loop_cfg.can_break(&self.fn_builder) {
                    let break_block = self.fn_builder.add_block();
                    loop_cfg.break_to(&mut self.fn_builder, break_block);

                    // controll flow continues after breaking out of the block
                    Some(break_block)
                } else {
                    // unless there is some code that can break out of the loop there is no way to
                    // escape the loop
                    None
                }
            }
            Node::Break => {
                self.fn_builder.set_terminator(block_id, Terminator::Break);

                // after breaking out of a loop no more instructions should be added to
                // the current block
                None
            }
            Node::Continue => {
                self.fn_builder
                    .set_terminator(block_id, Terminator::Continue);

                // after breaking out of a loop no more instructions should be added to
                // the current block
                None
            }
            Node::FunctionDecl { .. } => panic!("function decls are not valid statements"),
            Node::TypeDecl { .. } => panic!("type decls are not valid statements"),
            Node::IntLiteral(_) => panic!("int literals are expressions, not statements"),
            Node::UIntLiteral(_) => panic!("int literals are expressions, not statements"),
            Node::FloatLiteral(_) => panic!("float literals are expressions, not statements"),
            Node::StringLiteral(_) => panic!("string literals are expressions, not statements"),
            Node::BoolLiteral(_) => panic!("bool literals are expressions, not statements"),
            Node::Identifier { .. } => panic!("identifiers are expressions, not statements"),
            Node::Binary { .. } => panic!("binary expressions are not statements"),
            Node::Unary { .. } => panic!("unary expressions are not statements"),
            Node::EnumConstructor { .. } => {
                panic!("enum constructors are expressions not statements")
            }
            Node::StructConstructor { .. } => {
                panic!("struct constructors are expressions not statements")
            }
            Node::StructConstructorField { .. } => {
                panic!("struct fields must be in struct constructor expressions")
            }
            Node::Index { .. } => panic!("index expressions are not statements"),
            Node::Range { .. } => panic!("range expressions are not statements"),
            Node::FieldAccess { .. } => panic!("field access expressions are not statements"),
            Node::MetaType => panic!("meta type expressions are not statements"),
            Node::Alloc { .. } => panic!("alloc expressions are not statements"),
            Node::Pattern(_) => panic!("patterns must appear within match expressions"),
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
                    let block = self.block_statement(arm_block, body);
                    if let Some(b) = block {
                        self.fn_builder.set_terminator(
                            b,
                            Terminator::Jump {
                                target: merge_block,
                            },
                        );
                    }

                    match_arms.push(SwitchArm {
                        target: ConstValue::Int(i as u64),
                        jump: arm_block,
                    });
                }
                PatternNode::UIntLiteral(i) => {
                    // Int literal patterns can never have a payload so this is pretty easy
                    let body = *body;
                    let i = *i;
                    let block = self.block_statement(arm_block, body);
                    if let Some(b) = block {
                        self.fn_builder.set_terminator(
                            b,
                            Terminator::Jump {
                                target: merge_block,
                            },
                        );
                    }

                    match_arms.push(SwitchArm {
                        target: ConstValue::Int(i),
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

                    let block = self.block_statement(arm_block, body);
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
                    "TODO: currently we assume that if there's is no default_arm, pattern matching is exaustive"
                );
                let unreachable_block = self.fn_builder.add_block();
                self.fn_builder
                    .set_terminator(unreachable_block, Terminator::Unreachable);
                unreachable_block
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

        // Get the target as a place as well since VariantGetPayload requires a place value instead
        // of just a simple value_id. We check if the target is already a global or local that we
        // can convert directly into a place or if we need a temporary local to hold the value
        let local_target = self.fn_builder.find_local(target);
        let global_target = self.global_map.get(&target);

        let target_place = match (local_target, global_target) {
            (Some(local_id), None) => Place::local(local_id),
            (None, Some(global_id)) => Place::global(*global_id),
            (Some(_), Some(_)) => unreachable!("value can not be both a local and a global"),
            _ => {
                let temp_local = self
                    .fn_builder
                    .add_local(str_store::MATCH_TARGET, ts.clone());
                let tmp_place = Place::local(temp_local);
                self.fn_builder
                    .emit_store(block_id, tmp_place.clone(), target_id);
                tmp_place
            }
        };

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
                    let variant_id = get_variant_tag(target_ts, pat.variant);

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
                                target_place.clone(),
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

                    let block = self.block_statement(arm_block, body);
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

                    let block = self.block_statement(arm_block, body);
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
                    "TODO: currently we assume that if there's is no default_arm, pattern matching is exaustive"
                );
                let unreachable_block = self.fn_builder.add_block();
                self.fn_builder
                    .set_terminator(unreachable_block, Terminator::Unreachable);
                unreachable_block
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

                    let block = self.block_statement(arm_block, body);
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

                    let block = self.block_statement(arm_block, body);
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
                // target is an unsafe pointer. Codegen emits `icmp ne ptr, null`
                // to produce an i1 condition: non-null = true, null = false.
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
                // This is necessary because int literals can technically be coerced into floating point types
                // in certian situations.
                let const_value = match ts {
                    TypeSpec::F32 | TypeSpec::F64 => ConstValue::Float(i as f64),
                    _ => ConstValue::Int(i as u64),
                };
                self.fn_builder.emit_const(block_id, ts, const_value)
            }
            Node::UIntLiteral(i) => {
                // This is necessary because int literals can technically be coerced into floating point types
                // in certian situations.
                let const_value = match ts {
                    TypeSpec::F32 | TypeSpec::F64 => ConstValue::Float(i as f64),
                    _ => ConstValue::Int(i),
                };
                self.fn_builder.emit_const(block_id, ts, const_value)
            }
            Node::FloatLiteral(f) => self
                .fn_builder
                .emit_const(block_id, ts, ConstValue::Float(f)),
            Node::BoolLiteral(b) => self
                .fn_builder
                .emit_const(block_id, ts, ConstValue::Bool(b)),
            Node::StringLiteral(s) => {
                self.fn_builder
                    .emit_const(block_id, ts, ConstValue::String(s))
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
                let name = self.get_ident_name(func);
                let arg_values: Vec<ValueId> = args
                    .iter()
                    .map(|a| self.block_expression(block_id, *a))
                    .collect();

                self.fn_builder.emit_call(block_id, name, arg_values, ts)
            }
            Node::EnumConstructor {
                variant, payload, ..
            } => {
                let ts = self
                    .node_tree
                    .get_type(node_id)
                    .expect("missing type for enum");

                let tag_val = get_variant_tag(ts, variant);
                let payload_val = payload.map(|n| self.block_expression(block_id, n));

                let ts = lower_type_spec(ts);
                self.fn_builder
                    .emit_make_variant(block_id, tag_val, payload_val, ts)
            }
            Node::StructConstructor { fields } => {
                let ts = self
                    .node_tree
                    .get_type(node_id)
                    .expect("missing type for struct");

                let mut field_values = vec![];
                for field in &fields {
                    let field_value = self.block_expression(block_id, *field);
                    field_values.push(field_value)
                }

                let ts = lower_type_spec(ts);
                self.fn_builder.emit_make_struct(block_id, field_values, ts)
            }
            Node::StructConstructorField { value, .. } => self.block_expression(block_id, value),
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
                // `ts` is the lowered type of the value being allocated (e.g. I32 for @i32).
                // Produce a const struct { sizeof: u64, alignof: u64, flags: u64 }.
                let layout = type_layout(&ts, Arch::W64);
                let meta_value = ConstValue::Struct(vec![
                    ConstValue::Int(layout.size),
                    ConstValue::Int(layout.align),
                    ConstValue::Int(0), // flags reserved for future use
                ]);
                let meta_type = TypeSpec::Struct(vec![TypeSpec::U64, TypeSpec::U64, TypeSpec::U64]);
                self.fn_builder.emit_const(block_id, meta_type, meta_value)
            }
            Node::Alloc { meta_type, .. } => {
                let meta_val = self.block_expression(block_id, meta_type);
                self.fn_builder.emit_alloc(block_id, meta_val)
            }
            Node::Free { expr } => {
                let ptr = self.block_expression(block_id, expr);
                self.fn_builder.emit_free(block_id, ptr)
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
                .map(|v| match v.payload.as_ref() {
                    Some(ts) => lower_type_spec(ts),
                    None => TypeSpec::Unit,
                })
                .collect(),
        },
        hir::TypeSpec::Named(nt) => lower_type_spec(&nt.type_spec),
        // For function types we lower to the return type, since MirFunction tracks params
        // separately and mir::TypeSpec has no Function variant.
        hir::TypeSpec::Function(ft) => lower_type_spec(&ft.return_type),
        hir::TypeSpec::Any
        | hir::TypeSpec::IntLiteral(_)
        | hir::TypeSpec::UIntLiteral(_)
        | hir::TypeSpec::FloatLiteral(_)
        | hir::TypeSpec::InferredEnumExpr(_)
        | hir::TypeSpec::InferredEnumPat(_) => {
            panic!("unresolved type {:?} reached MIR lowering", hir_ts)
        }
    }
}

/// The size and alignment of a type in bytes, used for computing MetaType struct values
/// and struct field offsets during MIR lowering.
#[derive(Clone, Copy)]
pub struct Layout {
    /// The size of the type in bytes.
    size: u64,
    /// The required alignment of the type in bytes. Must be a power of 2, or 0 for zero-sized types.
    align: u64,
}

impl Layout {
    pub fn size(&self) -> u64 {
        self.size
    }
}

/// Arch is used to specify 32-bit vs 64-bit targets
#[derive(Clone, Copy)]
pub enum Arch {
    W32,
    W64,
}

impl Arch {
    fn ptr_size(self) -> u64 {
        match self {
            Arch::W32 => 4,
            Arch::W64 => 8,
        }
    }
}

/// Returns the layout for a MIR TypeSpec on the given target architecture.
pub fn type_layout(ts: &TypeSpec, arch: Arch) -> Layout {
    // TODO: need to support sizes of less than 1 byte for packed structs
    let ptr = arch.ptr_size();
    match ts {
        TypeSpec::Bool | TypeSpec::I8 | TypeSpec::U8 => Layout { size: 1, align: 1 },
        TypeSpec::I16 | TypeSpec::U16 => Layout { size: 2, align: 2 },
        TypeSpec::I32 | TypeSpec::U32 | TypeSpec::F32 => Layout { size: 4, align: 4 },
        TypeSpec::I64 | TypeSpec::U64 | TypeSpec::F64 => Layout { size: 8, align: 8 },
        TypeSpec::Ptr(_) | TypeSpec::OpaquePtr => Layout {
            size: ptr,
            align: ptr,
        },
        // String is a fat pointer: { ptr: *u8, len: usize }
        TypeSpec::String => Layout {
            size: ptr * 2,
            align: ptr,
        },
        // Slice is a fat pointer: { ptr: *T, len: usize, cap: usize }
        TypeSpec::Slice(_) => Layout {
            size: ptr * 3,
            align: ptr,
        },
        TypeSpec::Unit => Layout { size: 0, align: 0 },
        TypeSpec::Array { elem, len } => {
            let elem_layout = type_layout(elem, arch);
            let stride = align_up(elem_layout);
            Layout {
                size: stride * (*len as u64),
                align: elem_layout.align,
            }
        }
        TypeSpec::Struct(fields) => struct_layout(fields, arch),
        TypeSpec::Enum { tag_size, variants } => {
            let tag_bytes = match tag_size {
                TagSize::U8 => 1u64,
                TagSize::U16 => 2,
                TagSize::U32 => 4,
                TagSize::U64 => 8,
            };
            let payload = variants.iter().map(|v| type_layout(v, arch)).fold(
                Layout { size: 0, align: 0 },
                |acc, l| Layout {
                    size: acc.size.max(l.size),
                    align: acc.align.max(l.align),
                },
            );
            let align = payload.align.max(tag_bytes);
            let size = align_up(Layout {
                size: tag_bytes + payload.size,
                align,
            });
            Layout { size, align }
        }
    }
}

/// Computes the layout of a struct by walking its fields in order, inserting alignment padding
/// between fields and after the last field so the struct size is a multiple of its alignment.
fn struct_layout(fields: &[TypeSpec], arch: Arch) -> Layout {
    let mut offset = 0u64;
    let mut align = 1u64;
    for field in fields {
        let field_layout = type_layout(field, arch);
        offset = align_up(Layout {
            size: offset,
            align: field_layout.align,
        });
        offset += field_layout.size;
        align = align.max(field_layout.align);
    }
    let layout = Layout {
        size: offset,
        align,
    };
    let size = align_up(layout);

    Layout { size, align }
}

/// Rounds layout.size up to the nearest multiple of layout.align.
///
/// ```
/// align_up(Layout { size: 1, align: 8 }) == 8   // 1 → next multiple of 8
/// align_up(Layout { size: 8, align: 8 }) == 8   // already aligned, unchanged
/// align_up(Layout { size: 9, align: 8 }) == 16  // 9 → next multiple of 8
/// align_up(Layout { size: 5, align: 4 }) == 8   // 5 → next multiple of 4
/// align_up(Layout { size: 0, align: 0 }) == 0   // zero-sized type, no-op
/// ```
fn align_up(layout: Layout) -> u64 {
    if layout.align == 0 {
        return layout.size;
    }
    let sum = layout.size + layout.align - 1;
    let align = layout.align - 1;
    sum & !align
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

fn get_variant_tag(type_spec: &hir::TypeSpec, variant_name: StrID) -> ConstValue {
    match typer::resolve_type(type_spec) {
        hir::TypeSpec::Enum(e) => {
            for (i, v) in e.variants.iter().enumerate() {
                if variant_name == v.name {
                    return ConstValue::Int(i as u64);
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
    use std::collections::{BTreeMap, HashMap};
    use std::fs;
    use std::path::Path;

    use crate::file_set::{File, FileSet};
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
        let file = File::new(file_name.to_string(), source);
        let file_set = FileSet::new_from_files(std::path::PathBuf::new(), vec![file]);
        let parser = Parser::new(&file_set);
        let module = parser.parse_module(&mut str_store);

        let node_tree = node_module(&module);
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

    // --- align_up tests ---

    #[test]
    fn align_up_already_aligned() {
        assert_eq!(align_up(Layout { size: 8, align: 8 }), 8);
    }

    #[test]
    fn align_up_unaligned() {
        assert_eq!(align_up(Layout { size: 9, align: 8 }), 16);
    }

    #[test]
    fn align_up_one_byte_to_four() {
        assert_eq!(align_up(Layout { size: 1, align: 4 }), 4);
    }

    #[test]
    fn align_up_zero_size() {
        assert_eq!(align_up(Layout { size: 0, align: 8 }), 0);
    }

    #[test]
    fn align_up_zero_align_is_noop() {
        assert_eq!(align_up(Layout { size: 7, align: 0 }), 7);
    }

    // --- struct_layout tests ---

    #[test]
    fn struct_layout_empty() {
        let l = struct_layout(&[], Arch::W64);
        assert_eq!(l.size, 0);
        assert_eq!(l.align, 1);
    }

    #[test]
    fn struct_layout_single_field() {
        // Just an i32: size=4, align=4, no padding needed
        let l = struct_layout(&[TypeSpec::I32], Arch::W64);
        assert_eq!(l.size, 4);
        assert_eq!(l.align, 4);
    }

    #[test]
    fn struct_layout_padding_between_fields() {
        // u8 then i32: u8 at 0, pad to 4, i32 at 4 → total 8
        let l = struct_layout(&[TypeSpec::U8, TypeSpec::I32], Arch::W64);
        assert_eq!(l.size, 8);
        assert_eq!(l.align, 4);
    }

    #[test]
    fn struct_layout_trailing_padding() {
        // i32 then u8: i32 at 0, u8 at 4, raw end=5, padded to 8
        let l = struct_layout(&[TypeSpec::I32, TypeSpec::U8], Arch::W64);
        assert_eq!(l.size, 8);
        assert_eq!(l.align, 4);
    }

    #[test]
    fn struct_layout_all_same_alignment() {
        // Three i32s: no padding, size=12, align=4
        let l = struct_layout(&[TypeSpec::I32, TypeSpec::I32, TypeSpec::I32], Arch::W64);
        assert_eq!(l.size, 12);
        assert_eq!(l.align, 4);
    }

    #[test]
    fn struct_layout_w32_pointer_size() {
        // A pointer on W32 is 4 bytes
        let l = struct_layout(&[TypeSpec::OpaquePtr], Arch::W32);
        assert_eq!(l.size, 4);
        assert_eq!(l.align, 4);
    }

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
                public_decls: HashMap::from([]),
                type_map: SideTable {
                    keys: BTreeMap::from([(NodeID::from_usize(1), 0), (NodeID::from_usize(4), 1),]),
                    values: vec![hir::TypeSpec::Int32, hir::TypeSpec::Int32],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![],
                },
                within_loop: false,
            },
            want: MirModule {
                globals: vec![],
                init: MirFunction {
                    blocks: vec![Some(BasicBlock {
                        instructions: vec![],
                        terminator: Terminator::Return { value: None },
                    })],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![],
                    name: str_store::INIT,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    params: vec![],
                    return_type: TypeSpec::Unit,
                    value_types: vec![],
                },
                functions: vec![MirFunction {
                    name: StrID::from_usize(1),
                    params: vec![],
                    return_type: TypeSpec::I32,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![
                        Some(BasicBlock {
                            instructions: vec![ValueId::from_usize(1)],
                            terminator: Terminator::Return {
                                value: Some(ValueId::from_usize(1)),
                            },
                        }),
                        None
                    ],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![Instruction::Const {
                        value: ConstValue::Int(42),
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
                public_decls: HashMap::from([]),
                type_map: SideTable {
                    keys: BTreeMap::from([(NodeID::from_usize(1), 0), (NodeID::from_usize(4), 1)]),
                    values: vec![hir::TypeSpec::Bool, hir::TypeSpec::Bool],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![]
                },
                within_loop: false,
            },
            want: MirModule {
                globals: vec![],
                init: MirFunction {
                    name: str_store::INIT,
                    params: vec![],
                    return_type: TypeSpec::Unit,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![Some(BasicBlock {
                        instructions: vec![],
                        terminator: Terminator::Return { value: None },
                    }),],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![],
                    value_types: vec![],
                },
                functions: vec![MirFunction {
                    name: StrID::from_usize(1),
                    params: vec![],
                    return_type: TypeSpec::Bool,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![
                        Some(BasicBlock {
                            instructions: vec![ValueId::from_usize(1)],
                            terminator: Terminator::Return {
                                value: Some(ValueId::from_usize(1))
                            },
                        }),
                        None
                    ],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![Instruction::Const {
                        value: ConstValue::Bool(true)
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
                public_decls: HashMap::from([]),
                type_map: SideTable {
                    keys: BTreeMap::from([(NodeID::from_usize(1), 0), (NodeID::from_usize(4), 1)]),
                    values: vec![hir::TypeSpec::Bool, hir::TypeSpec::Bool],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![]
                },
                within_loop: false,
            },
            want: MirModule {
                globals: vec![],
                init: MirFunction {
                    name: str_store::INIT,
                    params: vec![],
                    return_type: TypeSpec::Unit,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![Some(BasicBlock {
                        instructions: vec![],
                        terminator: Terminator::Return { value: None },
                    })],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![],
                    value_types: vec![],
                },
                functions: vec![MirFunction {
                    name: StrID::from_usize(1),
                    params: vec![],
                    return_type: TypeSpec::Bool,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![
                        Some(BasicBlock {
                            instructions: vec![ValueId::from_usize(1)],
                            terminator: Terminator::Return {
                                value: Some(ValueId::from_usize(1))
                            },
                        }),
                        None
                    ],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![Instruction::Const {
                        value: ConstValue::Bool(false)
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
                public_decls: HashMap::from([]),
                type_map: SideTable {
                    keys: BTreeMap::from([(NodeID::from_usize(1), 0), (NodeID::from_usize(4), 1)]),
                    values: vec![hir::TypeSpec::Float64, hir::TypeSpec::Float64],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![]
                },
                within_loop: false,
            },
            want: MirModule {
                globals: vec![],
                init: MirFunction {
                    name: str_store::INIT,
                    params: vec![],
                    return_type: TypeSpec::Unit,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![Some(BasicBlock {
                        instructions: vec![],
                        terminator: Terminator::Return { value: None },
                    })],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![],
                    value_types: vec![],
                },
                functions: vec![MirFunction {
                    name: StrID::from_usize(1),
                    params: vec![],
                    return_type: TypeSpec::F64,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![
                        Some(BasicBlock {
                            instructions: vec![ValueId::from_usize(1)],
                            terminator: Terminator::Return {
                                value: Some(ValueId::from_usize(1))
                            },
                        }),
                        None
                    ],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![Instruction::Const {
                        value: ConstValue::Float(3.45)
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
                public_decls: HashMap::from([]),
                type_map: SideTable {
                    keys: BTreeMap::from([(NodeID::from_usize(1), 0), (NodeID::from_usize(4), 1)]),
                    values: vec![hir::TypeSpec::String, hir::TypeSpec::String],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![]
                },
                within_loop: false,
            },
            want: MirModule {
                globals: vec![],
                init: MirFunction {
                    name: str_store::INIT,
                    params: vec![],
                    return_type: TypeSpec::Unit,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![Some(BasicBlock {
                        instructions: vec![],
                        terminator: Terminator::Return { value: None },
                    })],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![],
                    value_types: vec![],
                },
                functions: vec![MirFunction {
                    name: StrID::from_usize(1),
                    params: vec![],
                    return_type: TypeSpec::String,
                    local_map: BTreeMap::new(),
                    locals: vec![],
                    blocks: vec![
                        Some(BasicBlock {
                            instructions: vec![ValueId::from_usize(1)],
                            terminator: Terminator::Return {
                                value: Some(ValueId::from_usize(1))
                            },
                        }),
                        None
                    ],
                    entry_block: BlockId::from_u32(1),
                    instructions: vec![Instruction::Const {
                        value: ConstValue::String(StrID::from_usize(99))
                    }],
                    value_types: vec![TypeSpec::String],
                }],
            },
        },
    );
}

use std::collections::HashSet;

use crate::hir::{self, Node, NodeID, PatternNode};
use crate::mir::{
    BasicBlock, BlockId, ConstValue, Instruction, Local, LocalId, MirFunction, MirModule,
    SwitchArm, TagSize, Terminator, TypeSpec, ValueId,
};
use crate::noder::{NodeTree, typer};
use crate::str_store::{self, StrID};

#[derive(Debug, Clone)]
pub struct BlockBuilder {
    instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
}

impl BlockBuilder {
    fn new() -> Self {
        BlockBuilder {
            instructions: vec![],
            terminator: None,
        }
    }

    fn add_instruction(&mut self, inst: Instruction) {
        if self.is_closed() {
            panic!("can not add instructions to a block that's aready closed");
        }
        self.instructions.push(inst);
    }

    fn set_terminator(&mut self, term: Terminator) {
        if self.terminator.is_some() {
            panic!("terminator was already set for this block")
        }

        self.terminator = Some(term);
    }

    fn is_closed(&self) -> bool {
        self.terminator.is_some()
    }

    fn to_basic_block(&self) -> BasicBlock {
        let term = match &self.terminator {
            Some(term) => term,
            None => {
                eprintln!(
                    "TODO: basic block must have a terminator setting to unreachable for now"
                );
                &Terminator::Unreachable
            }
        };

        let mut created_values = HashSet::new();
        let mut block_args = vec![];
        for inst in self.instructions.clone() {
            match inst {
                Instruction::Const { result, .. } => {
                    created_values.insert(result);
                }
                Instruction::UnaryOp {
                    result, operand, ..
                } => {
                    created_values.insert(result);

                    if !created_values.contains(&operand) {
                        block_args.push(operand)
                    }
                }
                Instruction::BinaryOp {
                    result, lhs, rhs, ..
                } => {
                    created_values.insert(result);

                    if !created_values.contains(&lhs) {
                        block_args.push(lhs)
                    }
                    if !created_values.contains(&rhs) {
                        block_args.push(rhs);
                    }
                }
                Instruction::LoadLocal { result, .. } => {
                    created_values.insert(result);
                }
                Instruction::StoreLocal { value, .. } => {
                    if !created_values.contains(&value) {
                        block_args.push(value)
                    }
                }
                Instruction::Call { result, args, .. } => {
                    created_values.insert(result);

                    for arg in args {
                        if !created_values.contains(&arg) {
                            block_args.push(arg)
                        }
                    }
                }
                Instruction::CallTry { result, args, .. } => {
                    created_values.insert(result);

                    for arg in args {
                        if !created_values.contains(&arg) {
                            block_args.push(arg)
                        }
                    }
                }
                Instruction::VariantGetPayload { result, src, .. } => {
                    created_values.insert(result);

                    if !created_values.contains(&src) {
                        block_args.push(src)
                    }
                }
                Instruction::VariantGetTag { result, src } => {
                    created_values.insert(result);

                    if !created_values.contains(&src) {
                        block_args.push(src)
                    }
                }
                Instruction::Move { src, .. } => {
                    if !created_values.contains(&src) {
                        block_args.push(src)
                    }
                }
                Instruction::Copy { src, .. } => {
                    if !created_values.contains(&src) {
                        block_args.push(src)
                    }
                }
                Instruction::DropLocal { .. } => { /* no value IDs here */ }
                Instruction::DeclareLocal { .. } => { /* no value IDs here */ }
                Instruction::SetInitialized { .. } => { /* no value IDs here */ }
            }
        }

        BasicBlock {
            block_args,
            instructions: self.instructions.clone(),
            terminator: term.clone(),
        }
    }
}

pub struct FunctionBuilder {
    name: StrID,
    params: Vec<StrID>,
    type_spec: TypeSpec,
    locals: Vec<Local>,         // Indexed by LocalId
    blocks: Vec<BlockBuilder>,  // Indexed by BlockId
    value_types: Vec<TypeSpec>, // Indexed by ValueId
}

impl FunctionBuilder {
    fn new(name: StrID, params: Vec<StrID>, type_spec: TypeSpec) -> Self {
        FunctionBuilder {
            name,
            type_spec,
            params,
            locals: vec![],
            blocks: vec![],
            value_types: vec![],
        }
    }

    fn emit_variant_get_tag(
        &mut self,
        block: BlockId,
        target: ValueId,
        target_type: TypeSpec,
    ) -> ValueId {
        let tag_type = match target_type {
            TypeSpec::Enum { tag_size, .. } => match tag_size {
                TagSize::U8 => TypeSpec::U8,
                TagSize::U16 => TypeSpec::U16,
                TagSize::U32 => TypeSpec::U32,
                TagSize::U64 => TypeSpec::U64,
            },
            _ => panic!("incorrect type for enum match"),
        };

        let result = self.add_value(tag_type);
        self.add_instruction(
            block,
            Instruction::VariantGetTag {
                result,
                src: target,
            },
        );

        result
    }

    fn emit_variant_get_payload(
        &mut self,
        block: BlockId,
        target: ValueId,
        variant_id: ConstValue,
        result_type: TypeSpec,
    ) -> ValueId {
        let result = self.add_value(result_type);
        self.add_instruction(
            block,
            Instruction::VariantGetPayload {
                result,
                src: target,
                variant_id,
            },
        );

        result
    }

    fn emit_store_local(&mut self, block: BlockId, local: LocalId, value: ValueId) {
        self.add_instruction(block, Instruction::StoreLocal { local, value });
    }

    fn add_instruction(&mut self, block_id: BlockId, inst: Instruction) {
        let block = self.get_block_mut(block_id);
        block.add_instruction(inst);
    }

    fn set_terminator(&mut self, block_id: BlockId, term: Terminator) {
        let block = self.get_block_mut(block_id);
        block.set_terminator(term);
    }

    fn add_block(&mut self) -> BlockId {
        let block_builder = BlockBuilder::new();

        self.blocks.push(block_builder);
        BlockId::from_u32(self.blocks.len() as u32)
    }

    fn add_value(&mut self, type_spec: TypeSpec) -> ValueId {
        self.value_types.push(type_spec);
        ValueId::from_u32(self.value_types.len() as u32)
    }

    // TODO: locals should probably be 1:1 mapped to node_id so we're reusing them correctly.
    // also this should probably be `get_local` since we wont necessarily know if we're creating
    // a new local or reusing an existing one
    fn add_local(&mut self, name: StrID, type_spec: TypeSpec) -> LocalId {
        let local_id = LocalId::from_u32(self.locals.len() as u32 + 1);
        let local = Local { name, type_spec };

        self.locals.push(local);

        local_id
    }

    fn get_block(&self, block_id: BlockId) -> &BlockBuilder {
        match self.blocks.get(block_id.as_idx()) {
            Some(block) => block,
            None => panic!("Unknown block {:?}", block_id),
        }
    }

    fn get_block_mut(&mut self, block_id: BlockId) -> &mut BlockBuilder {
        match self.blocks.get_mut(block_id.as_idx()) {
            Some(block) => block,
            None => panic!("Unknown block {:?}", block_id),
        }
    }

    fn block_is_closed(&self, block_id: BlockId) -> bool {
        self.get_block(block_id).is_closed()
    }

    fn to_mir_function(&self) -> MirFunction {
        if self.blocks.is_empty() {
            panic!("function must have an entry block")
        };

        // it's possible for a basic block in the function to be empty if all other blocks
        // terminated without jumping to it. For example if every arm in a match statement
        // returns before the end of the function. here we take a quick walk through the
        // blocks and cull any that are not referenced by any other blocks in the function
        let mut valid_blocks = vec![];

        let mut block_stack = SetStack::new();
        block_stack.push(BlockId::from_u32(1));
        while let Some(b) = block_stack.pop() {
            let block_builder = self.blocks[b.as_idx()].clone();
            let block = block_builder.to_basic_block();

            match block.terminator {
                Terminator::Return { .. } => {}
                Terminator::Unreachable => {}
                Terminator::Jump { target } => {
                    block_stack.push(target);
                }
                Terminator::Branch {
                    true_target,
                    false_target,
                    ..
                } => {
                    block_stack.push(true_target);
                    block_stack.push(false_target);
                }
                Terminator::SwitchVariant {
                    default, ref arms, ..
                } => {
                    block_stack.push(default);
                    for arm in arms {
                        block_stack.push(arm.jump);
                    }
                }
            }

            valid_blocks.push(block);
        }

        MirFunction {
            name: self.name,
            params: self.params.clone(),
            type_spec: self.type_spec.clone(),
            blocks: valid_blocks,
            entry_block: BlockId::from_u32(1),
            locals: self.locals.clone(),
            value_types: self.value_types.clone(),
        }
    }
}

struct SetStack {
    queue: Vec<BlockId>,
    set: HashSet<BlockId>,
}

impl SetStack {
    fn new() -> Self {
        SetStack {
            queue: vec![],
            set: HashSet::new(),
        }
    }

    fn push(&mut self, block_id: BlockId) {
        if self.set.insert(block_id) {
            self.queue.push(block_id)
        }
    }

    fn pop(&mut self) -> Option<BlockId> {
        self.queue.pop()
    }
}

pub fn block_hir(node_tree: &NodeTree) -> MirModule {
    // init block constructed to contain global initializations
    let mut fn_builder = FunctionBuilder::new(str_store::INIT, vec![], TypeSpec::Unit);

    let mut init_block_id = fn_builder.add_block();

    for node_id in &node_tree.roots {
        match block_init_statement(node_tree, *node_id, &mut fn_builder, init_block_id) {
            Some(block) => init_block_id = block,
            None => break,
        }
    }

    // make sure we close the init block
    fn_builder.set_terminator(init_block_id, Terminator::Return { value: None });
    let init = fn_builder.to_mir_function();

    // block the rest of the functions
    let mut functions = vec![];
    for node_id in &node_tree.roots {
        if let Some(f) = block_root_node(node_tree, *node_id) {
            functions.push(f);
        }
    }

    MirModule::new(init, functions)
}

pub fn block_root_node(node_tree: &NodeTree, node_id: NodeID) -> Option<MirFunction> {
    let node = match node_tree.get_node(node_id) {
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
            let name = get_ident_name(node_tree, ident);

            let params: Vec<StrID> = params
                .iter()
                .map(|node_id| {
                    let node = node_tree
                        .get_node(*node_id)
                        .expect("failed to find param node");

                    let node_id = if let Node::VarDecl { ident } = node {
                        ident
                    } else {
                        panic!("param must be a var decl but was a {:?}", node)
                    };

                    let node = node_tree
                        .get_node(*node_id)
                        .expect("failed to find param identifier");

                    if let Node::Identifier { name, .. } = node {
                        *name
                    } else {
                        panic!("param mut be an identifier but was a {:?}", node)
                    }
                })
                .collect();

            let return_type = match node_tree.get_type(node_id) {
                Some(ts) => lower_type_spec(ts),
                None => panic!("missing type for function decl"),
            };

            let mut fn_builder = FunctionBuilder::new(name, params, return_type);

            let block_id = fn_builder.add_block();
            block_statement(node_tree, body, &mut fn_builder, block_id);

            Some(fn_builder.to_mir_function())
        }
        _ => None,
    }
}

pub fn block_init_statement(
    node_tree: &NodeTree,
    node_id: NodeID,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
) -> Option<BlockId> {
    if fn_builder.block_is_closed(block_id) {
        // if the block is closed just skip all the remaining instructions as they are no longer
        // reachable, trying to adding them would cause a panic
        return None;
    }

    let node = match node_tree.get_node(node_id) {
        Some(n) => n,
        None => panic!("type checking unknown node"),
    };
    let node = node.clone();

    match node {
        // Function declarations are skipped in the init function
        Node::FunctionDecl { .. } => Some(block_id),
        Node::Return { .. } => panic!("can not return from the root context"),
        _ => {
            let block_id = block_statement(node_tree, node_id, fn_builder, block_id);
            match block_id {
                Some(block) => Some(block),
                None => panic!("invalid root statement"),
            }
        }
    }
}

pub fn block_statement(
    node_tree: &NodeTree,
    node_id: NodeID,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
) -> Option<BlockId> {
    if fn_builder.block_is_closed(block_id) {
        // if the block is closed just skip all the remaining instructions as they are no longer
        // reachable, trying to adding them would cause a panic
        return None;
    }

    let node = match node_tree.get_node(node_id) {
        Some(n) => n,
        None => panic!("type checking unknown node"),
    };
    let node = node.clone();

    match node {
        Node::Invalid => {
            let result = fn_builder.add_value(TypeSpec::Unit);
            fn_builder.add_instruction(
                block_id,
                Instruction::Call {
                    result,
                    func: str_store::PANIC,
                    // TODO: what are the args here? Should the 'Node::Invalid' have associated error
                    // info so that we panic with a syntax error or something?
                    args: vec![],
                },
            );
            fn_builder.set_terminator(block_id, Terminator::Unreachable);

            // return a None because this block is closed and there's no more blocks that we know
            // about at this level
            None
        }
        Node::If {
            condition,
            then_block,
            else_block,
        } => {
            // the condition needs to be computed in the previous block, expressions can trigger
            // control flow so this should never create a new block
            let expr_value = block_expression(node_tree, condition, fn_builder, block_id);

            let true_block_id = fn_builder.add_block();
            let merge_block_id = fn_builder.add_block();

            let block = block_statement(node_tree, then_block, fn_builder, true_block_id);
            if let Some(b) = block {
                fn_builder.set_terminator(
                    b,
                    Terminator::Jump {
                        target: merge_block_id,
                    },
                )
            }

            let mut false_block_id = merge_block_id;
            if let Some(e) = else_block {
                false_block_id = fn_builder.add_block();
                let block = block_statement(node_tree, e, fn_builder, false_block_id);
                if let Some(b) = block {
                    fn_builder.set_terminator(
                        b,
                        Terminator::Jump {
                            target: merge_block_id,
                        },
                    );
                }
            }

            fn_builder.set_terminator(
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
            let discriminant_type = node_tree
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
                    let merge_block =
                        block_int_match(node_tree, fn_builder, block_id, target, arms);
                    Some(merge_block)
                }
                hir::TypeSpec::Enum(_) => {
                    let merge_block =
                        block_enum_match(node_tree, fn_builder, block_id, target, arms);
                    Some(merge_block)
                }
                hir::TypeSpec::UnsafePtr => {
                    let merge_block =
                        block_type_match(node_tree, fn_builder, block_id, target, arms);
                    Some(merge_block)
                }
                _ => {
                    eprintln!("TODO: other expressions need to be converted into if blocks");
                    Some(block_id)
                }
            }
        }
        Node::Block { statements } => {
            let mut current_block = block_id;
            for stmt in statements {
                match block_statement(node_tree, stmt, fn_builder, current_block) {
                    Some(b) => current_block = b,
                    None => return None,
                }
            }

            Some(current_block)
        }
        Node::Return { value } => {
            let ret = if let Some(v) = value {
                let ret = block_expression(node_tree, v, fn_builder, block_id);
                Some(ret)
            } else {
                None
            };

            fn_builder.set_terminator(block_id, Terminator::Return { value: ret });

            None
        }
        _ => {
            // TODO: remove me once all the nodes have been covered.
            // for now just return the block
            eprintln!(
                "TODO: skipping {:?} for now and just returning the block_id",
                node
            );
            Some(block_id)
        }
    }
}

fn block_int_match(
    node_tree: &NodeTree,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
    target: NodeID,
    arms: Vec<NodeID>,
) -> BlockId {
    let merge_block = fn_builder.add_block();
    let discriminant = block_expression(node_tree, target, fn_builder, block_id);

    let mut match_arms = vec![];
    let mut default_arm = None;
    for arm in arms {
        let arm_block = fn_builder.add_block();
        let arm_node = node_tree.get_node(arm).expect("missing arm node");

        let (pattern, body) = match arm_node {
            Node::MatchArm { pattern, body } => (pattern, body),
            _ => panic!("expect a match arm"),
        };

        let pattern = node_tree
            .get_node(*pattern)
            .expect("missing pattern for match arm");
        let pattern = match pattern {
            Node::Pattern(p) => p,
            _ => panic!("pattern was an invalid node"),
        };

        match pattern {
            PatternNode::IntLiteral(i) => {
                // Int literal patterns can never have a payload so this is pretty easy
                let block = block_statement(node_tree, *body, fn_builder, arm_block);
                if let Some(b) = block {
                    fn_builder.set_terminator(
                        b,
                        Terminator::Jump {
                            target: merge_block,
                        },
                    );
                }

                match_arms.push(SwitchArm {
                    target: ConstValue::ConstInt(*i),
                    jump: arm_block,
                });
            }
            PatternNode::Default(pat) => {
                if let Some(p) = pat.payload {
                    let name = get_ident_name(node_tree, p);

                    let target_ts = node_tree
                        .get_type(target)
                        .expect("missing type for match target");
                    let ts = lower_type_spec(target_ts);

                    let payload_local = fn_builder.add_local(name, ts);
                    fn_builder.emit_store_local(arm_block, payload_local, discriminant);
                }

                let block = block_statement(node_tree, *body, fn_builder, arm_block);
                if let Some(b) = block {
                    fn_builder.set_terminator(
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
            eprintln!("TODO: currently we don't always insist that pattern matching is exaustive");
            merge_block
        }
    };

    fn_builder.set_terminator(
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
    node_tree: &NodeTree,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
    target: NodeID,
    arms: Vec<NodeID>,
) -> BlockId {
    let merge_block = fn_builder.add_block();
    let target_id = block_expression(node_tree, target, fn_builder, block_id);

    let target_ts = node_tree
        .get_type(target)
        .expect("missing type for match target");
    let ts = lower_type_spec(target_ts);

    let discriminant = fn_builder.emit_variant_get_tag(block_id, target_id, ts);

    let mut match_arms = vec![];
    let mut default_arm = None;
    for arm in arms {
        let arm_block = fn_builder.add_block();
        let arm_node = node_tree.get_node(arm).expect("missing arm node");

        let (pattern, body) = match arm_node {
            Node::MatchArm { pattern, body } => (pattern, body),
            _ => panic!("expected a match arm"),
        };

        let pattern = node_tree
            .get_node(*pattern)
            .expect("missing pattern for match arm");
        let pattern = match pattern {
            Node::Pattern(p) => p,
            _ => panic!("pattern was an invalid node"),
        };

        match pattern {
            PatternNode::EnumVariant(pat) => {
                let variant_id = get_variant_id(target_ts, pat.variant);

                match pat.payload {
                    Some(p) => {
                        let ts = node_tree
                            .get_type(p)
                            .expect("missing type spec for enum variant");
                        let ts = lower_type_spec(ts);

                        let payload_value = fn_builder.emit_variant_get_payload(
                            arm_block,
                            target_id,
                            variant_id.clone(),
                            ts.clone(),
                        );

                        let name = get_ident_name(node_tree, p);
                        let payload_local = fn_builder.add_local(name, ts);
                        fn_builder.emit_store_local(arm_block, payload_local, payload_value);
                    }
                    None => {
                        // nothing to do because there's no payload to set up into a local
                    }
                }

                let block = block_statement(node_tree, *body, fn_builder, arm_block);
                if let Some(b) = block {
                    fn_builder.set_terminator(
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
                if let Some(p) = pat.payload {
                    let name = get_ident_name(node_tree, p);
                    let ts = lower_type_spec(target_ts);
                    let payload_local = fn_builder.add_local(name, ts);
                    fn_builder.emit_store_local(arm_block, payload_local, target_id);
                }

                let block = block_statement(node_tree, *body, fn_builder, arm_block);
                if let Some(b) = block {
                    fn_builder.set_terminator(
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
            eprintln!("TODO: currently we don't always insist that pattern matching is exaustive");
            merge_block
        }
    };

    fn_builder.set_terminator(
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
    node_tree: &NodeTree,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
    target: NodeID,
    arms: Vec<NodeID>,
) -> BlockId {
    let merge_block = fn_builder.add_block();
    let target_id = block_expression(node_tree, target, fn_builder, block_id);

    let target_ts = node_tree
        .get_type(target)
        .expect("missing type for match target");

    let mut success_arm = None;
    let mut default_arm = None;
    for arm in arms {
        let arm_block;
        let arm_node = node_tree.get_node(arm).expect("missing arm node");

        let (pattern_id, body) = match arm_node {
            Node::MatchArm { pattern, body } => (pattern, body),
            _ => panic!("expected a match arm"),
        };

        let pattern = node_tree
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
                arm_block = fn_builder.add_block();

                if let Some(p) = ts.payload {
                    let name = get_ident_name(node_tree, p);

                    let ts = node_tree
                        .get_type(*pattern_id)
                        .expect("missing type for type spec pattern");
                    let ts = lower_type_spec(ts);

                    // the target type will be UnsafePtr but we need this payload to change the
                    // type into whatever the target match is
                    let payload_local = fn_builder.add_local(name, ts);
                    fn_builder.emit_store_local(arm_block, payload_local, target_id);
                }

                let block = block_statement(node_tree, *body, fn_builder, arm_block);
                if let Some(b) = block {
                    fn_builder.set_terminator(
                        b,
                        Terminator::Jump {
                            target: merge_block,
                        },
                    );
                }

                success_arm = Some(arm_block);
            }
            PatternNode::Default(pat) => {
                arm_block = fn_builder.add_block();
                if let Some(p) = pat.payload {
                    let name = get_ident_name(node_tree, p);
                    let ts = lower_type_spec(target_ts);
                    let payload_local = fn_builder.add_local(name, ts);
                    fn_builder.emit_store_local(arm_block, payload_local, target_id);
                }

                let block = block_statement(node_tree, *body, fn_builder, arm_block);
                if let Some(b) = block {
                    fn_builder.set_terminator(
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

    fn_builder.set_terminator(
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

fn block_expression(
    node_tree: &NodeTree,
    node_id: NodeID,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
) -> ValueId {
    if fn_builder.block_is_closed(block_id) {
        // if the block is closed just skip all the remaining instructions as they are no longer
        // reachable, trying to adding them would cause a panic
        panic!("can not block expressions on a closed block");
    }

    let node = match node_tree.get_node(node_id) {
        Some(n) => n,
        None => panic!("type checking unknown node"),
    };
    let node = node.clone();

    match node {
        _ => {
            // TODO: implement this
            ValueId::nil()
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

fn get_ident_name(node_tree: &NodeTree, node_id: NodeID) -> StrID {
    let node = node_tree
        .get_node(node_id)
        .expect("missing identifier node");

    match node {
        Node::Identifier { name, .. } => *name,
        _ => panic!("node was not an identifier"),
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
    use std::fs;
    use std::path::Path;

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

        let mut str_store = crate::str_store::StrStore::new();
        let parser = crate::parser::Parser::new(source);
        let module = parser.parse_module(&mut str_store);

        let node_tree = crate::noder::node_module(module);
        let mir_module = block_hir(&node_tree);

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
}

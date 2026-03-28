use std::collections::{BTreeMap, HashSet};

use crate::hir::NodeID;
use crate::mir::{
    BasicBlock, BlockId, ConstValue, Instruction, Local, LocalId, MirFunction, Place, Projection,
    TagSize, Terminator, TypeSpec, ValueId,
};
use crate::str_store::{self, StrID};

#[derive(Debug, Clone)]
pub struct BlockBuilder {
    instructions: Vec<ValueId>,
    terminator: Option<Terminator>,
}

impl BlockBuilder {
    fn new() -> Self {
        BlockBuilder {
            instructions: vec![],
            terminator: None,
        }
    }

    fn add_instruction(&mut self, id: ValueId) {
        if self.is_closed() {
            panic!("can not add instructions to a block that's already closed");
        }
        self.instructions.push(id);
    }

    fn set_terminator(&mut self, term: Terminator) {
        if self.terminator.is_some() {
            panic!("terminator was already set for this block")
        }

        self.terminator = Some(term);
    }

    fn update_terminator(&mut self, term: Terminator) {
        if self.terminator.is_none() {
            panic!("terminator was not set, can not update it")
        }

        self.terminator = Some(term)
    }

    fn is_closed(&self) -> bool {
        self.terminator.is_some()
    }

    fn to_basic_block(&self, all_instructions: &[Instruction]) -> BasicBlock {
        let term = match &self.terminator {
            Some(term) => term,
            None => {
                eprintln!(
                    "TODO: basic block must have a terminator setting to unreachable for now"
                );
                &Terminator::Unreachable
            }
        };

        // All values defined in this block — any referenced value not in this set is a block arg.
        // note that because we do this before checking for block arguments which means there are
        // cases where values get used before they are defined. This means when building
        // instructions we must be careful to ensure we always create values BEFORE they are used
        // or they should not be created at all
        let created_values: HashSet<ValueId> = self.instructions.iter().cloned().collect();
        let mut block_args = vec![];

        for &id in &self.instructions {
            let inst = &all_instructions[id.as_idx()];
            for input in instruction_inputs(inst) {
                if !created_values.contains(&input) {
                    block_args.push(input);
                }
            }
        }

        BasicBlock {
            block_args,
            instructions: self.instructions.clone(),
            terminator: term.clone(),
        }
    }
}

fn place_inputs(place: &Place) -> Vec<ValueId> {
    place
        .projections
        .iter()
        .filter_map(|p| match p {
            Projection::Index(v) => Some(*v),
            _ => None,
        })
        .collect()
}

fn instruction_inputs(inst: &Instruction) -> Vec<ValueId> {
    match inst {
        Instruction::Const { .. } => vec![],
        Instruction::Load { place } => place_inputs(place),
        Instruction::Store { place, value } => {
            let mut inputs = place_inputs(place);
            inputs.push(*value);
            inputs
        }
        Instruction::AddressOf { place } => place_inputs(place),
        Instruction::Call { args, .. } => args.clone(),
        Instruction::CallTry { args, .. } => args.clone(),
        Instruction::VariantGetPayload { src, .. } => vec![*src],
        Instruction::VariantGetTag { src } => vec![*src],
        Instruction::MakeVariant { payload, .. } => match payload {
            Some(p) => vec![*p],
            None => vec![],
        },
        Instruction::Move { src, .. } => vec![*src],
        Instruction::Copy { src, .. } => vec![*src],
        Instruction::DropLocal { .. } => vec![],
        Instruction::DeclareLocal { .. } => vec![],
        Instruction::SetInitialized { .. } => vec![],
        Instruction::Add { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::Sub { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::Mul { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::SDiv { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::UDiv { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::SMod { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::UMod { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::Equal { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::NotEqual { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::SLessThan { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::ULessThan { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::SLessThanEqual { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::ULessThanEqual { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::SGreaterThan { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::UGreaterThan { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::SGreaterThanEqual { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::UGreaterThanEqual { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::LogicalAnd { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::LogicalOr { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::BitwiseAnd { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::BitwiseOr { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::BitwiseXOr { lhs, rhs } => vec![*lhs, *rhs],
        Instruction::BoolNot { op } => vec![*op],
        Instruction::Negate { op } => vec![*op],
        Instruction::Alloc { meta_type } => vec![*meta_type],
        Instruction::Free { ptr } => vec![*ptr],
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

pub struct FunctionBuilder {
    name: StrID,
    params: Vec<LocalId>,
    return_type: TypeSpec,
    locals: Vec<Local>, // Indexed by LocalId
    local_map: BTreeMap<NodeID, LocalId>,
    blocks: Vec<BlockBuilder>, // Indexed by BlockId
    defer_blocks: Vec<BlockId>,
    instructions: Vec<Instruction>, // Flat instruction array, indexed by ValueId
    value_types: Vec<TypeSpec>,     // Parallel to instructions, indexed by ValueId
}

impl FunctionBuilder {
    pub fn new(name: StrID, return_type: TypeSpec) -> Self {
        FunctionBuilder {
            name,
            return_type,
            params: vec![],
            locals: vec![],
            local_map: BTreeMap::new(),
            blocks: vec![],
            defer_blocks: vec![],
            instructions: vec![],
            value_types: vec![],
        }
    }

    pub fn add_param(&mut self, node: NodeID, name: StrID, return_type: TypeSpec) {
        let local = self.get_local(node, name, return_type);
        self.params.push(local);
    }

    pub fn emit_add(&mut self, block_id: BlockId, left: ValueId, right: ValueId) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        self.add_instruction(
            block_id,
            ts.clone(),
            Instruction::Add {
                lhs: left,
                rhs: right,
            },
        )
    }

    pub fn emit_sub(&mut self, block_id: BlockId, left: ValueId, right: ValueId) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        self.add_instruction(
            block_id,
            ts.clone(),
            Instruction::Sub {
                lhs: left,
                rhs: right,
            },
        )
    }

    pub fn emit_mul(&mut self, block_id: BlockId, left: ValueId, right: ValueId) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        self.add_instruction(
            block_id,
            ts.clone(),
            Instruction::Mul {
                lhs: left,
                rhs: right,
            },
        )
    }

    pub fn emit_div(&mut self, block_id: BlockId, left: ValueId, right: ValueId) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        let inst = if is_signed_type(ts) {
            Instruction::SDiv {
                lhs: left,
                rhs: right,
            }
        } else {
            Instruction::UDiv {
                lhs: left,
                rhs: right,
            }
        };

        self.add_instruction(block_id, ts.clone(), inst)
    }

    pub fn emit_mod(&mut self, block_id: BlockId, left: ValueId, right: ValueId) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        let inst = if is_signed_type(ts) {
            Instruction::SMod {
                lhs: left,
                rhs: right,
            }
        } else {
            Instruction::UMod {
                lhs: left,
                rhs: right,
            }
        };

        self.add_instruction(block_id, ts.clone(), inst)
    }

    pub fn emit_equal(&mut self, block_id: BlockId, left: ValueId, right: ValueId) -> ValueId {
        self.add_instruction(
            block_id,
            TypeSpec::Bool,
            Instruction::Equal {
                lhs: left,
                rhs: right,
            },
        )
    }

    pub fn emit_not_equal(&mut self, block_id: BlockId, left: ValueId, right: ValueId) -> ValueId {
        self.add_instruction(
            block_id,
            TypeSpec::Bool,
            Instruction::NotEqual {
                lhs: left,
                rhs: right,
            },
        )
    }

    pub fn emit_less_than(&mut self, block_id: BlockId, left: ValueId, right: ValueId) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        let inst = if is_signed_type(ts) {
            Instruction::SLessThan {
                lhs: left,
                rhs: right,
            }
        } else {
            Instruction::ULessThan {
                lhs: left,
                rhs: right,
            }
        };

        self.add_instruction(block_id, TypeSpec::Bool, inst)
    }

    pub fn emit_less_or_equal(
        &mut self,
        block_id: BlockId,
        left: ValueId,
        right: ValueId,
    ) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        let inst = if is_signed_type(ts) {
            Instruction::SLessThanEqual {
                lhs: left,
                rhs: right,
            }
        } else {
            Instruction::ULessThanEqual {
                lhs: left,
                rhs: right,
            }
        };

        self.add_instruction(block_id, TypeSpec::Bool, inst)
    }

    pub fn emit_greater_than(
        &mut self,
        block_id: BlockId,
        left: ValueId,
        right: ValueId,
    ) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        let inst = if is_signed_type(ts) {
            Instruction::SGreaterThan {
                lhs: left,
                rhs: right,
            }
        } else {
            Instruction::UGreaterThan {
                lhs: left,
                rhs: right,
            }
        };

        self.add_instruction(block_id, TypeSpec::Bool, inst)
    }

    pub fn emit_greater_or_equal(
        &mut self,
        block_id: BlockId,
        left: ValueId,
        right: ValueId,
    ) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        let inst = if is_signed_type(ts) {
            Instruction::SGreaterThanEqual {
                lhs: left,
                rhs: right,
            }
        } else {
            Instruction::UGreaterThanEqual {
                lhs: left,
                rhs: right,
            }
        };

        self.add_instruction(block_id, TypeSpec::Bool, inst)
    }

    pub fn emit_logical_and(
        &mut self,
        block_id: BlockId,
        left: ValueId,
        right: ValueId,
    ) -> ValueId {
        self.add_instruction(
            block_id,
            TypeSpec::Bool,
            Instruction::LogicalAnd {
                lhs: left,
                rhs: right,
            },
        )
    }

    pub fn emit_logical_or(&mut self, block_id: BlockId, left: ValueId, right: ValueId) -> ValueId {
        self.add_instruction(
            block_id,
            TypeSpec::Bool,
            Instruction::LogicalOr {
                lhs: left,
                rhs: right,
            },
        )
    }

    pub fn emit_bitwise_and(
        &mut self,
        block_id: BlockId,
        left: ValueId,
        right: ValueId,
    ) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        self.add_instruction(
            block_id,
            ts.clone(),
            Instruction::BitwiseAnd {
                lhs: left,
                rhs: right,
            },
        )
    }

    pub fn emit_bitwise_or(&mut self, block_id: BlockId, left: ValueId, right: ValueId) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        self.add_instruction(
            block_id,
            ts.clone(),
            Instruction::BitwiseOr {
                lhs: left,
                rhs: right,
            },
        )
    }

    pub fn emit_bitwise_xor(
        &mut self,
        block_id: BlockId,
        left: ValueId,
        right: ValueId,
    ) -> ValueId {
        let ts = self
            .value_types
            .get(left.as_idx())
            .expect("missing type for given value id");

        self.add_instruction(
            block_id,
            ts.clone(),
            Instruction::BitwiseXOr {
                lhs: left,
                rhs: right,
            },
        )
    }

    pub fn emit_bool_not(&mut self, block_id: BlockId, value: ValueId) -> ValueId {
        self.add_instruction(block_id, TypeSpec::Bool, Instruction::BoolNot { op: value })
    }

    pub fn emit_negate(&mut self, block_id: BlockId, value: ValueId) -> ValueId {
        let ts = self
            .value_types
            .get(value.as_idx())
            .expect("missing type for given value id");

        self.add_instruction(block_id, ts.clone(), Instruction::Negate { op: value })
    }

    /// Emit an address-of for a place, producing a pointer of `result_type`.
    pub fn emit_address_of(
        &mut self,
        block_id: BlockId,
        place: Place,
        result_type: TypeSpec,
    ) -> ValueId {
        self.add_instruction(block_id, result_type, Instruction::AddressOf { place })
    }

    pub fn emit_alloc(&mut self, block_id: BlockId, meta_type: ValueId) -> ValueId {
        self.add_instruction(
            block_id,
            TypeSpec::OpaquePtr,
            Instruction::Alloc { meta_type },
        )
    }

    pub fn emit_free(&mut self, block_id: BlockId, ptr: ValueId) -> ValueId {
        self.add_instruction(block_id, TypeSpec::Unit, Instruction::Free { ptr })
    }

    pub fn emit_call(
        &mut self,
        block: BlockId,
        func: StrID,
        args: Vec<ValueId>,
        return_type: TypeSpec,
    ) -> ValueId {
        self.add_instruction(block, return_type, Instruction::Call { func, args })
    }

    pub fn emit_variant_get_tag(
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

        self.add_instruction(block, tag_type, Instruction::VariantGetTag { src: target })
    }

    pub fn emit_variant_get_payload(
        &mut self,
        block: BlockId,
        target: ValueId,
        variant_id: ConstValue,
        result_type: TypeSpec,
    ) -> ValueId {
        self.add_instruction(
            block,
            result_type,
            Instruction::VariantGetPayload {
                src: target,
                variant_id,
            },
        )
    }

    pub fn emit_make_variant(
        &mut self,
        block: BlockId,
        tag: ConstValue,
        payload: Option<ValueId>,
        enum_type: TypeSpec,
    ) -> ValueId {
        self.add_instruction(block, enum_type, Instruction::MakeVariant { tag, payload })
    }

    /// Emit a load from a place, producing a value of `type_spec`.
    pub fn emit_load(&mut self, block_id: BlockId, place: Place, type_spec: TypeSpec) -> ValueId {
        self.add_instruction(block_id, type_spec, Instruction::Load { place })
    }

    /// Emit a store of `value` to a place. Produces Unit.
    pub fn emit_store(&mut self, block_id: BlockId, place: Place, value: ValueId) {
        self.add_instruction(
            block_id,
            TypeSpec::Unit,
            Instruction::Store { place, value },
        );
    }

    pub fn emit_const(
        &mut self,
        block: BlockId,
        const_type: TypeSpec,
        value: ConstValue,
    ) -> ValueId {
        self.add_instruction(block, const_type, Instruction::Const { value })
    }

    /// Appends an instruction to the function's flat instruction array and records it in the given
    /// block. Returns the ValueId (= position in the flat array) for the instruction's result.
    fn add_instruction(
        &mut self,
        block_id: BlockId,
        type_spec: TypeSpec,
        inst: Instruction,
    ) -> ValueId {
        self.instructions.push(inst);
        self.value_types.push(type_spec);
        let id = ValueId::from_usize(self.instructions.len());
        let block = self.get_block_mut(block_id);
        block.add_instruction(id);
        id
    }

    pub fn set_terminator(&mut self, block_id: BlockId, term: Terminator) {
        let block = self.get_block_mut(block_id);
        block.set_terminator(term);
    }

    pub fn update_terminator(&mut self, block_id: BlockId, term: Terminator) {
        let block = self.get_block_mut(block_id);
        block.update_terminator(term);
    }

    pub fn add_block(&mut self) -> BlockId {
        let block_builder = BlockBuilder::new();

        self.blocks.push(block_builder);
        BlockId::from_u32(self.blocks.len() as u32)
    }

    pub fn add_defer_block(&mut self) -> BlockId {
        let block_builder = BlockBuilder::new();

        self.blocks.push(block_builder);
        let id = BlockId::from_u32(self.blocks.len() as u32);
        self.defer_blocks.push(id);

        id
    }

    /// Return an existing local if one is created for the given node and create a new one otherwise
    pub fn get_local(&mut self, node: NodeID, name: StrID, type_spec: TypeSpec) -> LocalId {
        if let Some(local_id) = self.local_map.get(&node) {
            return *local_id;
        }

        let local = Local { name, type_spec };
        self.locals.push(local);

        let local_id = LocalId::from_usize(self.locals.len());
        self.local_map.insert(node, local_id);

        local_id
    }

    /// Creates a new local. You must track the local ID yourself as it's not tracked in the locals
    /// map which ties a local to it's node ID in the HIR
    pub fn add_local(&mut self, name: StrID, type_spec: TypeSpec) -> LocalId {
        let local = Local { name, type_spec };
        self.locals.push(local);

        LocalId::from_usize(self.locals.len())
    }

    /// Find the given loacal if it exists
    pub fn find_local(&self, node: NodeID) -> Option<LocalId> {
        self.local_map.get(&node).cloned()
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

    pub fn is_block_closed(&self, block_id: BlockId) -> bool {
        self.get_block(block_id).is_closed()
    }

    pub fn build_mir_function(&mut self) -> MirFunction {
        if self.blocks.is_empty() {
            panic!("function must have an entry block")
        };

        if let Some(id) = self.defer_blocks.pop() {
            self.build_defer_blocks(id);
        }

        // it's possible for a basic block in the function to be empty if all other blocks
        // terminated without jumping to it. For example if every arm in a match statement
        // returns before the end of the function. here we take a quick walk through the
        // blocks and cull any that are not referenced by any other blocks in the function
        let mut valid_blocks = vec![];

        let mut block_stack = SetStack::new();
        block_stack.push(BlockId::from_u32(1));
        while let Some(b) = block_stack.pop() {
            let block_builder = self.blocks[b.as_idx()].clone();
            let block = block_builder.to_basic_block(&self.instructions);

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
            return_type: self.return_type.clone(),
            blocks: valid_blocks,
            entry_block: BlockId::from_u32(1),
            local_map: self.local_map.clone(),
            locals: self.locals.clone(),
            instructions: self.instructions.clone(),
            value_types: self.value_types.clone(),
        }
    }

    /// update the CFG so that defer blocks are correctly stacked and called in the place of any
    /// return's or other unwinding operators like panics
    fn build_defer_blocks(&mut self, first_block: BlockId) {
        let merge_block = self.add_block();
        let defer_local = match self.return_type.clone() {
            TypeSpec::Unit => {
                self.set_terminator(merge_block, Terminator::Return { value: None });
                None
            }
            ts => {
                let defer_local = self.add_local(str_store::DEFER, ts.clone());
                let value = self.emit_load(merge_block, Place::local(defer_local), ts);
                self.set_terminator(merge_block, Terminator::Return { value: Some(value) });
                Some(defer_local)
            }
        };

        // string all the defer blocks together so that each defer block jumps to the next in LIFO
        // order
        let mut block_id = first_block;
        while let Some(next_block) = self.defer_blocks.pop() {
            self.update_defer_block(block_id, next_block);
            block_id = next_block;
        }

        self.update_defer_block(block_id, merge_block);

        // update all other blocks in the builder so that return instructions instead write to the
        // defer_local and then jump to the first block

        let mut block_stack = SetStack::new();
        block_stack.push(BlockId::from_u32(1));
        while let Some(b) = block_stack.pop() {
            let block = self.get_block(b);
            match &block.terminator {
                Some(Terminator::Return { value }) => match (defer_local, value) {
                    (Some(local), Some(value)) => {
                        self.emit_store(b, Place::local(local), *value);
                        self.update_terminator(
                            b,
                            Terminator::Jump {
                                target: first_block,
                            },
                        );
                    }
                    (None, Some(_)) => panic!("missing defer_local to write to"),
                    (Some(_), None) => panic!("missing return value"),
                    (None, None) => {
                        self.update_terminator(
                            b,
                            Terminator::Jump {
                                target: first_block,
                            },
                        );
                    }
                },
                Some(Terminator::Jump { target }) => block_stack.push(*target),
                Some(Terminator::Branch {
                    true_target,
                    false_target,
                    ..
                }) => {
                    block_stack.push(*true_target);
                    block_stack.push(*false_target);
                }
                Some(Terminator::SwitchVariant { default, arms, .. }) => {
                    for arm in arms {
                        block_stack.push(arm.jump);
                    }
                    block_stack.push(*default);
                }
                Some(Terminator::Unreachable) => {
                    // TODO: should this be converted to jump into the defer blocks? Is there any
                    // situation where Unreachable means anything other than panic? We should
                    // probably have a specific panic type to make this more clear since
                    // technically unreachable should represent a distinct idea
                }
                None => panic!("missing terminator for block"),
            }
        }
    }

    fn update_defer_block(&mut self, block_id: BlockId, next_block: BlockId) {
        let mut block_stack = SetStack::new();
        block_stack.push(block_id);
        while let Some(b) = block_stack.pop() {
            let block = self.get_block(b);
            match &block.terminator {
                Some(Terminator::Return { .. }) => {
                    panic!("can not return from defer blocks")
                }
                Some(Terminator::Jump { target }) => block_stack.push(*target),
                Some(Terminator::Branch {
                    true_target,
                    false_target,
                    ..
                }) => {
                    block_stack.push(*true_target);
                    block_stack.push(*false_target);
                }
                Some(Terminator::SwitchVariant { default, arms, .. }) => {
                    for arm in arms {
                        block_stack.push(arm.jump);
                    }
                    block_stack.push(*default);
                }
                Some(Terminator::Unreachable) => {
                    eprintln!(
                        "TODO: this block should panic, stoping execution and stepping into the next branch"
                    );
                    self.update_terminator(b, Terminator::Jump { target: next_block });
                }
                None => {
                    self.set_terminator(b, Terminator::Jump { target: next_block });
                }
            }
        }
    }
}

fn is_signed_type(ts: &TypeSpec) -> bool {
    matches!(
        ts,
        TypeSpec::I8
            | TypeSpec::I16
            | TypeSpec::I32
            | TypeSpec::I64
            | TypeSpec::F32
            | TypeSpec::F64
    )
}

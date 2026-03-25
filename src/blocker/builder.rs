use std::collections::{BTreeMap, HashSet};

use crate::hir::NodeID;
use crate::mir::{
    BasicBlock, BlockId, ConstValue, Global, GlobalId, Instruction, Local, LocalId, MirFunction,
    TagSize, Terminator, TypeSpec, ValueId,
};
use crate::str_store::StrID;

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

fn instruction_inputs(inst: &Instruction) -> Vec<ValueId> {
    match inst {
        Instruction::Const { .. } => vec![],
        Instruction::UnaryOp { operand, .. } => vec![*operand],
        Instruction::BinaryOp { lhs, rhs, .. } => vec![*lhs, *rhs],
        Instruction::LoadLocal { .. } => vec![],
        Instruction::StoreLocal { value, .. } => vec![*value],
        Instruction::LoadGlobal { .. } => vec![],
        Instruction::StoreGlobal { value, .. } => vec![*value],
        Instruction::Call { args, .. } => args.clone(),
        Instruction::CallTry { args, .. } => args.clone(),
        Instruction::VariantGetPayload { src, .. } => vec![*src],
        Instruction::VariantGetTag { src } => vec![*src],
        Instruction::Move { src, .. } => vec![*src],
        Instruction::Copy { src, .. } => vec![*src],
        Instruction::DropLocal { .. } => vec![],
        Instruction::DeclareLocal { .. } => vec![],
        Instruction::SetInitialized { .. } => vec![],
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
    type_spec: TypeSpec,
    locals: Vec<Local>, // Indexed by LocalId
    local_map: BTreeMap<NodeID, LocalId>,
    blocks: Vec<BlockBuilder>,      // Indexed by BlockId
    instructions: Vec<Instruction>, // Flat instruction array, indexed by ValueId
    value_types: Vec<TypeSpec>,     // Parallel to instructions, indexed by ValueId
}

impl FunctionBuilder {
    pub fn new(name: StrID, type_spec: TypeSpec) -> Self {
        FunctionBuilder {
            name,
            type_spec,
            params: vec![],
            locals: vec![],
            local_map: BTreeMap::new(),
            blocks: vec![],
            instructions: vec![],
            value_types: vec![],
        }
    }

    pub fn add_param(&mut self, node: NodeID, name: StrID, type_spec: TypeSpec) {
        let local = self.get_local(node, name, type_spec);
        self.params.push(local);
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

    pub fn emit_store_local(&mut self, block: BlockId, local: LocalId, value: ValueId) {
        self.add_instruction(
            block,
            TypeSpec::Unit,
            Instruction::StoreLocal { local, value },
        );
    }

    pub fn emit_load_global(
        &mut self,
        block: BlockId,
        global: GlobalId,
        type_spec: TypeSpec,
    ) -> ValueId {
        self.add_instruction(block, type_spec, Instruction::LoadGlobal { global })
    }

    pub fn emit_load_local(&mut self, block: BlockId, node: NodeID) -> ValueId {
        let local = self
            .local_map
            .get(&node)
            .expect("unknown local does not exist");
        let local = *local;
        let ts = self
            .locals
            .get(local.as_idx())
            .expect("missing type for local")
            .clone()
            .type_spec;

        self.add_instruction(block, ts, Instruction::LoadLocal { local })
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

    pub fn add_block(&mut self) -> BlockId {
        let block_builder = BlockBuilder::new();

        self.blocks.push(block_builder);
        BlockId::from_u32(self.blocks.len() as u32)
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

    pub fn build_mir_function(&self) -> MirFunction {
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
            type_spec: self.type_spec.clone(),
            blocks: valid_blocks,
            entry_block: BlockId::from_u32(1),
            local_map: self.local_map.clone(),
            locals: self.locals.clone(),
            instructions: self.instructions.clone(),
            value_types: self.value_types.clone(),
        }
    }
}

use std::collections::{BTreeMap, HashMap, HashSet};

use crate::hir::NodeID;
use crate::mir::{
    BasicBlock, BlockId, ConstValue, Instruction, Local, LocalId, MirFunction, Place, SwitchArm,
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

    fn unset_terminator(&mut self) {
        if self.terminator.is_none() {
            panic!("terminator was not set, can not unset itit")
        }

        self.terminator = None;
    }

    fn is_closed(&self) -> bool {
        self.terminator.is_some()
    }

    fn to_basic_block(&self) -> BasicBlock {
        let term = match &self.terminator {
            Some(term) => term,
            None => panic!("blocks must have a valid terminator"),
        };

        BasicBlock {
            instructions: self.instructions.clone(),
            terminator: term.clone(),
        }
    }
}

// all the variations of the defer CFG that are terminated with the various
// control flow mechanisms. panic_cfg is the root CFG and all other CFG's are
// a clone of that CFG
struct DeferThreads {
    panic_cfg: Cfg,
    return_cfg: Cfg,
    merge_cfg: Cfg,
    break_cfg: Cfg,
    continue_cfg: Cfg,
}

impl DeferThreads {
    fn new(fn_builder: &mut FunctionBuilder, b: BlockId) -> Self {
        let panic_cfg = Cfg::new(fn_builder, b);
        if panic_cfg.can_return(fn_builder) {
            panic!("can not return from inside defer blocks")
        }
        if panic_cfg.can_break(fn_builder) {
            panic!("can not break from inside defer blocks")
        }
        if panic_cfg.can_continue(fn_builder) {
            panic!("can not continue from inside defer blocks")
        }

        let merge_defer = panic_cfg.clone(fn_builder);
        let merge_cfg = Cfg::new(fn_builder, merge_defer);

        let return_defer = panic_cfg.clone(fn_builder);
        let return_cfg = Cfg::new(fn_builder, return_defer);

        let break_defer = panic_cfg.clone(fn_builder);
        let break_cfg = Cfg::new(fn_builder, break_defer);

        let continue_defer = panic_cfg.clone(fn_builder);
        let continue_cfg = Cfg::new(fn_builder, continue_defer);

        DeferThreads {
            panic_cfg,
            return_cfg,
            merge_cfg,
            break_cfg,
            continue_cfg,
        }
    }
}

pub struct Cfg {
    entry: BlockId,
    blocks: Vec<BlockId>,
}

impl Cfg {
    pub fn new(fn_builder: &mut FunctionBuilder, entry: BlockId) -> Self {
        let blocks = Self::visit_all_blocks(fn_builder, entry);
        Cfg { entry, blocks }
    }

    fn visit_all_blocks(fn_builder: &mut FunctionBuilder, entry: BlockId) -> Vec<BlockId> {
        let mut visited = vec![];
        let mut stack = SetStack::new();
        stack.push(entry);
        while let Some(b) = stack.pop() {
            visited.push(b);

            let block = fn_builder.get_block(b);
            match &block.terminator {
                Some(Terminator::Jump { target }) => stack.push(*target),
                Some(Terminator::Branch {
                    true_target,
                    false_target,
                    ..
                }) => {
                    stack.push(*true_target);
                    stack.push(*false_target);
                }
                Some(Terminator::SwitchVariant { default, arms, .. }) => {
                    stack.push(*default);
                    for arm in arms {
                        stack.push(arm.jump)
                    }
                }
                Some(Terminator::Unreachable) => {}
                Some(Terminator::Panic { .. }) => {}
                Some(Terminator::Return { .. }) => {}
                Some(Terminator::Break) => {}
                Some(Terminator::Continue) => {}
                None => {}
            }
        }

        visited
    }

    /// Clone copies all blocks in a CFG and re-wires the full graph so the new blocks correctly
    /// point to each other. The instructions and values will be identical between the original and
    /// cloned blocks
    fn clone(&self, fn_builder: &mut FunctionBuilder) -> BlockId {
        let block_map: HashMap<BlockId, BlockId> = self
            .blocks
            .iter()
            .map(|&id| (id, fn_builder.clone_block(id)))
            .collect();

        for (&old_id, &new_id) in &block_map {
            let term = fn_builder.get_block(old_id).terminator.clone();
            if let Some(t) = Self::remap_terminator(term, &block_map) {
                fn_builder.unset_terminator(new_id);
                fn_builder.set_terminator(new_id, t);
            }
        }

        block_map[&self.entry]
    }

    fn remap_terminator(
        term: Option<Terminator>,
        block_map: &HashMap<BlockId, BlockId>,
    ) -> Option<Terminator> {
        match term {
            Some(Terminator::Jump { target }) => {
                let target = block_map[&target];
                Some(Terminator::Jump { target })
            }
            Some(Terminator::Branch {
                cond,
                true_target,
                false_target,
            }) => {
                let true_target = block_map[&true_target];
                let false_target = block_map[&false_target];

                Some(Terminator::Branch {
                    cond,
                    true_target,
                    false_target,
                })
            }
            Some(Terminator::SwitchVariant {
                discriminant,
                default,
                arms,
            }) => {
                let mut new_arms = vec![];
                for arm in arms {
                    let jump = block_map[&arm.jump];
                    new_arms.push(SwitchArm {
                        target: arm.target,
                        jump,
                    })
                }

                let default = block_map[&default];
                Some(Terminator::SwitchVariant {
                    discriminant,
                    default,
                    arms: new_arms,
                })
            }
            Some(Terminator::Unreachable) => None,
            Some(Terminator::Panic { .. }) => None,
            Some(Terminator::Return { .. }) => None,
            Some(Terminator::Break) => None,
            Some(Terminator::Continue) => None,
            None => None,
        }
    }

    // this returns true only if all blocks in the graph terminate correctly
    pub fn all_blocks_terminate(&self, fn_builder: &FunctionBuilder) -> bool {
        for block_id in &self.blocks {
            if !fn_builder.is_block_closed(*block_id) {
                return false;
            }
        }

        true
    }

    /// this function replaces all break terminators with unconditional jumps to the target block
    pub fn break_to(&self, fn_builder: &mut FunctionBuilder, break_to: BlockId) {
        for block_id in &self.blocks {
            let block = fn_builder.get_block_mut(*block_id);

            if matches!(block.terminator, Some(Terminator::Break)) {
                fn_builder.unset_terminator(*block_id);
                fn_builder.set_terminator(*block_id, Terminator::Jump { target: break_to });
            }
        }
    }

    /// this function returns true if any block in the CFG terminates in a break
    pub fn can_break(&self, fn_builder: &FunctionBuilder) -> bool {
        for block_id in &self.blocks {
            let block = fn_builder.get_block(*block_id);
            if matches!(block.terminator, Some(Terminator::Break)) {
                return true;
            }
        }

        false
    }

    pub fn continue_to(&self, fn_builder: &mut FunctionBuilder, continue_to: BlockId) {
        for block_id in &self.blocks {
            let block = fn_builder.get_block_mut(*block_id);

            if matches!(block.terminator, Some(Terminator::Continue)) {
                fn_builder.unset_terminator(*block_id);
                fn_builder.set_terminator(
                    *block_id,
                    Terminator::Jump {
                        target: continue_to,
                    },
                );
            }
        }
    }

    /// this function returns true if any block in the CFG terminates in a continue
    fn can_continue(&self, fn_builder: &FunctionBuilder) -> bool {
        for block_id in &self.blocks {
            let block = fn_builder.get_block(*block_id);
            if matches!(block.terminator, Some(Terminator::Continue)) {
                return true;
            }
        }

        false
    }

    /// returns true if any path through the CFG results in a return terminator
    fn can_return(&self, fn_builder: &FunctionBuilder) -> bool {
        for block_id in &self.blocks {
            let block = fn_builder.get_block(*block_id);
            if matches!(block.terminator, Some(Terminator::Return { .. })) {
                return true;
            }
        }

        false
    }

    // this function sets all the blocks with missing or continue terminators in the CFG
    // to jump to the specified block unconditionally
    fn jump_to(&self, fn_builder: &mut FunctionBuilder, jump_to: BlockId) {
        for block_id in &self.blocks {
            let block = fn_builder.get_block_mut(*block_id);
            if block.terminator.is_none() {
                block.terminator = Some(Terminator::Jump { target: jump_to })
            }
        }
    }

    // this function sets all the blocks with panic terminators in the CFG to jump to the
    // specified block unconditionally
    fn panic_to(&self, fn_builder: &mut FunctionBuilder, panic_to: BlockId) {
        for block_id in &self.blocks {
            let block = fn_builder.get_block_mut(*block_id);
            // TODO: need to set a panic value just like we do with return terminators
            // skipping for now though
            if matches!(block.terminator, Some(Terminator::Panic { .. })) {
                block.terminator = Some(Terminator::Jump { target: panic_to })
            }
        }
    }

    // this function sets all the blocks with return terminators in the CFG to jump to the
    // specified block unconditionally and set a local value for a future return
    fn return_to(
        &self,
        fn_builder: &mut FunctionBuilder,
        return_to: BlockId,
        local_id: Option<LocalId>,
    ) {
        for block_id in &self.blocks {
            let block = fn_builder.get_block_mut(*block_id);

            let value = match block.terminator.clone() {
                Some(Terminator::Return { value }) => value,
                _ => continue,
            };

            fn_builder.unset_terminator(*block_id);

            if let Some(local_id) = local_id {
                fn_builder.emit_store(*block_id, Place::local(local_id), value.unwrap())
            }

            fn_builder.set_terminator(*block_id, Terminator::Jump { target: return_to });
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

struct Scope {
    start: BlockId,
    defer: Vec<BlockId>,
}

pub struct FunctionBuilder {
    public: bool,
    name: StrID,
    params: Vec<LocalId>,
    return_type: TypeSpec,
    locals: Vec<Local>, // Indexed by LocalId
    local_map: BTreeMap<NodeID, LocalId>,
    blocks: Vec<BlockBuilder>, // Indexed by BlockId
    scopes: Vec<Scope>,
    instructions: Vec<Instruction>, // Flat instruction array, indexed by ValueId
    value_types: Vec<TypeSpec>,     // Parallel to instructions, indexed by ValueId
}

impl FunctionBuilder {
    pub fn new_public(name: StrID, return_type: TypeSpec) -> Self {
        FunctionBuilder {
            public: true,
            name,
            return_type,
            params: vec![],
            locals: vec![],
            local_map: BTreeMap::new(),
            blocks: vec![],
            scopes: vec![],
            instructions: vec![],
            value_types: vec![],
        }
    }

    pub fn new_private(name: StrID, return_type: TypeSpec) -> Self {
        FunctionBuilder {
            public: false,
            name,
            return_type,
            params: vec![],
            locals: vec![],
            local_map: BTreeMap::new(),
            blocks: vec![],
            scopes: vec![],
            instructions: vec![],
            value_types: vec![],
        }
    }

    pub fn open_scope(&mut self, block_id: BlockId) {
        self.scopes.push(Scope {
            start: block_id,
            defer: vec![],
        })
    }

    pub fn close_scope(&mut self) -> BlockId {
        let mut scope = self
            .scopes
            .pop()
            .expect("can not close scope, not currently in a scope");

        // String all the defer blocks together, we need distinct flows depending on how you
        // enterd the defer block, though a panic, a return statement, a normal scope close
        // (i.e. no explicit terminator), or a loop break/ continue
        let block = Cfg::new(self, scope.start);
        let mut defer_threads = match scope.defer.pop() {
            Some(b) => DeferThreads::new(self, b),
            None => {
                // If there's no defer block we can just jump from this scope directly into a merge
                // black and return that block id to continue gathering instructions
                let b = self.add_block();
                block.jump_to(self, b);
                return b;
            }
        };

        // jump from the original block CFG into the correct defer stack
        block.jump_to(self, defer_threads.merge_cfg.entry);
        block.panic_to(self, defer_threads.panic_cfg.entry);
        block.break_to(self, defer_threads.break_cfg.entry);
        block.continue_to(self, defer_threads.continue_cfg.entry);

        // need to create a local if this function has an expected return value
        let ret_local = match self.return_type.clone() {
            TypeSpec::Unit => None,
            ts => {
                let local = self.add_local(str_store::DEFER, ts);
                Some(local)
            }
        };
        block.return_to(self, defer_threads.return_cfg.entry, ret_local);

        // TODO: nee a similar local for tracking defer values

        // Now we need to wire up all the remaining defer blocks so they feed into each other
        // correctly. All blocks should jump into the next block in the sequece depending on the
        // entry path. The one execption is if any of the defer blocks panic in which case we jump
        // to the panic defer stack and continue from there. There is no escape from the panic
        // defer stack since we don't allow recovery from a panic defer as it leads to potentially
        // undefined return values
        while let Some(b) = scope.defer.pop() {
            let next_defer_threads = DeferThreads::new(self, b);

            defer_threads
                .panic_cfg
                .jump_to(self, next_defer_threads.panic_cfg.entry);
            defer_threads
                .merge_cfg
                .jump_to(self, next_defer_threads.merge_cfg.entry);
            defer_threads
                .return_cfg
                .jump_to(self, next_defer_threads.return_cfg.entry);
            defer_threads
                .break_cfg
                .jump_to(self, next_defer_threads.break_cfg.entry);
            defer_threads
                .continue_cfg
                .jump_to(self, next_defer_threads.continue_cfg.entry);

            defer_threads
                .panic_cfg
                .panic_to(self, next_defer_threads.panic_cfg.entry);
            defer_threads
                .merge_cfg
                .panic_to(self, next_defer_threads.panic_cfg.entry);
            defer_threads
                .return_cfg
                .panic_to(self, next_defer_threads.panic_cfg.entry);
            defer_threads
                .break_cfg
                .panic_to(self, next_defer_threads.panic_cfg.entry);
            defer_threads
                .continue_cfg
                .panic_to(self, next_defer_threads.panic_cfg.entry);

            defer_threads = next_defer_threads;
        }

        // wire the final defer blocks up so they correctly jump back into the flow either
        // returing, continuing to panic, or mergeing back into an empty merge block
        let final_panic_block = self.add_block();

        // TODO: need to actually get the panic value here in the same way we do returns
        self.set_terminator(
            final_panic_block,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );

        defer_threads.panic_cfg.jump_to(self, final_panic_block);
        defer_threads.panic_cfg.panic_to(self, final_panic_block);
        defer_threads.merge_cfg.panic_to(self, final_panic_block);
        defer_threads.return_cfg.panic_to(self, final_panic_block);
        defer_threads.break_cfg.panic_to(self, final_panic_block);
        defer_threads.continue_cfg.panic_to(self, final_panic_block);

        let final_return_block = self.add_block();
        match ret_local {
            Some(local_id) => {
                let ts = self.return_type.clone();
                let value = self.emit_load(final_return_block, Place::local(local_id), ts);
                self.set_terminator(
                    final_return_block,
                    Terminator::Return { value: Some(value) },
                );
            }
            None => {
                self.set_terminator(final_return_block, Terminator::Return { value: None });
            }
        }
        defer_threads.return_cfg.jump_to(self, final_return_block);

        let final_break_block = self.add_block();
        defer_threads.break_cfg.jump_to(self, final_break_block);
        self.set_terminator(final_break_block, Terminator::Break);

        let final_continue_block = self.add_block();
        defer_threads
            .continue_cfg
            .jump_to(self, final_continue_block);
        self.set_terminator(final_continue_block, Terminator::Continue);

        let final_merge_block = self.add_block();
        defer_threads.merge_cfg.jump_to(self, final_merge_block);

        final_merge_block
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
        target: Place,
        result_type: TypeSpec,
    ) -> ValueId {
        self.add_instruction(
            block,
            result_type,
            Instruction::VariantGetPayload { src: target },
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

    pub fn emit_make_struct(
        &mut self,
        block: BlockId,
        fields: Vec<ValueId>,
        struct_type: TypeSpec,
    ) -> ValueId {
        self.add_instruction(block, struct_type, Instruction::MakeStruct { fields })
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

    pub fn unset_terminator(&mut self, block_id: BlockId) {
        let block = self.get_block_mut(block_id);
        block.unset_terminator();
    }

    pub fn add_block(&mut self) -> BlockId {
        let block_builder = BlockBuilder::new();

        self.blocks.push(block_builder);
        BlockId::from_u32(self.blocks.len() as u32)
    }

    pub fn clone_block(&mut self, block_id: BlockId) -> BlockId {
        let clone = self.get_block(block_id).clone();
        self.blocks.push(clone);
        BlockId::from_u32(self.blocks.len() as u32)
    }

    pub fn add_defer_block(&mut self) -> BlockId {
        let block_builder = BlockBuilder::new();

        self.blocks.push(block_builder);
        let id = BlockId::from_u32(self.blocks.len() as u32);

        match self.scopes.last_mut() {
            Some(scope) => scope.defer.push(id),
            None => panic!("can not add defer block outside of a scope"),
        }

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

        // it's possible for a block to be unreachable after building and unreachable blocks
        // may not be fully valid. We travers the graph in DFS order here to visit all reachable
        // blocks to get a vector where reachable bocks are Some and unreachable blocks are None
        let mut valid_blocks = vec![None; self.blocks.len()];

        let mut block_stack = SetStack::new();
        block_stack.push(BlockId::from_u32(1));
        while let Some(b) = block_stack.pop() {
            let block_builder = self.blocks[b.as_idx()].clone();
            let block = block_builder.to_basic_block();

            match block.terminator {
                Terminator::Return { .. } => {}
                Terminator::Break => {}
                Terminator::Continue => {}
                Terminator::Unreachable => {}
                Terminator::Panic { .. } => {}
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

            valid_blocks[b.as_idx()] = Some(block);
        }

        MirFunction {
            public: self.public,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::str_store::StrID;

    fn test_builder() -> FunctionBuilder {
        FunctionBuilder::new_private(StrID::from_usize(1), TypeSpec::Unit)
    }

    // A single block with a Return terminator should be cloned to a new block
    // with a different ID but the same terminator.
    #[test]
    fn test_cfg_clone_single_block() {
        let mut fb = test_builder();
        let entry = fb.add_block();
        fb.set_terminator(entry, Terminator::Return { value: None });

        let cloned = Cfg::new(&mut fb, entry).clone(&mut fb);

        assert_ne!(entry, cloned);
        assert_eq!(
            fb.get_block(cloned).terminator,
            Some(Terminator::Return { value: None })
        );
    }

    // A → B(Return) linear chain: both blocks should be cloned and the cloned
    // entry should jump to the cloned successor, not the original.
    #[test]
    fn test_cfg_clone_linear_chain() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(b, Terminator::Return { value: None });

        let cloned_a = Cfg::new(&mut fb, a).clone(&mut fb);

        assert_ne!(a, cloned_a);
        let cloned_b = match fb.get_block(cloned_a).terminator.clone() {
            Some(Terminator::Jump { target }) => target,
            term => panic!("expected Jump, got {:?}", term),
        };
        assert_ne!(b, cloned_b);
        assert_eq!(
            fb.get_block(cloned_b).terminator,
            Some(Terminator::Return { value: None })
        );
    }

    // A Branch block with two arms: all three blocks should be cloned. The
    // cloned branch should keep the same cond ValueId (instructions are shared)
    // but point to the two new arm blocks.
    #[test]
    fn test_cfg_clone_branch() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        // Use a placeholder ValueId — instructions are shared across clones so
        // the cond value doesn't need to be real for this test.
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        fb.set_terminator(b, Terminator::Return { value: None });
        fb.set_terminator(c, Terminator::Return { value: None });

        let cloned_a = Cfg::new(&mut fb, a).clone(&mut fb);

        assert_ne!(a, cloned_a);
        match fb.get_block(cloned_a).terminator.clone() {
            Some(Terminator::Branch {
                cond: cloned_cond,
                true_target: cloned_b,
                false_target: cloned_c,
            }) => {
                assert_eq!(cond, cloned_cond);
                assert_ne!(b, cloned_b);
                assert_ne!(c, cloned_c);
                assert_eq!(
                    fb.get_block(cloned_b).terminator,
                    Some(Terminator::Return { value: None })
                );
                assert_eq!(
                    fb.get_block(cloned_c).terminator,
                    Some(Terminator::Return { value: None })
                );
            }
            term => panic!("expected Branch, got {:?}", term),
        }
    }

    // A SwitchVariant with a default block and one arm: all three blocks should
    // be cloned and the cloned switch should reference the new block IDs.
    #[test]
    fn test_cfg_clone_switch_variant() {
        let mut fb = test_builder();
        let entry = fb.add_block();
        let default = fb.add_block();
        let arm_block = fb.add_block();
        let discriminant = ValueId::from_usize(1);
        fb.set_terminator(
            entry,
            Terminator::SwitchVariant {
                discriminant,
                default,
                arms: vec![SwitchArm {
                    target: ConstValue::Int(0),
                    jump: arm_block,
                }],
            },
        );
        fb.set_terminator(default, Terminator::Return { value: None });
        fb.set_terminator(arm_block, Terminator::Return { value: None });

        let cloned_entry = Cfg::new(&mut fb, entry).clone(&mut fb);

        assert_ne!(entry, cloned_entry);
        match fb.get_block(cloned_entry).terminator.clone() {
            Some(Terminator::SwitchVariant {
                discriminant: cloned_disc,
                default: cloned_default,
                arms: cloned_arms,
            }) => {
                assert_eq!(discriminant, cloned_disc);
                assert_ne!(default, cloned_default);
                assert_eq!(cloned_arms.len(), 1);
                assert_ne!(arm_block, cloned_arms[0].jump);
                assert_eq!(cloned_arms[0].target, ConstValue::Int(0));
                assert_eq!(
                    fb.get_block(cloned_default).terminator,
                    Some(Terminator::Return { value: None })
                );
                assert_eq!(
                    fb.get_block(cloned_arms[0].jump).terminator,
                    Some(Terminator::Return { value: None })
                );
            }
            term => panic!("expected SwitchVariant, got {:?}", term),
        }
    }

    // A → B → A cycle: clone must terminate and the back-edge in the clone
    // should point to the cloned entry, not the original.
    #[test]
    fn test_cfg_clone_cycle() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(b, Terminator::Jump { target: a });

        let cloned_a = Cfg::new(&mut fb, a).clone(&mut fb);

        assert_ne!(a, cloned_a);
        let cloned_b = match fb.get_block(cloned_a).terminator.clone() {
            Some(Terminator::Jump { target }) => target,
            term => panic!("expected Jump from cloned_a, got {:?}", term),
        };
        assert_ne!(b, cloned_b);
        match fb.get_block(cloned_b).terminator.clone() {
            Some(Terminator::Jump { target }) => {
                assert_eq!(target, cloned_a);
            }
            term => panic!("expected Jump from cloned_b, got {:?}", term),
        }
    }

    // --- all_blocks_terminate ---

    #[test]
    fn test_all_blocks_terminate_single_block_with_terminator() {
        let mut fb = test_builder();
        let a = fb.add_block();
        fb.set_terminator(a, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        let did_terminate = cfg.all_blocks_terminate(&fb);
        assert!(did_terminate);
    }

    #[test]
    fn test_all_blocks_terminate_single_block_missing_terminator() {
        let mut fb = test_builder();
        let a = fb.add_block();
        // no terminator set

        let cfg = Cfg::new(&mut fb, a);
        let did_terminate = cfg.all_blocks_terminate(&fb);
        assert!(!did_terminate);
    }

    // Each of the terminal terminator kinds should count as terminated.
    #[test]
    fn test_all_blocks_terminate_terminal_kinds() {
        for term in [
            Terminator::Return { value: None },
            Terminator::Unreachable,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        ] {
            let mut fb = test_builder();
            let a = fb.add_block();
            fb.set_terminator(a, term);

            let cfg = Cfg::new(&mut fb, a);
            let did_terminate = cfg.all_blocks_terminate(&fb);
            assert!(did_terminate);
        }
    }

    #[test]
    fn test_all_blocks_terminate_linear_chain_all_terminated() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(b, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        let did_terminate = cfg.all_blocks_terminate(&fb);
        assert!(did_terminate);
    }

    #[test]
    fn test_all_blocks_terminate_linear_chain_missing_terminator() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        fb.set_terminator(a, Terminator::Jump { target: b });
        // b has no terminator

        let cfg = Cfg::new(&mut fb, a);
        let did_terminate = cfg.all_blocks_terminate(&fb);
        assert!(!did_terminate);
    }

    #[test]
    fn test_all_blocks_terminate_branch_all_terminated() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        fb.set_terminator(b, Terminator::Return { value: None });
        fb.set_terminator(c, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        let did_terminate = cfg.all_blocks_terminate(&fb);
        assert!(did_terminate);
    }

    #[test]
    fn test_all_blocks_terminate_branch_one_arm_missing() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        fb.set_terminator(b, Terminator::Return { value: None });
        // c has no terminator

        let cfg = Cfg::new(&mut fb, a);
        let did_terminate = cfg.all_blocks_terminate(&fb);
        assert!(!did_terminate);
    }

    #[test]
    fn test_all_blocks_terminate_switch_all_terminated() {
        let mut fb = test_builder();
        let entry = fb.add_block();
        let default = fb.add_block();
        let arm_block = fb.add_block();
        let discriminant = ValueId::from_usize(1);
        fb.set_terminator(
            entry,
            Terminator::SwitchVariant {
                discriminant,
                default,
                arms: vec![SwitchArm {
                    target: ConstValue::Int(0),
                    jump: arm_block,
                }],
            },
        );
        fb.set_terminator(default, Terminator::Return { value: None });
        fb.set_terminator(arm_block, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, entry);
        let did_terminate = cfg.all_blocks_terminate(&fb);
        assert!(did_terminate);
    }

    #[test]
    fn test_all_blocks_terminate_switch_arm_missing() {
        let mut fb = test_builder();
        let entry = fb.add_block();
        let default = fb.add_block();
        let arm_block = fb.add_block();
        let discriminant = ValueId::from_usize(1);
        fb.set_terminator(
            entry,
            Terminator::SwitchVariant {
                discriminant,
                default,
                arms: vec![SwitchArm {
                    target: ConstValue::Int(0),
                    jump: arm_block,
                }],
            },
        );
        fb.set_terminator(default, Terminator::Return { value: None });
        // arm_block has no terminator

        let cfg = Cfg::new(&mut fb, entry);
        let did_terminate = cfg.all_blocks_terminate(&fb);
        assert!(!did_terminate);
    }

    // A cycle where all blocks have terminators should return true — the
    // back-edge is short-circuited by the visited set.
    #[test]
    fn test_all_blocks_terminate_cycle_all_terminated() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(b, Terminator::Jump { target: a });

        let cfg = Cfg::new(&mut fb, a);
        let did_terminate = cfg.all_blocks_terminate(&fb);
        assert!(did_terminate);
    }

    // A cycle where one block is missing its terminator should still return
    // false. b branches back to a (the cycle) and also forward to c, and c has
    // no terminator. The visited set handles the back-edge cleanly but the
    // missing terminator on c must still be caught.
    #[test]
    fn test_all_blocks_terminate_cycle_missing_terminator() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let cond = ValueId::from_usize(1);
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(
            b,
            Terminator::Branch {
                cond,
                true_target: a,
                false_target: c,
            },
        );
        // c has no terminator

        let cfg = Cfg::new(&mut fb, a);
        let did_terminate = cfg.all_blocks_terminate(&fb);
        assert!(!did_terminate);
    }

    // --- can_return ---

    #[test]
    fn test_can_return_single_block_returns() {
        let mut fb = test_builder();
        let a = fb.add_block();
        fb.set_terminator(a, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        let can_return = cfg.can_return(&fb);
        assert!(can_return);
    }

    #[test]
    fn test_can_return_single_block_unreachable() {
        let mut fb = test_builder();
        let a = fb.add_block();
        fb.set_terminator(a, Terminator::Unreachable);

        let cfg = Cfg::new(&mut fb, a);
        let can_return = cfg.can_return(&fb);
        assert!(!can_return);
    }

    #[test]
    fn test_can_return_single_block_panic() {
        let mut fb = test_builder();
        let a = fb.add_block();
        fb.set_terminator(
            a,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );

        let cfg = Cfg::new(&mut fb, a);
        let can_return = cfg.can_return(&fb);
        assert!(!can_return);
    }

    #[test]
    fn test_can_return_single_block_no_terminator() {
        let mut fb = test_builder();
        let a = fb.add_block();
        // no terminator set

        let cfg = Cfg::new(&mut fb, a);
        let can_return = cfg.can_return(&fb);
        assert!(!can_return);
    }

    #[test]
    fn test_can_return_linear_chain_returns() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(b, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        let can_return = cfg.can_return(&fb);
        assert!(can_return);
    }

    #[test]
    fn test_can_return_linear_chain_no_return() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(b, Terminator::Unreachable);

        let cfg = Cfg::new(&mut fb, a);
        let can_return = cfg.can_return(&fb);
        assert!(!can_return);
    }

    // Only one arm needs to return for can_return to be true.
    #[test]
    fn test_can_return_branch_one_arm_returns() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        fb.set_terminator(b, Terminator::Return { value: None });
        fb.set_terminator(c, Terminator::Unreachable);

        let cfg = Cfg::new(&mut fb, a);
        let can_return = cfg.can_return(&fb);
        assert!(can_return);
    }

    #[test]
    fn test_can_return_branch_no_arm_returns() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        fb.set_terminator(b, Terminator::Unreachable);
        fb.set_terminator(
            c,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );

        let cfg = Cfg::new(&mut fb, a);
        let can_return = cfg.can_return(&fb);
        assert!(!can_return);
    }

    #[test]
    fn test_can_return_switch_one_arm_returns() {
        let mut fb = test_builder();
        let entry = fb.add_block();
        let default = fb.add_block();
        let arm_block = fb.add_block();
        let discriminant = ValueId::from_usize(1);
        fb.set_terminator(
            entry,
            Terminator::SwitchVariant {
                discriminant,
                default,
                arms: vec![SwitchArm {
                    target: ConstValue::Int(0),
                    jump: arm_block,
                }],
            },
        );
        fb.set_terminator(default, Terminator::Unreachable);
        fb.set_terminator(arm_block, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, entry);
        let can_return = cfg.can_return(&fb);
        assert!(can_return);
    }

    #[test]
    fn test_can_return_switch_no_arm_returns() {
        let mut fb = test_builder();
        let entry = fb.add_block();
        let default = fb.add_block();
        let arm_block = fb.add_block();
        let discriminant = ValueId::from_usize(1);
        fb.set_terminator(
            entry,
            Terminator::SwitchVariant {
                discriminant,
                default,
                arms: vec![SwitchArm {
                    target: ConstValue::Int(0),
                    jump: arm_block,
                }],
            },
        );
        fb.set_terminator(default, Terminator::Unreachable);
        fb.set_terminator(
            arm_block,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );

        let cfg = Cfg::new(&mut fb, entry);
        let can_return = cfg.can_return(&fb);
        assert!(!can_return);
    }

    // A cycle where one path through the loop eventually returns.
    // b loops back to a and also branches to c which returns.
    #[test]
    fn test_can_return_cycle_with_return() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let cond = ValueId::from_usize(1);
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(
            b,
            Terminator::Branch {
                cond,
                true_target: a,
                false_target: c,
            },
        );
        fb.set_terminator(c, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        let can_return = cfg.can_return(&fb);
        assert!(can_return);
    }

    // A cycle with no return anywhere — the back-edge is short-circuited by
    // the visited set and the remaining paths all end in non-return terminators.
    #[test]
    fn test_can_return_cycle_without_return() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let cond = ValueId::from_usize(1);
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(
            b,
            Terminator::Branch {
                cond,
                true_target: a,
                false_target: c,
            },
        );
        fb.set_terminator(c, Terminator::Unreachable);

        let cfg = Cfg::new(&mut fb, a);
        let can_return = cfg.can_return(&fb);
        assert!(!can_return);
    }

    // --- jump_to ---

    // A block with no terminator should have it set to jump to the target.
    #[test]
    fn test_jump_to_single_block_no_terminator() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        cfg.jump_to(&mut fb, target);

        let a_terminator = fb.get_block(a).terminator.clone();
        assert_eq!(a_terminator, Some(Terminator::Jump { target }));
    }

    // Blocks that already have a terminal terminator should be left unchanged.
    #[test]
    fn test_jump_to_leaves_existing_terminators_alone() {
        for term in [
            Terminator::Return { value: None },
            Terminator::Unreachable,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        ] {
            let mut fb = test_builder();
            let a = fb.add_block();
            let target = fb.add_block();
            fb.set_terminator(target, Terminator::Return { value: None });
            fb.set_terminator(a, term.clone());

            let cfg = Cfg::new(&mut fb, a);
            cfg.jump_to(&mut fb, target);

            let a_terminator = fb.get_block(a).terminator.clone();
            assert_eq!(a_terminator, Some(term));
        }
    }

    // In a linear chain where the tail has no terminator, only the tail should
    // be wired up.
    #[test]
    fn test_jump_to_linear_chain_wires_tail() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Return { value: None });
        fb.set_terminator(a, Terminator::Jump { target: b });
        // b has no terminator

        let cfg = Cfg::new(&mut fb, a);
        cfg.jump_to(&mut fb, target);

        let a_terminator = fb.get_block(a).terminator.clone();
        let b_terminator = fb.get_block(b).terminator.clone();
        assert_eq!(a_terminator, Some(Terminator::Jump { target: b }));
        assert_eq!(b_terminator, Some(Terminator::Jump { target }));
    }

    // Both arms of a branch with no terminators should be wired up.
    #[test]
    fn test_jump_to_branch_wires_both_arms() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Return { value: None });
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        // b and c have no terminators

        let cfg = Cfg::new(&mut fb, a);
        cfg.jump_to(&mut fb, target);

        let b_terminator = fb.get_block(b).terminator.clone();
        let c_terminator = fb.get_block(c).terminator.clone();
        assert_eq!(b_terminator, Some(Terminator::Jump { target }));
        assert_eq!(c_terminator, Some(Terminator::Jump { target }));
    }

    // Only the arm without a terminator should be wired up; the other is left alone.
    #[test]
    fn test_jump_to_branch_wires_only_missing_arm() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Return { value: None });
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        fb.set_terminator(b, Terminator::Return { value: None });
        // c has no terminator

        let cfg = Cfg::new(&mut fb, a);
        cfg.jump_to(&mut fb, target);

        let b_terminator = fb.get_block(b).terminator.clone();
        let c_terminator = fb.get_block(c).terminator.clone();
        assert_eq!(b_terminator, Some(Terminator::Return { value: None }));
        assert_eq!(c_terminator, Some(Terminator::Jump { target }));
    }

    // In a cycle, the back-edge is short-circuited by the visited set. The
    // off-loop block with no terminator should still be wired up correctly.
    #[test]
    fn test_jump_to_cycle_wires_off_loop_block() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Return { value: None });
        let cond = ValueId::from_usize(1);
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(
            b,
            Terminator::Branch {
                cond,
                true_target: a,
                false_target: c,
            },
        );
        // c has no terminator

        let cfg = Cfg::new(&mut fb, a);
        cfg.jump_to(&mut fb, target);

        let a_terminator = fb.get_block(a).terminator.clone();
        let b_terminator = fb.get_block(b).terminator.clone();
        let c_terminator = fb.get_block(c).terminator.clone();
        assert_eq!(a_terminator, Some(Terminator::Jump { target: b }));
        assert_eq!(
            b_terminator,
            Some(Terminator::Branch {
                cond,
                true_target: a,
                false_target: c
            })
        );
        assert_eq!(c_terminator, Some(Terminator::Jump { target }));
    }

    // --- panic_to ---

    // A block with a Panic terminator should have it replaced with a Jump to the target.
    #[test]
    fn test_panic_to_single_block_panics() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Return { value: None });
        fb.set_terminator(
            a,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );

        let cfg = Cfg::new(&mut fb, a);
        cfg.panic_to(&mut fb, target);

        let a_terminator = fb.get_block(a).terminator.clone();
        assert_eq!(a_terminator, Some(Terminator::Jump { target }));
    }

    // Non-Panic terminators should be left unchanged.
    #[test]
    fn test_panic_to_leaves_existing_terminators_alone() {
        for term in [Terminator::Return { value: None }, Terminator::Unreachable] {
            let mut fb = test_builder();
            let a = fb.add_block();
            let target = fb.add_block();
            fb.set_terminator(target, Terminator::Return { value: None });
            fb.set_terminator(a, term.clone());

            let cfg = Cfg::new(&mut fb, a);
            cfg.panic_to(&mut fb, target);

            let a_terminator = fb.get_block(a).terminator.clone();
            assert_eq!(a_terminator, Some(term));
        }
    }

    // In a linear chain where the tail panics, only the tail should be replaced.
    #[test]
    fn test_panic_to_linear_chain_wires_tail() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Return { value: None });
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(
            b,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );

        let cfg = Cfg::new(&mut fb, a);
        cfg.panic_to(&mut fb, target);

        let a_terminator = fb.get_block(a).terminator.clone();
        let b_terminator = fb.get_block(b).terminator.clone();
        assert_eq!(a_terminator, Some(Terminator::Jump { target: b }));
        assert_eq!(b_terminator, Some(Terminator::Jump { target }));
    }

    // Both arms of a branch that panic should be replaced.
    #[test]
    fn test_panic_to_branch_wires_both_arms() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Return { value: None });
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        fb.set_terminator(
            b,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );
        fb.set_terminator(
            c,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );

        let cfg = Cfg::new(&mut fb, a);
        cfg.panic_to(&mut fb, target);

        let b_terminator = fb.get_block(b).terminator.clone();
        let c_terminator = fb.get_block(c).terminator.clone();
        assert_eq!(b_terminator, Some(Terminator::Jump { target }));
        assert_eq!(c_terminator, Some(Terminator::Jump { target }));
    }

    // Only the arm that panics should be replaced; the other is left alone.
    #[test]
    fn test_panic_to_branch_wires_only_panicking_arm() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Return { value: None });
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        fb.set_terminator(b, Terminator::Return { value: None });
        fb.set_terminator(
            c,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );

        let cfg = Cfg::new(&mut fb, a);
        cfg.panic_to(&mut fb, target);

        let b_terminator = fb.get_block(b).terminator.clone();
        let c_terminator = fb.get_block(c).terminator.clone();
        assert_eq!(b_terminator, Some(Terminator::Return { value: None }));
        assert_eq!(c_terminator, Some(Terminator::Jump { target }));
    }

    // In a cycle, the back-edge is short-circuited and the off-loop block that
    // panics is still replaced correctly.
    #[test]
    fn test_panic_to_cycle_wires_off_loop_block() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Return { value: None });
        let cond = ValueId::from_usize(1);
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(
            b,
            Terminator::Branch {
                cond,
                true_target: a,
                false_target: c,
            },
        );
        fb.set_terminator(
            c,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );

        let cfg = Cfg::new(&mut fb, a);
        cfg.panic_to(&mut fb, target);

        let a_terminator = fb.get_block(a).terminator.clone();
        let b_terminator = fb.get_block(b).terminator.clone();
        let c_terminator = fb.get_block(c).terminator.clone();
        assert_eq!(a_terminator, Some(Terminator::Jump { target: b }));
        assert_eq!(
            b_terminator,
            Some(Terminator::Branch {
                cond,
                true_target: a,
                false_target: c
            })
        );
        assert_eq!(c_terminator, Some(Terminator::Jump { target }));
    }

    // --- return_to ---

    // A unit return (no value) should be replaced with a Jump and no store emitted.
    #[test]
    fn test_return_to_unit_return_single_block() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Unreachable);
        fb.set_terminator(a, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        cfg.return_to(&mut fb, target, None);

        let a_terminator = fb.get_block(a).terminator.clone();
        let a_instructions = fb.get_block(a).instructions.clone();
        assert_eq!(a_terminator, Some(Terminator::Jump { target }));
        assert!(a_instructions.is_empty());
    }

    // A value return should be replaced with a store to the local followed by a Jump.
    #[test]
    fn test_return_to_value_return_single_block() {
        let mut fb = FunctionBuilder::new_private(StrID::from_usize(1), TypeSpec::I32);
        let a = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Unreachable);
        let return_val = fb.emit_const(a, TypeSpec::I32, ConstValue::Int(42));
        let local = fb.add_local(StrID::from_usize(2), TypeSpec::I32);
        fb.set_terminator(
            a,
            Terminator::Return {
                value: Some(return_val),
            },
        );

        let cfg = Cfg::new(&mut fb, a);
        cfg.return_to(&mut fb, target, Some(local));

        let a_terminator = fb.get_block(a).terminator.clone();
        assert_eq!(a_terminator, Some(Terminator::Jump { target }));

        let store_value_id = *fb.get_block(a).instructions.last().unwrap();
        let store_instruction = fb.instructions[store_value_id.as_idx()].clone();
        assert_eq!(
            store_instruction,
            Instruction::Store {
                place: Place::local(local),
                value: return_val
            }
        );
    }

    // Non-Return terminators should be left unchanged.
    #[test]
    fn test_return_to_leaves_non_return_terminators_alone() {
        for term in [
            Terminator::Panic {
                msg: ValueId::nil(),
            },
            Terminator::Unreachable,
        ] {
            let mut fb = test_builder();
            let a = fb.add_block();
            let target = fb.add_block();
            fb.set_terminator(target, Terminator::Unreachable);
            fb.set_terminator(a, term.clone());

            let cfg = Cfg::new(&mut fb, a);
            cfg.return_to(&mut fb, target, None);

            let a_terminator = fb.get_block(a).terminator.clone();
            assert_eq!(a_terminator, Some(term));
        }
    }

    // In a linear chain where the tail returns, only the tail should be replaced.
    #[test]
    fn test_return_to_linear_chain_wires_tail() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Unreachable);
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(b, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        cfg.return_to(&mut fb, target, None);

        let a_terminator = fb.get_block(a).terminator.clone();
        let b_terminator = fb.get_block(b).terminator.clone();
        assert_eq!(a_terminator, Some(Terminator::Jump { target: b }));
        assert_eq!(b_terminator, Some(Terminator::Jump { target }));
    }

    // Both arms of a branch that return should be replaced.
    #[test]
    fn test_return_to_branch_wires_both_arms() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Unreachable);
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        fb.set_terminator(b, Terminator::Return { value: None });
        fb.set_terminator(c, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        cfg.return_to(&mut fb, target, None);

        let b_terminator = fb.get_block(b).terminator.clone();
        let c_terminator = fb.get_block(c).terminator.clone();
        assert_eq!(b_terminator, Some(Terminator::Jump { target }));
        assert_eq!(c_terminator, Some(Terminator::Jump { target }));
    }

    // Only the arm that returns should be replaced; the other is left alone.
    #[test]
    fn test_return_to_branch_wires_only_returning_arm() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Unreachable);
        let cond = ValueId::from_usize(1);
        fb.set_terminator(
            a,
            Terminator::Branch {
                cond,
                true_target: b,
                false_target: c,
            },
        );
        fb.set_terminator(
            b,
            Terminator::Panic {
                msg: ValueId::nil(),
            },
        );
        fb.set_terminator(c, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        cfg.return_to(&mut fb, target, None);

        let b_terminator = fb.get_block(b).terminator.clone();
        let c_terminator = fb.get_block(c).terminator.clone();
        assert_eq!(
            b_terminator,
            Some(Terminator::Panic {
                msg: ValueId::nil()
            })
        );
        assert_eq!(c_terminator, Some(Terminator::Jump { target }));
    }

    // In a cycle, the back-edge is short-circuited and the off-loop block that
    // returns is still replaced correctly.
    #[test]
    fn test_return_to_cycle_wires_off_loop_block() {
        let mut fb = test_builder();
        let a = fb.add_block();
        let b = fb.add_block();
        let c = fb.add_block();
        let target = fb.add_block();
        fb.set_terminator(target, Terminator::Unreachable);
        let cond = ValueId::from_usize(1);
        fb.set_terminator(a, Terminator::Jump { target: b });
        fb.set_terminator(
            b,
            Terminator::Branch {
                cond,
                true_target: a,
                false_target: c,
            },
        );
        fb.set_terminator(c, Terminator::Return { value: None });

        let cfg = Cfg::new(&mut fb, a);
        cfg.return_to(&mut fb, target, None);

        let a_terminator = fb.get_block(a).terminator.clone();
        let b_terminator = fb.get_block(b).terminator.clone();
        let c_terminator = fb.get_block(c).terminator.clone();
        assert_eq!(a_terminator, Some(Terminator::Jump { target: b }));
        assert_eq!(
            b_terminator,
            Some(Terminator::Branch {
                cond,
                true_target: a,
                false_target: c
            })
        );
        assert_eq!(c_terminator, Some(Terminator::Jump { target }));
    }
}

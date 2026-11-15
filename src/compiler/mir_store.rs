use crate::hir::hir::FuncDecl;
use crate::index_vec::IndexVec;
use crate::mir::mir::{
    Block, BlockValue, Func, FuncValue, Inst, Local, LocalValue, Terminator, TerminatorValue, Value,
};
use crate::types::checked_ty::CheckedTy;

pub struct MirStore {
    insts: IndexVec<Value, Inst>,
    inst_ty: IndexVec<Value, CheckedTy>,

    locals: IndexVec<Local, LocalValue>,
    local_ty: IndexVec<Local, CheckedTy>,

    funcs: IndexVec<Func, FuncValue>,
    blocks: IndexVec<Block, BlockValue>,
    terminators: IndexVec<Terminator, TerminatorValue>,
}

impl MirStore {
    pub fn new() -> Self {
        // the first terminator servers as a none value
        let mut terminators = IndexVec::new();
        terminators.push(TerminatorValue::None);

        Self {
            insts: IndexVec::new(),
            inst_ty: IndexVec::new(),
            locals: IndexVec::new(),
            local_ty: IndexVec::new(),
            funcs: IndexVec::new(),
            blocks: IndexVec::new(),
            terminators,
        }
    }

    pub fn create_func(&mut self, decl: &FuncDecl) -> Func {
        let func_value = FuncValue::new(self, decl);
        let idx = self.funcs.len();
        self.funcs.push(func_value);

        Func::from(idx)
    }

    pub fn create_block(&mut self, func: Func) -> Block {
        let idx = self.blocks.len();
        let block_value = BlockValue::new(self);
        self.blocks.push(block_value);

        let block = Block::from(idx);

        let func_value = self.funcs.get_mut(func).expect("failed to find func value");
        func_value.push_block(block);

        block
    }

    pub fn create_none_terminator(&self) -> Terminator {
        // the first terminator is always a TerminatorValue::None
        Terminator::from(0)
    }

    pub fn create_local(&self) -> Local {
        let idx = self.locals.len();
        let local_value = LocalValue::new();
    }
}

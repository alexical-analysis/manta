use std::collections::BTreeMap;

use crate::ast::{BinaryOp, TypeSpec, UnaryOp};
use crate::str_store::StrID;
use serde::Serialize;

/// A unique identifier for a value (temporary or SSA-like result from an instruction).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct ValueId(u32);

impl ValueId {
    /// Creates a ValueId from a raw u32. Panics if value is 0 (reserved as nil).
    pub fn from_u32(id: u32) -> Self {
        assert_ne!(id, 0, "ValueId(0) is reserved as nil");
        ValueId(id)
    }

    /// Returns the raw u32 value.
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Returns the nil ValueId (0).
    pub fn nil() -> Self {
        ValueId(0)
    }

    /// Checks if this is the nil ValueId.
    pub fn is_nil(self) -> bool {
        self.0 == 0
    }
}

/// A unique identifier for a block in the control-flow graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct BlockId(u32);

impl BlockId {
    /// Creates a BlockId from a raw u32. Panics if id is 0 (reserved as nil).
    pub fn from_u32(id: u32) -> Self {
        assert_ne!(id, 0, "BlockId(0) is reserved as nil");
        BlockId(id)
    }

    /// Returns the raw u32 value.
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Return the id as an index instead of an ID
    pub fn as_idx(self) -> usize {
        (self.0 - 1) as usize
    }

    /// Returns the nil BlockId (0).
    pub fn nil() -> Self {
        BlockId(0)
    }

    /// Checks if this is the nil BlockId.
    pub fn is_nil(self) -> bool {
        self.0 == 0
    }
}

/// A unique identifier for a local variable (storage slot).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct LocalId(u32);

impl LocalId {
    /// Creates a LocalId from a raw u32. Panics if id is 0 (reserved as nil).
    pub fn from_u32(id: u32) -> Self {
        assert_ne!(id, 0, "LocalId(0) is reserved as nil");
        LocalId(id)
    }

    /// Returns the raw u32 value.
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Return the id as an index instead of an ID
    pub fn as_idx(self) -> usize {
        (self.0 - 1) as usize
    }

    /// Returns the nil LocalId (0).
    pub fn nil() -> Self {
        LocalId(0)
    }

    /// Checks if this is the nil LocalId.
    pub fn is_nil(self) -> bool {
        self.0 == 0
    }
}

/// Metadata about a local variable.
#[derive(Debug, Clone, Serialize)]
pub struct Local {
    pub id: LocalId,
    pub type_spec: TypeSpec,
    pub name: StrID,
}

/// ConstValue is a constant value that can be compiled directly into the final binary
/// I'm not 100% sure we'll actually want this since we could just swap the constant values in
/// directly to the MIR but I've added them now just in case
#[derive(Debug, Clone, Serialize)]
pub enum ConstValue {
    ConstString(StrID),
    ConstInt(u64),
    ConstFloat(f64),
    ConstBool(bool),
    ConstArray(Vec<ConstValue>),
    ConstStruct(Vec<ConstValue>),
}

/// An instruction that produces a value or modifies state.
#[derive(Debug, Clone, Serialize)]
pub enum Instruction {
    /// const ty, value -> ValueId
    Const {
        type_spec: TypeSpec,
        value: ConstValue,
    },
    /// unary op, value -> ValueId
    UnaryOp { op: UnaryOp, operand: ValueId },
    /// binary op, lhs, rhs -> ValueId
    BinaryOp {
        op: BinaryOp,
        lhs: ValueId,
        rhs: ValueId,
    },
    /// load_local(LocalId) -> ValueId
    LoadLocal { local: LocalId },
    /// store_local(LocalId, ValueId)
    StoreLocal { local: LocalId, value: ValueId },
    /// call(func, args...) -> ValueId
    Call {
        func: StrID, // Function name or identifier
        args: Vec<ValueId>,
    },
    /// call_try(func, args..., handler: BlockId) -> ValueId
    CallTry {
        func: StrID,
        args: Vec<ValueId>,
        handler: BlockId,
    },
    /// variant_get_payload(src: ValueId, variant_id) -> ValueId
    /// variants can only ever contain a single value in Manta so there's no need to extract a
    /// specific field
    VariantGetPayload { src: ValueId, variant_id: u32 },
    /// move(dst_local, src_value)
    Move { dst: LocalId, src: ValueId },
    /// copy(dst_local, src_value)
    Copy { dst: LocalId, src: ValueId },
    /// drop_local(LocalId)
    DropLocal { local: LocalId },
    /// declare_local(LocalId) - declares a local as uninitialized (mostly for clarity)
    DeclareLocal { local: LocalId },
    /// set_initialized(LocalId) - marks a local as initialized
    SetInitialized { local: LocalId },
}

/// A terminator instruction that ends a basic block and directs control flow.
#[derive(Debug, Clone, Serialize)]
pub enum Terminator {
    /// Return(value?)
    Return { value: Option<ValueId> },
    /// Jump(target)
    Jump { target: BlockId },
    /// Branch(cond_value, true_target, false_target)
    Branch {
        cond: ValueId,
        true_target: BlockId,
        false_target: BlockId,
    },
    /// SwitchVariant(value, {(variant_id, target), ...})
    SwitchVariant {
        value: ValueId,
        arms: Vec<(u32, BlockId)>, // (variant_id, target_block)
    },
    /// Unreachable (for proven-unreachable code)
    Unreachable,
}

/// A basic block in the control-flow graph.
#[derive(Debug, Clone, Serialize)]
pub struct BasicBlock {
    pub block_args: Vec<ValueId>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

impl BasicBlock {
    pub fn new(
        block_args: Vec<ValueId>,
        instructions: Vec<Instruction>,
        terminator: Terminator,
    ) -> Self {
        BasicBlock {
            block_args,
            instructions,
            terminator,
        }
    }
}

/// A function in MIR form.
#[derive(Debug, Clone, Serialize)]
pub struct MirFunction {
    pub name: StrID,
    pub params: Vec<StrID>,
    pub type_spec: TypeSpec,
    pub locals: Vec<Local>,      // Indexed by LocalId
    pub blocks: Vec<BasicBlock>, // Indexed by BlockId
    // TODO: do I even need this? Right now the BasicBlocks all live inside the function. Does it
    // make sense to have all the basic blocks live together than then only have the functions
    // index into that larger store? My current implementation always set's this to 1
    pub entry_block: BlockId,
    pub value_types: Vec<TypeSpec>,
}

impl MirFunction {
    pub fn new(name: StrID, params: Vec<StrID>, type_spec: TypeSpec, entry_block: BlockId) -> Self {
        MirFunction {
            name,
            params,
            type_spec,
            locals: vec![],
            blocks: vec![],
            entry_block,
            value_types: vec![],
        }
    }

    /// Gets a local by LocalId (0-indexed into the locals Vec, but LocalIds are 1-indexed).
    pub fn get_local(&self, id: LocalId) -> Option<&Local> {
        if id.is_nil() {
            return None;
        }
        self.locals.get(id.as_idx())
    }

    /// Gets a block by BlockId.
    pub fn get_block(&self, id: BlockId) -> Option<&BasicBlock> {
        if id.is_nil() {
            return None;
        }
        self.blocks.get((id.as_u32() - 1) as usize)
    }
}

/// A collection of MIR functions (represents the entire program at the MIR level).
#[derive(Debug, Clone, Serialize)]
pub struct MirModule {
    pub init: MirFunction,
    pub functions: Vec<MirFunction>,
}

impl MirModule {
    pub fn new(init: MirFunction, functions: Vec<MirFunction>) -> Self {
        MirModule { init, functions }
    }
}

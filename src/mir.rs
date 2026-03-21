use crate::ast::{BinaryOp, UnaryOp};
use crate::str_store::StrID;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TagSize {
    U8,
    U16,
    U32,
    U64,
}

/// MIR-level type: all variants are concrete and map directly to LLVM types.
/// No inference artifacts or HIR back-references.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeSpec {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Unit,

    /// Typed pointer: *T
    Ptr(Box<TypeSpec>),

    /// Opaque pointer for alloc/free (LLVM `ptr` with no inner type)
    OpaquePtr,

    /// Fixed-size array: [N x T]
    Array {
        elem: Box<TypeSpec>,
        len: usize,
    },

    /// Immutable string fat pointer: { ptr: *u8, len: usize }
    String,

    /// Growable slice fat pointer: { ptr: *T, len: usize, cap: usize }
    Slice(Box<TypeSpec>),

    /// Struct with positional fields (no names needed at this level).
    /// The blocker maintains a name→index mapping for field access.
    Struct(Vec<TypeSpec>),

    /// Layout for an enum type. Variants are indexed by variant_id; `None` means a unit variant
    /// (no payload). The discriminant size defaults to a u8 but may be larger in cases where there
    /// are more than 256 variants
    Enum {
        tag_size: TagSize,
        variants: Vec<Option<TypeSpec>>,
    },
}

/// A unique identifier for a value (temporary or SSA-like result from an instruction).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct ValueId(u32);

impl ValueId {
    /// Creates a ValueId from a raw u32. Panics if value is 0 (reserved as nil).
    pub fn from_u32(id: u32) -> Self {
        assert_ne!(id, 0, "ValueId(0) is reserved as nil");
        ValueId(id)
    }

    /// Creates a ValueId from a raw usize
    pub fn from_usize(id: usize) -> Self {
        ValueId(id as u32)
    }

    /// Returns the ValueId as an index
    pub fn as_idx(self) -> usize {
        assert_ne!(self.0, 0, "ValueId(0) is reserved as nil");
        (self.0 - 1) as usize
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
    pub type_spec: TypeSpec,
    pub name: StrID,
}

/// ConstValue is a constant value that can be compiled directly into the final binary
/// I'm not 100% sure we'll actually want this since we could just swap the constant values in
/// directly to the MIR but I've added them now just in case
#[derive(Debug, Clone, Serialize)]
pub enum ConstValue {
    ConstString(StrID),
    ConstInt(i64),
    ConstUInt(u64),
    ConstFloat(f64),
    ConstBool(bool),
    ConstArray(Vec<ConstValue>),
    ConstStruct(Vec<ConstValue>),
}

/// An instruction that produces a value or modifies state.
/// The result ValueId of each instruction is its position in the function's flat instruction
/// array (1-indexed). Instructions that don't produce a meaningful value are assigned TypeSpec::Unit
/// in the parallel value_types vec so instructions and value_types stay aligned.
#[derive(Debug, Clone, Serialize)]
pub enum Instruction {
    /// const ty, value -> ValueId (implicit: instruction's own id)
    Const { value: ConstValue },
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
    /// store_local(LocalId, ValueId) — produces Unit
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
    VariantGetPayload {
        src: ValueId,
        variant_id: ConstValue,
    },
    /// variant_get_tag(src: ValueId) -> ValueId
    /// Extracts the discriminant tag from an enum value as an integer.
    VariantGetTag { src: ValueId },

    /// move(dst_local, src_value) — produces Unit
    Move { dst: LocalId, src: ValueId },
    /// copy(dst_local, src_value) — produces Unit
    Copy { dst: LocalId, src: ValueId },
    /// drop_local(LocalId) — produces Unit
    DropLocal { local: LocalId },
    /// declare_local(LocalId) — produces Unit (declares a local as uninitialized)
    DeclareLocal { local: LocalId },
    /// set_initialized(LocalId) — produces Unit
    SetInitialized { local: LocalId },
}

#[derive(Debug, Clone, Serialize)]
pub struct SwitchArm {
    // ConstValue should always be a const int since that what will correctly map to LLVM IR
    pub target: ConstValue,
    pub jump: BlockId,
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
        discriminant: ValueId,
        default: BlockId,
        arms: Vec<SwitchArm>,
    },
    /// Unreachable (for proven-unreachable code)
    Unreachable,
}

/// A basic block in the control-flow graph.
/// `instructions` holds ValueIds that index into the owning MirFunction's flat instruction array.
#[derive(Debug, Clone, Serialize)]
pub struct BasicBlock {
    pub block_args: Vec<ValueId>,
    pub instructions: Vec<ValueId>,
    pub terminator: Terminator,
}

impl BasicBlock {
    pub fn new(
        block_args: Vec<ValueId>,
        instructions: Vec<ValueId>,
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
    /// Flat instruction array owned by the function. Indexed by ValueId (1-based).
    pub instructions: Vec<Instruction>,
    /// Result type for each instruction, parallel to `instructions`. Instructions that don't
    /// produce a meaningful value use TypeSpec::Unit.
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
            instructions: vec![],
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

    /// Gets an instruction by ValueId.
    pub fn get_instruction(&self, id: ValueId) -> Option<&Instruction> {
        if id.is_nil() {
            return None;
        }
        self.instructions.get(id.as_u32() as usize - 1)
    }

    /// Gets the result type for a ValueId.
    pub fn get_value_type(&self, id: ValueId) -> Option<&TypeSpec> {
        if id.is_nil() {
            return None;
        }
        self.value_types.get(id.as_u32() as usize - 1)
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

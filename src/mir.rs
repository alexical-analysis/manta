use std::collections::BTreeMap;

use crate::ast::{BinaryOp, UnaryOp};
use crate::hir::NodeID;
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

/// A unique identifier for a global variable (storage slot).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct GlobalId(u32);

impl GlobalId {
    /// Creates a GlobalId from a raw u32. Panics if id is 0 (reserved as nil).
    pub fn from_u32(id: u32) -> Self {
        assert_ne!(id, 0, "GlobalId(0) is reserved as nil");
        GlobalId(id)
    }

    pub fn from_usize(id: usize) -> Self {
        assert_ne!(id, 0, "GlobalId(0) is reserved as nil");
        GlobalId(id as u32)
    }

    /// Returns the raw u32 value.
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Return the id as an index instead of an ID
    pub fn as_idx(self) -> usize {
        (self.0 - 1) as usize
    }

    /// Returns the nil GlobalId (0).
    pub fn nil() -> Self {
        GlobalId(0)
    }

    /// Checks if this is the nil GlobalId.
    pub fn is_nil(self) -> bool {
        self.0 == 0
    }
}

/// Metadata about a global variable.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Global {
    pub type_spec: TypeSpec,
    pub name: StrID,
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

    pub fn from_usize(id: usize) -> Self {
        assert_ne!(id, 0, "LocalId(0) is reserved as nil");
        LocalId(id as u32)
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
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Local {
    pub type_spec: TypeSpec,
    pub name: StrID,
}

/// ConstValue is a constant value that can be compiled directly into the final binary
/// I'm not 100% sure we'll actually want this since we could just swap the constant values in
/// directly to the MIR but I've added them now just in case
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ConstValue {
    ConstString(StrID),
    ConstInt(i64),
    ConstUInt(u64),
    ConstFloat(f64),
    ConstBool(bool),
    ConstArray(Vec<ConstValue>),
    ConstStruct(Vec<ConstValue>),
}

/// A place identifies a storage location — a base (local or global) plus a chain of projections
/// that navigate into it. Used as the target of reads, writes, and address-of operations.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Place {
    pub base: PlaceBase,
    pub projections: Vec<Projection>,
}

impl Place {
    pub fn local(local: LocalId) -> Self {
        Place {
            base: PlaceBase::Local(local),
            projections: vec![],
        }
    }

    pub fn global(global: GlobalId) -> Self {
        Place {
            base: PlaceBase::Global(global),
            projections: vec![],
        }
    }
}

/// The root storage of a place — either a local variable or a global.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum PlaceBase {
    Local(LocalId),
    Global(GlobalId),
}

/// A single step navigating into a place.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Projection {
    /// Dereference a pointer: `*p`
    Deref,
    /// Access a struct field by index: `s.field`
    Field(usize),
    /// Index into an array or slice: `arr[i]`
    Index(ValueId),
}

/// An instruction that produces a value or modifies state.
/// The result ValueId of each instruction is its position in the function's flat instruction
/// array (1-indexed). Instructions that don't produce a meaningful value are assigned TypeSpec::Unit
/// in the parallel value_types vec so instructions and value_types stay aligned.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Instruction {
    /// const ty, value -> ValueId (implicit: instruction's own id)
    Const {
        value: ConstValue,
    },
    /// load_local(LocalId) -> ValueId
    LoadLocal {
        local: LocalId,
    },
    /// store_local(LocalId, ValueId) — produces Unit
    StoreLocal {
        local: LocalId,
        value: ValueId,
    },
    /// load_global(globalId) -> ValueId
    LoadGlobal {
        global: GlobalId,
    },
    /// store_global(GlobalId, ValueId) — produces Unit
    StoreGlobal {
        global: GlobalId,
        value: ValueId,
    },
    /// load a value from a pointer into a value
    LoadPtr {
        ptr: ValueId,
    },
    // Store a value into a pointers memory address
    StorePtr {
        ptr: ValueId,
        value: ValueId,
    },
    /// LocalAddr gets the address of a local
    LocalAddr {
        local: LocalId,
    },
    /// GlobalAddr gets the address of a global
    GlobalAddr {
        global: GlobalId,
    },
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
    VariantGetTag {
        src: ValueId,
    },

    /// move(dst_local, src_value) — produces Unit
    Move {
        dst: LocalId,
        src: ValueId,
    },
    /// copy(dst_local, src_value) — produces Unit
    Copy {
        dst: LocalId,
        src: ValueId,
    },
    /// drop_local(LocalId) — produces Unit
    DropLocal {
        local: LocalId,
    },
    /// declare_local(LocalId) — produces Unit (declares a local as uninitialized)
    DeclareLocal {
        local: LocalId,
    },
    /// set_initialized(LocalId) — produces Unit
    SetInitialized {
        local: LocalId,
    },
    /// add two expressions - produces the same type as its args
    Add {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// subtracts two expressions - produces the same type as its args
    Sub {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// multiplies two expressions - produces the same type as its args
    Mul {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// does signed division of two expressions - produces the same type as its args
    SDiv {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// does unsigned division of two expressions - produces the same type as its args
    UDiv {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// does signed modulus of two expressions - produces the same type as its args
    SMod {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// does unsigned modulus of two expressions - produces the same type as its args
    UMod {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if two expressions are equal - produces a boolean
    Equal {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if two expressions are not equal - produces a boolean
    NotEqual {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if one expressions is less than the other - produces a boolean
    SLessThan {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if one expressions is less than the other - produces a boolean
    ULessThan {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if one expressions is less than or equal to the other - produces a boolean
    SLessThanEqual {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if one expressions is less than or equal to the other - produces a boolean
    ULessThanEqual {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if one expressions is greater than the other - produces a boolean
    SGreaterThan {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if one expressions is greater than the other - produces a boolean
    UGreaterThan {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if one expressions is greater than or equal to the other - produces a boolean
    SGreaterThanEqual {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if one expressions is greater than or equal to the other - produces a boolean
    UGreaterThanEqual {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if at least on of two expressions is true - produces a boolean
    LogicalOr {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// checks if two expressions are both true - produces a boolean
    LogicalAnd {
        lhs: ValueId,
        rhs: ValueId,
    },
    // does a bitwise AND between the two expressions - produces the same type as it's args
    BitwiseAnd {
        lhs: ValueId,
        rhs: ValueId,
    },
    // does a bitwise OR between the two expressions - produces the same type as it's args
    BitwiseOr {
        lhs: ValueId,
        rhs: ValueId,
    },
    // does a bitwise XOR between the two expressions - produces the same type as it's args
    BitwiseXOr {
        lhs: ValueId,
        rhs: ValueId,
    },
    /// inverts the boolean value - produces a boolean value
    BoolNot {
        op: ValueId,
    },
    /// negates the given expression - produces the same type as it's operand
    Negate {
        op: ValueId,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SwitchArm {
    // ConstValue should always be a const int since that what will correctly map to LLVM IR
    pub target: ConstValue,
    pub jump: BlockId,
}

/// A terminator instruction that ends a basic block and directs control flow.
#[derive(Debug, Clone, PartialEq, Serialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MirFunction {
    pub name: StrID,
    pub params: Vec<LocalId>,
    pub type_spec: TypeSpec,
    pub local_map: BTreeMap<NodeID, LocalId>,
    pub locals: Vec<Local>,      // indexed by LocalId
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

/// A collection of MIR functions (represents the entire program at the MIR level).
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MirModule {
    pub globals: Vec<Global>,
    pub init: MirFunction,
    pub functions: Vec<MirFunction>,
}

impl MirModule {
    pub fn new(globals: Vec<Global>, init: MirFunction, functions: Vec<MirFunction>) -> Self {
        MirModule {
            globals,
            init,
            functions,
        }
    }
}

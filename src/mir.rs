use std::collections::BTreeMap;
use std::fmt;

use crate::hir::NodeID;
use crate::str_store::StrID;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TagSize {
    U8,
    U16,
    U32,
    U64,
}

/// MIR-level type: all variants are concrete and map directly to LLVM types.
/// No inference artifacts or HIR back-references.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeSpec {
    I8,
    I16,
    I32,
    I64,
    // TODO: do we need unsigned types? We already encode signedness in ops
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
        // TODO: this is optional because the payload might be missing but really we should just
        // use a unit type here so we should fix that
        variants: Vec<TypeSpec>,
    },
}

/// A unique identifier for a value (temporary or SSA-like result from an instruction).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct ValueId(u32);

impl ValueId {
    /// Creates a ValueId from a raw usize. Panics if value is 0 (reserved as nil).
    pub fn from_usize(id: usize) -> Self {
        ValueId(id as u32)
    }

    /// Returns the ValueId as an index
    pub fn as_idx(self) -> usize {
        assert_ne!(self.0, 0, "ValueId(0) is reserved as nil");
        (self.0 - 1) as usize
    }

    /// Returns the nil ValueId (0).
    pub fn nil() -> Self {
        ValueId(0)
    }
}

/// A unique identifier for a block in the control-flow graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct BlockId(u32);

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Block_{:?}", self.0)
    }
}

impl BlockId {
    /// Creates a BlockId from a raw u32. Panics if id is 0 (reserved as nil).
    pub fn from_u32(id: u32) -> Self {
        assert_ne!(id, 0, "BlockId(0) is reserved as nil");
        BlockId(id)
    }

    /// Creates a BlockId from a raw usize. Panics if id is 0 (reserved as nil).
    pub fn from_usize(id: usize) -> Self {
        assert_ne!(id, 0, "BlockId(0) is reserved as nil");
        BlockId(id as u32)
    }

    /// Return the id as an index instead of an ID
    pub fn as_idx(self) -> usize {
        (self.0 - 1) as usize
    }
}

/// A unique identifier for a global variable (storage slot).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct GlobalId(u32);

impl GlobalId {
    /// Creates a GlobalId from a raw usize. Panics if id is 0 (reserved as nil).
    pub fn from_usize(id: usize) -> Self {
        assert_ne!(id, 0, "GlobalId(0) is reserved as nil");
        GlobalId(id as u32)
    }
}

/// Metadata about a global variable.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Global {
    pub type_spec: TypeSpec,
    pub name: StrID,
    pub public: bool,
}

/// A unique identifier for a local variable (storage slot).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct LocalId(u32);

impl LocalId {
    /// Creates a LocalId from a raw usize. Panics if id is 0 (reserved as nil).
    pub fn from_usize(id: usize) -> Self {
        assert_ne!(id, 0, "LocalId(0) is reserved as nil");
        LocalId(id as u32)
    }

    /// Return the id as an index instead of an ID
    pub fn as_idx(self) -> usize {
        (self.0 - 1) as usize
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
    Int(u64),
    Float(f64),
    Bool(bool),
    Array(Vec<ConstValue>),
    String(StrID),
    Struct(Vec<ConstValue>),
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
    /// Load a value from a place: load(place) -> ValueId
    Load {
        place: Place,
    },
    /// Store a value to a place: store(place, value) — produces Unit
    Store {
        place: Place,
        value: ValueId,
    },
    /// Take the address of a place: address_of(place) -> Ptr(T)
    AddressOf {
        place: Place,
    },
    // TODO: need to add a panic handler block so that panics can get propaged up correctly
    /// call(func, args...) -> ValueId
    Call {
        func: StrID, // Function name or identifier
        args: Vec<ValueId>,
    },
    /// variant_get_payload(src: Place) -> ValueId
    /// variants can only ever contain a single value in Manta so there's no need to extract a
    /// specific field. src is a Place so codegen can struct_gep directly into the enum's alloca.
    /// The payload type is already encoded in the instruction's result TypeSpec.
    VariantGetPayload {
        src: Place,
    },
    /// variant_get_tag(src: ValueId) -> ValueId
    /// Extracts the discriminant tag from an enum value as an integer.
    VariantGetTag {
        src: ValueId,
    },

    /// MakeVariant { tag, payload? } -> ValueId (type: Enum { ... })
    MakeVariant {
        tag: ConstValue, // ConstUInt(variant_id)
        payload: Option<ValueId>,
    },

    /// MakeStruct { fields } -> ValueId (type: Struct { ... })
    MakeStruct {
        fields: Vec<ValueId>,
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
    /// alloc(meta_type) -> OpaquePtr — allocates memory described by the meta_type struct value
    Alloc {
        meta_type: ValueId,
    },
    /// free(ptr) -> Unit — frees memory at the given pointer
    Free {
        ptr: ValueId,
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
    /// Panic (for code the exit control-flow with a panic)
    Panic { msg: ValueId },
    /// Break (for statements that break out of loops)
    Break,
    /// Continue (for statments that continue to the next iteration of loops)
    Continue,
}

/// A basic block in the control-flow graph.
/// `instructions` holds ValueIds that index into the owning MirFunction's flat instruction array.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BasicBlock {
    pub instructions: Vec<ValueId>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Linkage {
    External(StrID),
    Private,
    Public,
}

/// A function in MIR form.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MirFunction {
    pub linkage: Linkage,
    pub name: StrID,
    pub params: Vec<LocalId>,
    pub return_type: TypeSpec,
    pub local_map: BTreeMap<NodeID, LocalId>,
    pub locals: Vec<Local>,              // indexed by LocalId
    pub blocks: Vec<Option<BasicBlock>>, // Indexed by BlockId
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
    pub fn get_locals(&self) -> Vec<(LocalId, &Local)> {
        let mut locals = vec![];
        for (i, local) in self.locals.iter().enumerate() {
            let local_id = LocalId::from_usize(i + 1);
            locals.push((local_id, local))
        }

        locals
    }

    pub fn get_local(&self, local_id: LocalId) -> &Local {
        self.locals
            .get(local_id.as_idx())
            .expect("failed to get local from local_id")
    }

    pub fn get_block(&self, block_id: BlockId) -> &BasicBlock {
        match self.blocks.get(block_id.as_idx()) {
            Some(Some(b)) => b,
            _ => panic!("failed to find block with block_id {:?}", block_id),
        }
    }

    // TODO: this might only need to return BlockIds
    /// get a vector of all reachable blocks
    pub fn get_blocks(&self) -> Vec<(BlockId, &BasicBlock)> {
        let mut blocks = vec![];
        for (i, block) in self.blocks.iter().enumerate() {
            if let Some(block) = block {
                let block_id = BlockId::from_usize(i + 1);
                blocks.push((block_id, block))
            }
        }

        blocks
    }

    pub fn get_value_type(&self, value_id: ValueId) -> &TypeSpec {
        self.value_types
            .get(value_id.as_idx())
            .expect("failed to get value type")
    }

    pub fn get_inst(&self, value_id: ValueId) -> &Instruction {
        self.instructions
            .get(value_id.as_idx())
            .expect("failed to find instruction")
    }
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

    pub fn get_globals(&self) -> Vec<(GlobalId, &Global)> {
        let mut globals = vec![];
        for (i, global) in self.globals.iter().enumerate() {
            let global_id = GlobalId::from_usize(i + 1);
            globals.push((global_id, global))
        }

        globals
    }
}

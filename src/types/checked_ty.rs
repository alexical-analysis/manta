use crate::compiler::module::Module;
use crate::compiler::str_store::MStr;
use crate::index_vec::Indexer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CheckedTy(pub u32);

impl Indexer for CheckedTy {
    fn index(&self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for CheckedTy {
    fn from(value: usize) -> Self {
        CheckedTy(value as u32)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum CheckedTyValue {
    Panic,
    Unit,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Range,
    Named(NamedTy),
    Func(FuncTy),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NamedTy {
    pub module: Module,
    pub name: MStr,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FuncTy {
    pub params: Vec<CheckedTy>,
    pub return_ty: CheckedTy,
}

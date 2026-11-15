use crate::compiler::str_store::MStr;
use crate::index_vec::Indexer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UncheckedTy(u32);

impl Indexer for UncheckedTy {
    fn index(&self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for UncheckedTy {
    fn from(value: usize) -> Self {
        UncheckedTy(value as u32)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum UncheckedTyValue {
    Panic,
    Unit,
    ILiteral,
    I64,
    FLiteral,
    F64,
    Bool,
    Named(NamedTy),
    Func(FuncTy),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NamedTy {
    pub module: Option<MStr>,
    pub name: MStr,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FuncTy {
    pub params: Vec<UncheckedTy>,
    pub return_ty: UncheckedTy,
}

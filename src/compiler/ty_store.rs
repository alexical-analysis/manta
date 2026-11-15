use std::collections::HashMap;
use std::mem;

use bumpalo::Bump;

use crate::compiler::str_store::MStr;
use crate::index_vec::IndexVec;
use crate::types::checked_ty::{CheckedTy, CheckedTyValue};
use crate::types::unchecked_ty::{FuncTy, NamedTy, UncheckedTy, UncheckedTyValue};

pub struct CheckedTyStore {
    arena: Bump,
    map: HashMap<&'static CheckedTyValue, CheckedTy>,
    vec: IndexVec<CheckedTy, &'static CheckedTyValue>,
}

impl CheckedTyStore {
    pub fn new() -> Self {
        Self {
            arena: Bump::new(),
            map: HashMap::new(),
            vec: IndexVec::new(),
        }
    }

    pub fn get_ty(&mut self, type_value: CheckedTyValue) -> CheckedTy {
        if let Some(&id) = self.map.get(&type_value) {
            return id;
        }

        // SAFETY: Bump allocations live in heap-allocated chunks that never
        // move. We never expose these references beyond the lifetime of `self`.
        let interned: &'static CheckedTyValue =
            unsafe { mem::transmute(self.arena.alloc(type_value)) };
        let idx = self.vec.len() as u32;
        let ty = CheckedTy(idx);

        self.vec.push(interned);
        self.map.insert(interned, ty);

        ty
    }

    pub fn get_ty_value(&self, ty: CheckedTy) -> &CheckedTyValue {
        self.vec.get(ty).expect("failed to find checked type")
    }
}

pub struct UncheckedTyStore {
    type_arena: Bump,
    type_map: HashMap<&'static UncheckedTyValue, UncheckedTy>,
    ty_values: IndexVec<UncheckedTy, &'static UncheckedTyValue>,
}

impl UncheckedTyStore {
    pub fn new() -> Self {
        Self {
            type_arena: Bump::new(),
            type_map: HashMap::new(),
            ty_values: IndexVec::new(),
        }
    }

    pub fn get_ty(&mut self, ty: UncheckedTyValue) -> UncheckedTy {
        if let Some(&id) = self.type_map.get(&ty) {
            return id;
        }

        // SAFETY: Bump allocations live in heap-allocated chunks that never
        // move. We never expose these references beyond the lifetime of `self`.
        let interned: &'static UncheckedTyValue =
            unsafe { std::mem::transmute(self.type_arena.alloc(ty)) };
        let idx = self.ty_values.len();
        let ty = UncheckedTy::from(idx);

        self.ty_values.push(interned);
        self.type_map.insert(interned, ty);

        ty
    }

    pub fn get_ty_value(&self, ty: UncheckedTy) -> &UncheckedTyValue {
        self.ty_values.get(ty).expect("failed to get type")
    }

    pub fn get_fn_type(&mut self, params: Vec<UncheckedTy>, return_ty: UncheckedTy) -> UncheckedTy {
        self.get_ty(UncheckedTyValue::Func(FuncTy { params, return_ty }))
    }

    pub fn get_i64_ty(&mut self) -> UncheckedTy {
        self.get_ty(UncheckedTyValue::I64)
    }

    pub fn get_f64_ty(&mut self) -> UncheckedTy {
        self.get_ty(UncheckedTyValue::F64)
    }

    pub fn get_none_ty(&mut self) -> UncheckedTy {
        self.get_ty(UncheckedTyValue::Unit)
    }

    pub fn get_bool_ty(&mut self) -> UncheckedTy {
        self.get_ty(UncheckedTyValue::Bool)
    }

    pub fn get_named_ty(&mut self, module: Option<MStr>, name: MStr) -> UncheckedTy {
        self.get_ty(UncheckedTyValue::Named(NamedTy { module, name }))
    }
}

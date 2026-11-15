use std::collections::HashMap;
use std::path::Path;

use crate::ast::parser::Parser;
use crate::codegen::generator::Generator;
use crate::compiler::ast_store::AstStore;
use crate::compiler::builder::Builder;
use crate::compiler::hir_store::HirStore;
use crate::compiler::mir_store::MirStore;
use crate::compiler::module::{Module, ModuleValue};
use crate::compiler::str_store::{MStr, StrStore};
use crate::compiler::ty_store::{CheckedTyStore, UncheckedTyStore};
use crate::hir::noder::Noder;
use crate::index_vec::IndexVec;
use crate::mir::grapher::Grapher;

pub struct Context {
    pub str_store: StrStore,
    pub checked_ty_store: CheckedTyStore,
    pub unchecked_ty_store: UncheckedTyStore,
    pub ast_store: AstStore,
    pub hir_store: HirStore,
    pub mir_store: MirStore,
    pub module_store: IndexVec<Module, ModuleValue>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            str_store: StrStore::new(),
            checked_ty_store: CheckedTyStore::new(),
            unchecked_ty_store: UncheckedTyStore::new(),
            ast_store: AstStore::new(),
            hir_store: HirStore::new(),
            mir_store: MirStore::new(),
            module_store: IndexVec::new(),
        }
    }

    pub fn get_mstr(&mut self, s: &str) -> MStr {
        self.str_store.get_mstr(s)
    }

    pub fn create_builder<'ctx>(&'ctx mut self) -> Builder<'ctx> {
        Builder::new(self)
    }

    pub fn create_parser<'ctx>(&'ctx mut self, module: Module) -> Parser<'ctx> {
        Parser::new(self, module)
    }

    pub fn create_noder<'ctx>(&'ctx mut self, module: Module) -> Noder<'ctx> {
        Noder::new(self, module)
    }

    pub fn create_grapher<'ctx>(&'ctx mut self, module: Module) -> Grapher<'ctx> {
        Grapher::new(self, module)
    }

    pub fn create_generator<'ctx>(&'ctx mut self, module: Module) -> Generator<'ctx> {
        Generator::new(self, module)
    }

    pub fn create_module(&mut self, import_path: MStr) -> Module {
        let idx = self.module_store.len();
        let module_value = ModuleValue::new(import_path);
        self.module_store.push(module_value);

        Module::from(idx)
    }

    pub fn module_map(&self) -> HashMap<MStr, Module> {
        let mut module_map = HashMap::new();
        for (module, module_value) in self.module_store.iter_enumerated() {
            module_map.insert(module_value.name, module);
        }

        module_map
    }

    pub fn get_module_value(&self, module: Module) -> &ModuleValue {
        self.module_store
            .get(module)
            .expect("failed to find module value")
    }

    pub fn get_object_files(&self) -> Vec<&Path> {
        let mut paths = vec![];

        for module in self.module_store.iter() {
            paths.push(module.get_object_path());
        }

        paths
    }
}

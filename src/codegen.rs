use std::collections::BTreeMap;

use crate::mir::{BasicBlock, GlobalId, MirFunction, MirModule, TypeSpec};
use crate::str_store::StrStore;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::GlobalValue;

pub struct Codegen<'ctx, 'str> {
    str_store: &'str StrStore,
    context: &'ctx Context,
    module_name: String,
    global_map: BTreeMap<GlobalId, GlobalValue<'ctx>>,
}

impl<'ctx, 'str> Codegen<'ctx, 'str> {
    pub fn new(str_store: &'str StrStore, context: &'ctx Context, module_name: String) -> Self {
        Codegen {
            str_store,
            context,
            module_name,
            global_map: BTreeMap::new(),
        }
    }
    pub fn gen_module(&mut self, module: MirModule) -> Module<'ctx> {
        let llvm_module = self.context.create_module(&self.module_name);
        let builder = self.context.create_builder();

        // add all the globals to the module
        for (global_id, global) in module.get_globals() {
            let global_name = self
                .str_store
                .get_string(global.name)
                .expect("failed to get global name");
            let global_type = match self.gen_type_spec(&global.type_spec) {
                Some(ts) => ts,
                None => panic!("can not have global values with unit type"),
            };

            let global_value = llvm_module.add_global(global_type, None, global_name.as_str());
            self.global_map.insert(global_id, global_value);
        }

        // add the init function
        self.gen_function(&llvm_module, &builder, module.init);

        // add the rest of the functions
        for function in module.functions {
            self.gen_function(&llvm_module, &builder, function);
        }

        llvm_module
    }

    fn gen_function(&self, module: &Module, builder: &Builder, function: MirFunction) {}

    fn gen_block(&self, module: &Module, builder: &Builder, block: BasicBlock) {}

    fn gen_type_spec(&self, type_spec: &TypeSpec) -> Option<BasicTypeEnum<'ctx>> {
        if matches!(type_spec, TypeSpec::Unit) {
            return None;
        }

        let ts = match type_spec {
            TypeSpec::I8 => self.context.i8_type().into(),
            TypeSpec::I16 => self.context.i16_type().into(),
            TypeSpec::I32 => self.context.i32_type().into(),
            TypeSpec::I64 => self.context.i64_type().into(),

            // LLVM has no unsigned; signedness is in ops so we just use the basic int types here
            TypeSpec::U8 => self.context.i8_type().into(),
            TypeSpec::U16 => self.context.i16_type().into(),
            TypeSpec::U32 => self.context.i32_type().into(),
            TypeSpec::U64 => self.context.i64_type().into(),

            TypeSpec::F32 => self.context.f32_type().into(),
            TypeSpec::F64 => self.context.f64_type().into(),
            TypeSpec::Bool => self.context.bool_type().into(),
            TypeSpec::Ptr(_) | TypeSpec::OpaquePtr => {
                self.context.ptr_type(AddressSpace::default()).into()
            }
            TypeSpec::Array { elem, len } => self
                .gen_type_spec(elem)
                .expect("can not get array of unit types")
                .array_type(*len as u32)
                .into(),
            TypeSpec::Struct(fields) => {
                let field_types: Vec<_> = fields
                    .iter()
                    .map(|f| {
                        self.gen_type_spec(f)
                            .expect("can not have a field of unit type")
                    })
                    .collect();
                self.context.struct_type(&field_types, false).into()
            }
            _ => todo!("not yet supported"),
        };

        Some(ts)
    }
}

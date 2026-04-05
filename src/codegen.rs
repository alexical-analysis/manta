use std::collections::BTreeMap;

use crate::blocker::{self, Arch};
use crate::mir::{BasicBlock, GlobalId, Local, LocalId, MirFunction, MirModule, TagSize, TypeSpec};
use crate::str_store::StrStore;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{GlobalValue, PointerValue};

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

    fn gen_function(&self, module: &Module<'ctx>, builder: &Builder, function: MirFunction) {
        let function_name = self
            .str_store
            .get_string(function.name)
            .expect("failed to get function name");

        let return_type = self.gen_type_spec(&function.return_type);
        let mut param_types: Vec<BasicMetadataTypeEnum> = vec![];
        for param in &function.params {
            let local_type = &function
                .locals
                .get(param.as_idx())
                .expect("missing local for param")
                .type_spec;

            let param_type = self
                .gen_type_spec(local_type)
                .expect("can not have paramater of unit type");

            param_types.push(param_type.into());
        }

        let func_type = match return_type {
            Some(ts) => ts.fn_type(&param_types, false),
            None => self.context.void_type().fn_type(&param_types, false),
        };

        let func = module.add_function(function_name.as_str(), func_type, None);

        let entry_block = function.entry_block;
        let block = function
            .blocks
            .get(entry_block.as_idx())
            .expect("failed to get entry block");

        // build the entry block
        let entry_block = self.context.append_basic_block(func, "entry");
        builder.position_at_end(entry_block);
        let locals = self.gen_locals(builder, &function.get_locals());
        self.gen_block(module, builder, &locals, block);

        for (block_id, block) in function.get_blocks() {
            if function.entry_block == block_id {
                continue;
            }

            let basic_block = self
                .context
                .append_basic_block(func, block_id.to_string().as_str());
            builder.position_at_end(basic_block);
            self.gen_block(module, builder, &locals, block);
        }
    }

    fn gen_locals(
        &self,
        builder: &Builder<'ctx>,
        locals: &[(LocalId, &Local)],
    ) -> BTreeMap<LocalId, PointerValue<'ctx>> {
        let mut local_map = BTreeMap::new();

        for (local_id, local) in locals {
            let type_name = self
                .str_store
                .get_string(local.name)
                .expect("missing local name");
            let type_spec = self
                .gen_type_spec(&local.type_spec)
                .expect("can not have a local with unit type");

            let local_value = builder
                .build_alloca(type_spec, type_name.as_str())
                .expect("failed to alloca local");

            local_map.insert(*local_id, local_value);
        }

        local_map
    }

    fn gen_block(
        &self,
        module: &Module,
        builder: &Builder,
        locals: &BTreeMap<LocalId, PointerValue<'ctx>>,
        block: &BasicBlock,
    ) {
    }

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
            TypeSpec::String => {
                let len_type = self.context.i64_type();
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                self.context
                    .struct_type(&[len_type.into(), ptr_type.into()], false)
                    .into()
            }
            TypeSpec::Enum { tag_size, variants } => {
                let tag_type = match tag_size {
                    TagSize::U8 => self.context.i8_type(),
                    TagSize::U16 => self.context.i16_type(),
                    TagSize::U32 => self.context.i32_type(),
                    TagSize::U64 => self.context.i64_type(),
                };

                let mut max_size = 0;
                for payload in variants.iter().flatten() {
                    // TODO: need to actually respect the target arch width
                    let layout = blocker::type_layout(payload, Arch::W64);
                    if layout.size() > max_size {
                        max_size = layout.size()
                    }
                }

                let raw_data_type = self.context.i8_type().array_type(max_size as u32);

                self.context
                    .struct_type(&[tag_type.into(), raw_data_type.into()], false)
                    .into()
            }
            TypeSpec::Slice(_) => self
                .context
                .struct_type(
                    &[
                        self.context.i64_type().into(),
                        self.context.i64_type().into(),
                        self.context.ptr_type(AddressSpace::default()).into(),
                    ],
                    false,
                )
                .into(),
            ts => todo!("failed to gen type for type spec: {:?}", ts),
        };

        Some(ts)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::path::Path;

    use crate::blocker::Blocker;
    use crate::noder::node_module;
    use crate::parser::Parser;
    use crate::str_store::StrStore;

    fn assert_file_path_eq(path: &Path, codegen_dir: &Path) {
        let ext = path.extension().expect("Failed to get file extension");
        if ext != "manta" {
            return;
        }

        let file_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => panic!("Failed to read {}", path.display()),
        };

        let mut str_store = StrStore::new();
        let parser = Parser::new(source);
        let module = parser.parse_module(&mut str_store);

        let node_tree = node_module(module);
        let blocker = Blocker::new(&node_tree);
        let mir_module = blocker.build_module();

        let context = Context::create();
        let mut codegen = Codegen::new(&str_store, &context, file_name.to_string());
        let llvm_module = codegen.gen_module(mir_module);

        let ll_output = llvm_module.print_to_string().to_string();

        let codegen_file = codegen_dir.join(format!("{}.ll", file_name));

        if codegen_file.exists() {
            let expected_ll = match fs::read_to_string(&codegen_file) {
                Ok(s) => s,
                Err(_) => panic!("Failed to read {}", codegen_file.display()),
            };

            assert_eq!(
                ll_output, expected_ll,
                "Codegen output mismatch for {}",
                file_name
            );
        } else {
            fs::create_dir_all(codegen_dir).expect("Failed to create codegen test directory");

            match fs::write(&codegen_file, &ll_output) {
                Ok(_) => (),
                Err(_) => panic!("Failed to write codegen output to {:?}", codegen_file),
            };

            panic!(
                "Generated new codegen output file: {:?}. Please verify its correctness.",
                codegen_file
            );
        }
    }

    include!(concat!(env!("OUT_DIR"), "/generated_codegen_tests.rs"));
}

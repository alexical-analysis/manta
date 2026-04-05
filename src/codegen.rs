use std::collections::{BTreeMap, HashMap};

use crate::blocker::{self, Arch};
use crate::mir::{
    self, BlockId, ConstValue, GlobalId, Instruction, Local, LocalId, MirFunction, MirModule,
    TagSize, Terminator, TypeSpec, ValueId,
};
use crate::str_store::StrStore;

use inkwell::AddressSpace;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, GlobalValue, PointerValue};

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

        let func_value = module.add_function(function_name.as_str(), func_type, None);

        // pre-gen all the blocks
        let mut block_map = HashMap::new();
        let mut entry_block = None;
        for (block_id, block) in function.get_blocks() {
            let basic_block;
            if block_id == function.entry_block {
                basic_block = self.context.append_basic_block(func_value, "entry");
                entry_block = Some(block);
            } else {
                basic_block = self
                    .context
                    .append_basic_block(func_value, &block_id.to_string());
            }

            block_map.insert(block_id, basic_block);
        }

        // build the entry block
        let block = block_map
            .get(&function.entry_block)
            .expect("failed to get inkwell entry block");
        builder.position_at_end(*block);

        let local_map = self.gen_local_map(builder, &function.get_locals());
        let entry_block = entry_block.expect("failed to find mir entry block");
        self.gen_block(
            module,
            builder,
            &block_map,
            &local_map,
            &function,
            entry_block,
        );

        for (block_id, block) in function.get_blocks() {
            if function.entry_block == block_id {
                continue;
            }

            let basic_block = block_map.get(&block_id).expect("failed to get basic block");
            builder.position_at_end(*basic_block);
            self.gen_block(module, builder, &block_map, &local_map, &function, block);
        }
    }

    fn gen_local_map(
        &self,
        builder: &Builder<'ctx>,
        locals: &[(LocalId, &Local)],
    ) -> HashMap<LocalId, PointerValue<'ctx>> {
        let mut local_map = HashMap::new();

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

    // TODO: we're getting a lot of params here (6), that probably means we should try to bundle
    // some of these together and expose only the functionality that we need to simplify the code
    fn gen_block(
        &self,
        module: &Module,
        builder: &Builder<'ctx>,
        block_map: &HashMap<BlockId, BasicBlock>,
        local_map: &HashMap<LocalId, PointerValue<'ctx>>,
        function: &MirFunction,
        block: &mir::BasicBlock,
    ) {
        let mut value_map: HashMap<ValueId, BasicValueEnum<'ctx>> = HashMap::new();
        // TODO: should this be in a gen_inst function instead of having this all be in a loop like
        // this?
        for value_id in &block.instructions {
            let inst = function
                .instructions
                .get(value_id.as_idx())
                .expect("failed to get instruction for block");

            match inst {
                Instruction::Const { value } => {
                    let value = self.gen_const(value);
                    value_map.insert(*value_id, value);
                }
                _ => {
                    eprintln!(
                        "TODO: not all instructions are supported yet, skipping for now {:?}",
                        inst
                    );
                }
            }
        }

        match &block.terminator {
            Terminator::Return { value } => {
                // TODO: I can probably move to .map instead of doing the match like this
                match value {
                    Some(v) => {
                        let value = value_map.get(v);
                        let value: Option<&dyn BasicValue<'ctx>> = match value {
                            Some(v) => Some(v),
                            None => {
                                eprintln!(
                                    "not all values are being calculated yet, skipping for now"
                                );
                                return;
                            }
                        };
                        builder.build_return(value).expect("failed to build return");
                    }
                    None => {
                        builder.build_return(None).expect("failed to build return");
                    }
                };
            }
            Terminator::Jump { target } => {
                let dest = block_map.get(target).expect(
                    format!("failed to get target block from block map {:?}", target).as_str(),
                );
                builder
                    .build_unconditional_branch(*dest)
                    .expect("failed to build unconditinoal branch");
            }
            Terminator::Branch {
                cond,
                true_target,
                false_target,
            } => {
                let cond = value_map.get(cond);
                let cond = match cond {
                    Some(v) => v.into_int_value(),
                    None => {
                        eprintln!(
                            "TODO: not all values are supported yet, returning const 0 for now"
                        );
                        self.context.i64_type().const_zero()
                    }
                };
                let then_block = block_map
                    .get(true_target)
                    .expect("failed to find true target in block map");
                let else_block = block_map
                    .get(false_target)
                    .expect("failed to find false target in block map");
                builder
                    .build_conditional_branch(cond, *then_block, *else_block)
                    .expect("failed to build conditional branch");
            }
            Terminator::SwitchVariant {
                discriminant,
                default,
                arms,
            } => {
                let value = value_map.get(discriminant);
                let value = match value {
                    Some(v) => v.into_int_value(),
                    None => {
                        eprintln!("TODO: not all values are computed yet, using const 0 for now");
                        self.context.i64_type().const_zero()
                    }
                };
                let else_block = block_map
                    .get(default)
                    .expect("failed to find default block in block map");
                let mut cases = vec![];
                for arm in arms {
                    let target = self.gen_const(&arm.target).into_int_value();
                    let basic_block = block_map
                        .get(&arm.jump)
                        .expect("failed to get switch target from block map");
                    cases.push((target, *basic_block))
                }

                builder
                    .build_switch(value, *else_block, &cases)
                    .expect("failed to build switch");
            }
            Terminator::Unreachable => {
                builder
                    .build_unreachable()
                    .expect("failed to build unreachable");
            }
            Terminator::Panic => {
                builder.build_unreachable().expect("failed to build panic");
            }
        };
    }

    fn gen_const(&self, const_value: &ConstValue) -> BasicValueEnum<'ctx> {
        match const_value {
            ConstValue::ConstInt(i) => {
                let value = self.context.i64_type().const_int(*i as u64, false);
                value.into()
            }
            _ => {
                eprintln!(
                    "TODO: not all constants are supported yet emitting const 0 for now {:?}",
                    const_value
                );
                self.context.i64_type().const_zero().into()
            }
        }
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

use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue,
    PointerValue, StructValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use crate::blocker::{self, Arch};
use crate::mir::{
    self, BlockId, ConstValue, Instruction, Linkage as MirLinkage, LocalId, MirFunction, SwitchArm,
    TagSize, TypeSpec, ValueId,
};
use crate::str_store::{self, StrStore};

// build an llvm function value from a manta MirFunction
pub fn build_func_value<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    str_store: &StrStore,
    import_path: &PathBuf,
    function: &MirFunction,
) -> FunctionValue<'ctx> {
    // build the function value
    let param_types: Vec<BasicMetadataTypeEnum> = function
        .params
        .iter()
        .map(|local_id| {
            function
                .locals
                .get(local_id.as_idx())
                .expect("failed to find paramater local")
        })
        .map(|ts| &ts.type_spec)
        .map(|ts| {
            convert_type_spec(context, ts)
                .expect("params can not be of unit type")
                .into()
        })
        .collect();

    let return_type = convert_type_spec(context, &function.return_type);
    let func_type = match return_type {
        Some(ts) => ts.fn_type(&param_types, false),
        None => context.void_type().fn_type(&param_types, false),
    };

    let function_name = str_store
        .get_string(function.name)
        .expect("failed to get function name");

    match function.linkage {
        MirLinkage::Public => {
            // external functions need to be prefixed with the module name so there are no linking collisions
            let mut function_prefix = String::from("manta_");
            let import_path = import_path.to_string_lossy().to_string();
            if !import_path.is_empty() {
                let import_path = import_path.replace("_", "_0").replace("/", "_") + "_";
                function_prefix = function_prefix + &import_path;
            }

            let function_name = function_prefix + function_name.as_str();
            module.add_function(function_name.as_str(), func_type, Some(Linkage::External))
        }
        MirLinkage::Private => {
            module.add_function(function_name.as_str(), func_type, Some(Linkage::Internal))
        }
        MirLinkage::External(import_path) => {
            // external functions need to be prefixed with the module name so there are no linking collisions
            let mut function_prefix = String::from("manta_");
            let import_path = str_store
                .get_string(import_path)
                .expect("failed to get import path");
            if !import_path.is_empty() {
                let import_path = import_path.replace("_", "_0").replace("/", "_") + "_";
                function_prefix = function_prefix + &import_path;
            }

            let function_name = function_prefix + function_name.as_str();
            module.add_function(function_name.as_str(), func_type, Some(Linkage::External))
        }
    }
}

pub fn build_c_deps<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
) -> HashMap<&'static str, FunctionValue<'ctx>> {
    let mut c_funcs = HashMap::new();

    // TODO: need to make sure malloc uses the correct arch-width. Just assuming Arch:W64 for
    // now, but will need to update that in the future
    let malloc_type = context
        .ptr_type(AddressSpace::default())
        .fn_type(&[context.i64_type().into()], false);
    let malloc_fn = module.add_function("malloc", malloc_type, Some(Linkage::External));
    c_funcs.insert("malloc", malloc_fn);

    let free_type = context
        .void_type()
        .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false);
    let free_fn = module.add_function("free", free_type, Some(Linkage::External));
    c_funcs.insert("free", free_fn);

    let puts_type = context
        .i32_type()
        .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false);
    let puts_fn = module.add_function("puts", puts_type, Some(Linkage::External));
    c_funcs.insert("puts", puts_fn);

    let abort_type = context.void_type().fn_type(&[], false);
    let abort_fn = module.add_function("abort", abort_type, Some(Linkage::External));
    c_funcs.insert("abort", abort_fn);

    let write_type = context.i64_type().fn_type(
        &[
            context.i32_type().into(),
            context.ptr_type(AddressSpace::default()).into(),
            context.i64_type().into(),
        ],
        false,
    );
    let write_fn = module.add_function("write", write_type, Some(Linkage::External));
    c_funcs.insert("write", write_fn);

    c_funcs
}

// builds the manta panic builtin by wrapping puts and abort
pub fn build_manta_panic<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    puts_fn: FunctionValue<'ctx>,
    abort_fn: FunctionValue<'ctx>,
) -> FunctionValue<'ctx> {
    // TODO: eventually it would be nice to have panic take a message but first we need to get
    // structs, and then strings straight earlier on in the pipeline
    let panic_type = context.void_type().fn_type(&[], false);
    let panic_name =
        str_store::constant_id_str(str_store::PANIC).expect("failed to get panic name");

    let panic_fn = module.add_function(panic_name, panic_type, Some(Linkage::Internal));

    let entry_block = context.append_basic_block(panic_fn, "entry");
    builder.position_at_end(entry_block);

    let panic_msg = builder
        .build_global_string_ptr("Panic reached! exiting!", "panic_msg")
        .expect("failed to build panic message")
        .as_pointer_value();

    builder
        .build_call(puts_fn, &[panic_msg.into()], "puts")
        .expect("failed to build puts call");

    builder
        .build_call(abort_fn, &[], "abort")
        .expect("failed to build abort call");

    builder
        .build_unreachable()
        .expect("failed to build unreachable");

    panic_fn
}

/// builds the manta alloc builtin by wrapping c-runtimes malloc
pub fn build_manta_alloc<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    malloc_fn: FunctionValue<'ctx>,
) -> FunctionValue<'ctx> {
    let u64_type = context.i64_type();
    let meta_type =
        context.struct_type(&[u64_type.into(), u64_type.into(), u64_type.into()], false);
    let alloc_type = context
        .ptr_type(AddressSpace::default())
        .fn_type(&[meta_type.into()], false);
    let alloc_name =
        str_store::constant_id_str(str_store::ALLOC).expect("failed to get alloc name");

    let alloc_fn = module.add_function(alloc_name, alloc_type, Some(Linkage::Internal));

    let entry_block = context.append_basic_block(alloc_fn, "entry");
    builder.position_at_end(entry_block);

    let meta = alloc_fn
        .get_nth_param(0)
        .expect("failed to get input meta")
        .into_struct_value();
    let size_of = builder
        .build_extract_value(meta, 0, "meta_size")
        .expect("failed to extract size from meta struct");

    let ptr = builder
        .build_call(malloc_fn, &[size_of.into()], "malloc")
        .expect("failed to build call")
        .try_as_basic_value()
        .expect_basic("failed to conver malloc return into a basic value");

    let ret: Option<&dyn BasicValue<'ctx>> = Some(&ptr);
    builder.build_return(ret).expect("failed to build return");

    alloc_fn
}

pub fn build_manta_print<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    write_fn: FunctionValue<'ctx>,
) -> FunctionValue<'ctx> {
    let str_type = context.struct_type(
        &[
            context.i64_type().into(),
            context.ptr_type(AddressSpace::default()).into(),
        ],
        false,
    );
    let print_type = context.void_type().fn_type(&[str_type.into()], false);
    let print_fn = module.add_function("print", print_type, Some(Linkage::Internal));

    let entry_block = context.append_basic_block(print_fn, "entry");
    builder.position_at_end(entry_block);

    let msg = print_fn
        .get_nth_param(0)
        .expect("failed to get input message")
        .into_struct_value();
    let len = builder
        .build_extract_value(msg, 0, "len")
        .expect("failed to build extract value");
    let ptr = builder
        .build_extract_value(msg, 1, "ptr")
        .expect("failed to build extract value");

    let std_out = context.i32_type().const_int(1, false);
    builder
        .build_call(write_fn, &[std_out.into(), ptr.into(), len.into()], "write")
        .expect("failed to build call");

    builder.build_return(None).expect("failed to build return");

    print_fn
}

pub fn build_manta_eprint<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    write_fn: FunctionValue<'ctx>,
) -> FunctionValue<'ctx> {
    let str_type = context.struct_type(
        &[
            context.i64_type().into(),
            context.ptr_type(AddressSpace::default()).into(),
        ],
        false,
    );
    let eprint_type = context.void_type().fn_type(&[str_type.into()], false);
    let eprint_fn = module.add_function("eprint", eprint_type, Some(Linkage::Internal));

    let entry_block = context.append_basic_block(eprint_fn, "entry");
    builder.position_at_end(entry_block);

    let msg = eprint_fn
        .get_nth_param(0)
        .expect("failed to get input message")
        .into_struct_value();
    let len = builder
        .build_extract_value(msg, 0, "len")
        .expect("failed to build extract value");
    let ptr = builder
        .build_extract_value(msg, 1, "ptr")
        .expect("failed to build extract value");

    let std_err = context.i32_type().const_int(2, false);
    builder
        .build_call(write_fn, &[std_err.into(), ptr.into(), len.into()], "write")
        .expect("failed to build call");

    builder.build_return(None).expect("failed to build return");

    eprint_fn
}

pub struct FuncBuilder<'ctx, 'a> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    mir_function: &'a MirFunction,
    current_block: BlockId,
    block_map: BTreeMap<BlockId, BasicBlock<'ctx>>,
    local_map: HashMap<LocalId, PointerValue<'ctx>>,
    value_map: HashMap<ValueId, BasicValueEnum<'ctx>>,
}

impl<'ctx, 'a> FuncBuilder<'ctx, 'a> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        function: &'a MirFunction,
        llvm_function: FunctionValue<'ctx>,
        str_store: &StrStore,
    ) -> Self {
        if matches!(function.linkage, MirLinkage::External(_)) {
            return FuncBuilder {
                context,
                builder,
                mir_function: function,
                current_block: function.entry_block,
                block_map: BTreeMap::new(),
                local_map: HashMap::new(),
                value_map: HashMap::new(),
            };
        }

        // pre-gen all the blocks
        let mut block_map = BTreeMap::new();
        let mut entry_block = None;
        for (block_id, _) in function.get_blocks() {
            let basic_block;
            if block_id == function.entry_block {
                basic_block = context.append_basic_block(llvm_function, "entry");
                entry_block = Some(basic_block);
            } else {
                basic_block = context.append_basic_block(llvm_function, &block_id.to_string());
            }

            block_map.insert(block_id, basic_block);
        }

        // ensure all the paramaters are used to assign locals
        match entry_block {
            Some(block) => builder.position_at_end(block),
            None => panic!("failed to find entry block in function"),
        };

        let mut local_map = HashMap::new();
        for (local_id, local) in function.get_locals() {
            let type_name = str_store
                .get_string(local.name)
                .expect("missing local name");
            let type_spec = convert_type_spec(context, &local.type_spec)
                .expect("can not have a local with unit type");

            let local_value = builder
                .build_alloca(type_spec, type_name.as_str())
                .expect("failed to alloca local");

            local_map.insert(local_id, local_value);
        }

        for (i, param) in function.params.iter().enumerate() {
            let local = local_map.get(param).expect("failed to get paramater");
            let value = llvm_function
                .get_nth_param(i as u32)
                .expect("failed to get nth param");
            builder
                .build_store(*local, value)
                .expect("failed to store paramater");
        }

        FuncBuilder {
            context,
            builder,
            mir_function: function,
            current_block: function.entry_block,
            block_map,
            local_map,
            value_map: HashMap::new(),
        }
    }

    pub fn get_blocks(&self) -> Vec<(BlockId, &'a mir::BasicBlock)> {
        // TODO: need to get these blocks in dominance order so that all values_ids exist before
        // they are read
        self.block_map
            .iter()
            .map(|b| (*b.0, self.mir_function.get_block(*b.0)))
            .collect()
    }

    pub fn position_at_entry_block(&self) {
        let block = self
            .block_map
            .get(&self.mir_function.entry_block)
            .expect("failed to find entry block in block map");
        match block.get_first_instruction() {
            Some(first_inst) => self.builder.position_before(&first_inst),
            None => self.builder.position_at_end(*block),
        }
    }

    pub fn position_at_block(&mut self, block_id: BlockId) {
        self.current_block = block_id;
        let block = self
            .block_map
            .get(&block_id)
            .expect("failed to find block in block map");
        self.builder.position_at_end(*block);
    }

    pub fn get_inst(&self, value_id: ValueId) -> &Instruction {
        self.mir_function.get_inst(value_id)
    }

    pub fn get_llvm_value(&self, value_id: ValueId) -> Option<&BasicValueEnum<'ctx>> {
        self.value_map.get(&value_id)
    }

    pub fn get_value_type(&self, value_id: ValueId) -> &TypeSpec {
        self.mir_function.get_value_type(value_id)
    }

    pub fn get_local_ptr(&'a self, local_id: LocalId) -> &'a PointerValue<'ctx> {
        self.local_map
            .get(&local_id)
            .expect("failed to get local value from local map")
    }

    pub fn get_local_type_spec(&self, local_id: LocalId) -> &TypeSpec {
        &self.mir_function.get_local(local_id).type_spec
    }

    pub fn insert_value(&mut self, value_id: ValueId, value: BasicValueEnum<'ctx>) {
        self.value_map.insert(value_id, value);
    }

    fn convert_type_spec(&self, type_spec: &TypeSpec) -> Option<BasicTypeEnum<'ctx>> {
        convert_type_spec(self.context, type_spec)
    }

    fn convert_const(
        &self,
        const_value: &ConstValue,
        type_spec: &TypeSpec,
    ) -> BasicValueEnum<'ctx> {
        match (const_value, type_spec) {
            (ConstValue::Int(i), TypeSpec::I8) => {
                let value = self.context.i8_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::Int(i), TypeSpec::I16) => {
                let value = self.context.i16_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::Int(i), TypeSpec::I32) => {
                let value = self.context.i32_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::Int(i), TypeSpec::I64) => {
                let value = self.context.i64_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::Int(i), TypeSpec::U8) => {
                let value = self.context.i8_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::Int(i), TypeSpec::U16) => {
                let value = self.context.i16_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::Int(i), TypeSpec::U32) => {
                let value = self.context.i32_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::Int(i), TypeSpec::U64) => {
                let value = self.context.i64_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::Float(f), TypeSpec::F32) => {
                let value = self.context.f32_type().const_float(*f);
                value.into()
            }
            (ConstValue::Float(f), TypeSpec::F64) => {
                let value = self.context.f64_type().const_float(*f);
                value.into()
            }
            (ConstValue::Bool(b), TypeSpec::Bool) => {
                let value = match b {
                    true => self.context.bool_type().const_int(1, false),
                    false => self.context.bool_type().const_zero(),
                };
                value.into()
            }
            (ConstValue::Array(values), TypeSpec::Array { elem, .. }) => {
                let mut basic_values = vec![];
                for value in values {
                    let const_value = self.convert_const(value, elem);
                    basic_values.push(const_value);
                }

                let gen_type = self
                    .convert_type_spec(elem)
                    .expect("can not have const array of unit type");

                match gen_type {
                    BasicTypeEnum::ArrayType(gen_type) => {
                        let values: Vec<_> =
                            basic_values.iter().map(|v| v.into_array_value()).collect();
                        let value = gen_type.const_array(&values);
                        value.into()
                    }
                    BasicTypeEnum::FloatType(gen_type) => {
                        let values: Vec<_> =
                            basic_values.iter().map(|v| v.into_float_value()).collect();
                        let value = gen_type.const_array(&values);
                        value.into()
                    }
                    BasicTypeEnum::IntType(gen_type) => {
                        let values: Vec<_> =
                            basic_values.iter().map(|v| v.into_int_value()).collect();
                        let value = gen_type.const_array(&values);
                        value.into()
                    }
                    _ => todo!("TODO: not all array types are supported yet"),
                }
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
}

// This impl block provides all the wrappers around inkwell builder calls
impl<'ctx, 'a> FuncBuilder<'ctx, 'a> {
    pub fn build_unreachable(&self) {
        self.builder
            .build_unreachable()
            .expect("failed to build unreachable terminator");
    }

    pub fn build_panic(&self, panic_fn: FunctionValue<'ctx>) {
        self.build_void_call("panic", panic_fn, &[]);
        self.build_unreachable();
    }

    pub fn build_value_return(&self, value_id: &ValueId) {
        match self.value_map.get(value_id) {
            Some(v) => {
                let value: Option<&dyn BasicValue<'ctx>> = Some(v);
                self.builder
                    .build_return(value)
                    .expect("failed to build value return terminator");
            }
            None => {
                eprintln!("not all values are being calculated yet, skipping for now");
                self.build_unreachable();
            }
        }
    }

    pub fn build_void_return(&self) {
        self.builder
            .build_return(None)
            .expect("failed to build void return terminator");
    }

    pub fn build_unconditional_branch(&self, target: &BlockId) {
        let dest = self
            .block_map
            .get(target)
            .expect("failed to get target block");
        self.builder
            .build_unconditional_branch(*dest)
            .expect("failed to build unconditional branch terminator");
    }

    pub fn build_conditional_branch(
        &self,
        cond: &ValueId,
        then_block: &BlockId,
        else_block: &BlockId,
    ) {
        let cond = self.value_map.get(cond);
        let cond = match cond {
            Some(BasicValueEnum::IntValue(v)) => *v,
            Some(BasicValueEnum::PointerValue(ptr)) => {
                // Pointer used as a bool: non-null = true, null = false
                let null = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder
                    .build_int_compare(IntPredicate::NE, *ptr, null, "ptr_nonnull")
                    .expect("failed to build pointer null check")
            }
            Some(_) => {
                eprintln!("TODO: not all values are supported yet, returning const 0 for now");
                self.context.bool_type().const_zero()
            }
            None => {
                eprintln!("TODO: not all values are supported yet, returning const 0 for now");
                self.context.bool_type().const_zero()
            }
        };

        let then_block = *self
            .block_map
            .get(then_block)
            .expect("failed to get then block");
        let else_block = *self
            .block_map
            .get(else_block)
            .expect("failed to get then block");

        self.builder
            .build_conditional_branch(cond, then_block, else_block)
            .expect("failed to build conditional branch terminator");
    }

    pub fn build_switch(
        &self,
        discriminant: &ValueId,
        default_block: &BlockId,
        arms: &Vec<SwitchArm>,
    ) {
        let value = self.value_map.get(discriminant);
        let value = match value {
            Some(v) => v.into_int_value(),
            None => {
                eprintln!("TODO: not all values are supported yet, returning const 0 for now");
                self.context.bool_type().const_zero()
            }
        };
        let discriminant_ty = self.get_value_type(*discriminant);

        let else_block = *self
            .block_map
            .get(default_block)
            .expect("failed to find default block in block map");

        let mut cases = vec![];
        for arm in arms {
            let target = self
                .convert_const(&arm.target, discriminant_ty)
                .into_int_value();
            let basic_block = self
                .block_map
                .get(&arm.jump)
                .expect("failed to get switch target from block map");
            cases.push((target, *basic_block))
        }

        self.builder
            .build_switch(value, else_block, &cases)
            .expect("failed to build switch terminator");
    }

    pub fn build_alloca(&mut self, type_spec: &TypeSpec, name: &str) -> PointerValue<'ctx> {
        let block_id = self.current_block;
        self.position_at_entry_block();

        let ty = self
            .convert_type_spec(type_spec)
            .expect("cant alloca a unit type");
        let ptr = self
            .builder
            .build_alloca(ty, name)
            .expect("failed to build alloca");

        self.position_at_block(block_id);

        ptr
    }

    pub fn build_int_add(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_int_add(lhs, rhs, "iadd")
            .expect("failed to build integer addition")
            .into()
    }

    pub fn build_int_sub(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_int_sub(lhs, rhs, "isub")
            .expect("failed to build integer subtraction")
            .into()
    }

    pub fn build_float_add(
        &self,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_float_add(lhs, rhs, "fadd")
            .expect("failed to build float addition")
            .into()
    }

    pub fn build_float_sub(
        &self,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_float_sub(lhs, rhs, "fsub")
            .expect("failed to build float subtraction")
            .into()
    }

    pub fn build_int_signed_div(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_int_signed_div(lhs, rhs, "sdiv")
            .expect("failed to build signed integer division")
            .into()
    }

    pub fn build_int_unsigned_div(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_int_unsigned_div(lhs, rhs, "udiv")
            .expect("failed to build unsigned integer division")
            .into()
    }

    pub fn build_float_div(
        &self,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_float_div(lhs, rhs, "fdiv")
            .expect("failed to build float division")
            .into()
    }

    pub fn build_int_mul(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_int_mul(lhs, rhs, "imul")
            .expect("failed to build integer multiplication")
            .into()
    }

    pub fn build_float_mul(
        &self,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_float_mul(lhs, rhs, "fmul")
            .expect("failed to build float multiplication")
            .into()
    }

    pub fn build_int_signed_rem(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_int_signed_rem(lhs, rhs, "srem")
            .expect("failed to build signed integer remainder")
            .into()
    }

    pub fn build_int_unsigned_rem(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_int_unsigned_rem(lhs, rhs, "urem")
            .expect("failed to build signed integer remainder")
            .into()
    }

    pub fn build_int_compare(
        &self,
        op: IntPredicate,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let name = match op {
            IntPredicate::EQ => "ieq",
            IntPredicate::NE => "ine",
            IntPredicate::SGT => "sgt",
            IntPredicate::SGE => "sge",
            IntPredicate::SLT => "slt",
            IntPredicate::SLE => "sle",
            IntPredicate::UGT => "ugt",
            IntPredicate::UGE => "uge",
            IntPredicate::ULT => "ult",
            IntPredicate::ULE => "ule",
        };

        self.builder
            .build_int_compare(op, lhs, rhs, name)
            .expect("failed to build int compare")
            .into()
    }

    pub fn build_float_compare(
        &self,
        op: FloatPredicate,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let name = match op {
            FloatPredicate::OEQ => "feq",
            FloatPredicate::ONE => "fne",
            FloatPredicate::OGT => "fgt",
            FloatPredicate::OGE => "fge",
            FloatPredicate::OLT => "flt",
            FloatPredicate::OLE => "fle",
            FloatPredicate::UEQ => "fueq",
            FloatPredicate::UNE => "fune",
            FloatPredicate::UGT => "fugt",
            FloatPredicate::UGE => "fuge",
            FloatPredicate::ULT => "fult",
            FloatPredicate::ULE => "fule",
            FloatPredicate::ORD => "ford",
            FloatPredicate::UNO => "funo",
            FloatPredicate::PredicateFalse => "ffalse",
            FloatPredicate::PredicateTrue => "ftrue",
        };

        self.builder
            .build_float_compare(op, lhs, rhs, name)
            .expect("failed to build float compare")
            .into()
    }

    pub fn build_and(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_and(lhs, rhs, "and")
            .expect("failed to build logical and")
            .into()
    }

    pub fn build_or(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_or(lhs, rhs, "or")
            .expect("failed to build logical or")
            .into()
    }

    pub fn build_load(
        &self,
        pointee_ty: &TypeSpec,
        ptr: PointerValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let llvm_pointee_ty = self
            .convert_type_spec(pointee_ty)
            .expect("can not load unit type");

        let load = self
            .builder
            .build_load(llvm_pointee_ty, ptr, "load")
            .expect("failed to build load");

        // there's currently a bug where i64/u64 loads don't have the correct alignment. We
        // explicitly set the alignment here to compensate
        if matches!(pointee_ty, TypeSpec::I64 | TypeSpec::U64 | TypeSpec::F64) {
            load.as_instruction_value()
                .expect("failed to get load instruction")
                .set_alignment(8)
                .expect("failed to set 8 widht aligment");
        }

        load.as_basic_value_enum()
    }

    pub fn build_extract_payload(
        &self,
        enum_type: &TypeSpec,
        ptr: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let pointee_ty = self
            .convert_type_spec(enum_type)
            .expect("can not access field on unit type");

        self.builder
            .build_struct_gep(pointee_ty, ptr, 1, "ext_pay")
            .expect("failed to build struct gep")
    }

    pub fn build_extract_tag(&self, struct_value: StructValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_extract_value(struct_value, 0, "ext_tag")
            .expect("failed to build extract value")
    }

    pub fn build_insert_payload(
        &self,
        struct_value: StructValue<'ctx>,
        payload_value: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_insert_value(struct_value, payload_value, 1, "set_pay")
            .expect("failed to build insert value")
            .as_basic_value_enum()
    }

    pub fn build_insert_tag(
        &self,
        struct_value: StructValue<'ctx>,
        tag_value: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_insert_value(struct_value, tag_value, 0, "set_tag")
            .expect("failed to build insert value")
            .as_basic_value_enum()
    }

    pub fn build_insert_struct_field(
        &self,
        struct_value: StructValue<'ctx>,
        value: BasicValueEnum<'ctx>,
        index: usize,
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_insert_value(struct_value, value, index as u32, "set_field")
            .expect("failed to build insert value")
            .as_basic_value_enum()
    }

    pub fn build_store(
        &self,
        pointee_typ: &TypeSpec,
        ptr: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) {
        let store = self
            .builder
            .build_store(ptr, value)
            .expect("failed to build store instruction");

        if matches!(pointee_typ, TypeSpec::I64 | TypeSpec::U64 | TypeSpec::F64) {
            store
                .set_alignment(8)
                .expect("failed to set 8 widht aligment");
        }
    }

    pub fn build_gep(
        &self,
        pointee_ty: &TypeSpec,
        ptr: PointerValue<'ctx>,
        indexes: &[IntValue<'ctx>],
    ) -> PointerValue<'ctx> {
        let pointee_ty = self
            .convert_type_spec(pointee_ty)
            .expect("can not gep unit types");

        unsafe {
            self.builder
                .build_gep(pointee_ty, ptr, indexes, "gep")
                .expect("failed to build gep")
        }
    }

    pub fn build_struct_gep(
        &self,
        pointee_ty: &TypeSpec,
        ptr: PointerValue<'ctx>,
        field: usize,
    ) -> PointerValue<'ctx> {
        let pointee_ty = self
            .convert_type_spec(pointee_ty)
            .expect("can not access field on unit type");

        self.builder
            .build_struct_gep(pointee_ty, ptr, field as u32, "struct_gep")
            .expect("failed to build struct gep")
    }

    pub fn build_value_call(
        &self,
        function_name: &str,
        function: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let value = self
            .builder
            .build_call(function, args, function_name)
            .expect("failed to build call");

        value
            .try_as_basic_value()
            .expect_basic("failed to get return value as basic value")
    }

    pub fn build_void_call(
        &self,
        function_name: &str,
        function: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) {
        self.builder
            .build_call(function, args, function_name)
            .expect("failed to build call");
    }

    pub fn build_xor(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_xor(lhs, rhs, "xor")
            .expect("failed to build bitwise xor")
            .into()
    }

    pub fn build_not(&self, op: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_not(op, "not")
            .expect("failed to build logical not")
            .into()
    }

    pub fn build_int_neg(&self, op: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_int_neg(op, "ineg")
            .expect("failed to build integer negation")
            .into()
    }

    pub fn build_float_neg(&self, op: FloatValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder
            .build_float_neg(op, "fneg")
            .expect("failed to build integer negation")
            .into()
    }
}

pub fn convert_type_spec<'ctx>(
    context: &'ctx Context,
    type_spec: &TypeSpec,
) -> Option<BasicTypeEnum<'ctx>> {
    if matches!(type_spec, TypeSpec::Unit) {
        return None;
    }

    let ts = match type_spec {
        TypeSpec::I8 => context.i8_type().into(),
        TypeSpec::I16 => context.i16_type().into(),
        TypeSpec::I32 => context.i32_type().into(),
        TypeSpec::I64 => context.i64_type().into(),

        // LLVM has no unsigned; signedness is in ops so we just use the basic int types here
        TypeSpec::U8 => context.i8_type().into(),
        TypeSpec::U16 => context.i16_type().into(),
        TypeSpec::U32 => context.i32_type().into(),
        TypeSpec::U64 => context.i64_type().into(),

        TypeSpec::F32 => context.f32_type().into(),
        TypeSpec::F64 => context.f64_type().into(),
        TypeSpec::Bool => context.bool_type().into(),
        TypeSpec::Ptr(_) | TypeSpec::OpaquePtr => context.ptr_type(AddressSpace::default()).into(),
        TypeSpec::Array { elem, len } => convert_type_spec(context, elem)
            .expect("can not get array of unit types")
            .array_type(*len as u32)
            .into(),
        TypeSpec::Struct(fields) => {
            let field_types: Vec<_> = fields
                .iter()
                .map(|f| convert_type_spec(context, f).expect("can not have a field of unit type"))
                .collect();
            context.struct_type(&field_types, false).into()
        }
        TypeSpec::String => {
            let len_type = context.i64_type();
            let ptr_type = context.ptr_type(AddressSpace::default());
            context
                .struct_type(&[len_type.into(), ptr_type.into()], false)
                .into()
        }
        TypeSpec::Enum { tag_size, variants } => {
            let tag_type = match tag_size {
                TagSize::U8 => context.i8_type(),
                TagSize::U16 => context.i16_type(),
                TagSize::U32 => context.i32_type(),
                TagSize::U64 => context.i64_type(),
            };

            let mut max_size = 0;
            for payload in variants {
                // TODO: need to actually respect the target arch width
                let layout = blocker::type_layout(payload, Arch::W64);
                if layout.size() > max_size {
                    max_size = layout.size()
                }
            }

            let raw_data_type = context.i8_type().array_type(max_size as u32);

            context
                .struct_type(&[tag_type.into(), raw_data_type.into()], false)
                .into()
        }
        TypeSpec::Slice(_) => context
            .struct_type(
                &[
                    context.i64_type().into(),
                    context.i64_type().into(),
                    context.ptr_type(AddressSpace::default()).into(),
                ],
                false,
            )
            .into(),
        ts => todo!("failed to gen type for type spec: {:?}", ts),
    };

    Some(ts)
}

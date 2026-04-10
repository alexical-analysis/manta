use std::collections::{BTreeMap, HashMap};

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue,
    InstructionValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use crate::blocker::{self, Arch};
use crate::mir::{
    self, BlockId, ConstValue, Instruction, LocalId, MirFunction, SwitchArm, TagSize, TypeSpec,
    ValueId,
};
use crate::str_store::StrStore;

pub fn build_func_value<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    str_store: &StrStore,
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
    let llvm_function = module.add_function(function_name.as_str(), func_type, None);

    llvm_function
}

pub struct FuncBuilder<'ctx, 'a> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    mir_function: &'a MirFunction,
    llvm_function: FunctionValue<'ctx>,
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

        FuncBuilder {
            context,
            builder,
            mir_function: function,
            llvm_function,
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

    pub fn position_at_block(&self, block_id: BlockId) {
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

    pub fn convert_type_spec(&self, type_spec: &TypeSpec) -> Option<BasicTypeEnum<'ctx>> {
        convert_type_spec(self.context, type_spec)
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

    fn convert_const(
        &self,
        const_value: &ConstValue,
        type_spec: &TypeSpec,
    ) -> BasicValueEnum<'ctx> {
        match (const_value, type_spec) {
            (ConstValue::ConstInt(i), TypeSpec::I8) => {
                let value = self.context.i8_type().const_int(*i as u64, false);
                value.into()
            }
            (ConstValue::ConstInt(i), TypeSpec::I16) => {
                let value = self.context.i16_type().const_int(*i as u64, false);
                value.into()
            }
            (ConstValue::ConstInt(i), TypeSpec::I32) => {
                let value = self.context.i32_type().const_int(*i as u64, false);
                value.into()
            }
            (ConstValue::ConstInt(i), TypeSpec::I64) => {
                let value = self.context.i64_type().const_int(*i as u64, false);
                value.into()
            }
            (ConstValue::ConstUInt(i), TypeSpec::U8) => {
                let value = self.context.i8_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstUInt(i), TypeSpec::U16) => {
                let value = self.context.i16_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstUInt(i), TypeSpec::U32) => {
                let value = self.context.i32_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstUInt(i), TypeSpec::U64) => {
                let value = self.context.i64_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstFloat(f), TypeSpec::F32) => {
                let value = self.context.f32_type().const_float(*f);
                value.into()
            }
            (ConstValue::ConstFloat(f), TypeSpec::F64) => {
                let value = self.context.f64_type().const_float(*f);
                value.into()
            }
            (ConstValue::ConstBool(b), TypeSpec::Bool) => {
                let value = match b {
                    true => self.context.bool_type().const_int(1, false),
                    false => self.context.bool_type().const_zero(),
                };
                value.into()
            }
            (ConstValue::ConstArray(values), TypeSpec::Array { elem, .. }) => {
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
    pub fn build_unreachable(&self) -> InstructionValue<'ctx> {
        self.builder
            .build_unreachable()
            .expect("failed to build unreachable terminator")
    }

    pub fn build_value_return(&self, value_id: &ValueId) -> InstructionValue<'ctx> {
        match self.value_map.get(&value_id) {
            Some(v) => {
                let value: Option<&dyn BasicValue<'ctx>> = Some(v);
                self.builder
                    .build_return(value)
                    .expect("failed to build value return terminator")
            }
            None => {
                eprintln!("not all values are being calculated yet, skipping for now");
                self.build_unreachable()
            }
        }
    }

    pub fn build_void_return(&self) -> InstructionValue<'ctx> {
        self.builder
            .build_return(None)
            .expect("failed to build void return terminator")
    }

    pub fn build_unconditional_branch(&self, target: &BlockId) -> InstructionValue<'ctx> {
        let dest = self
            .block_map
            .get(target)
            .expect("failed to get target block");
        self.builder
            .build_unconditional_branch(*dest)
            .expect("failed to build unconditional branch terminator")
    }

    pub fn build_conditional_branch(
        &self,
        cond: &ValueId,
        then_block: &BlockId,
        else_block: &BlockId,
    ) -> InstructionValue<'ctx> {
        let cond = self.value_map.get(cond);
        let cond = match cond {
            Some(v) => v.into_int_value(),
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
            .expect("failed to build conditional branch terminator")
    }

    pub fn build_switch(
        &self,
        discriminant: &ValueId,
        default_block: &BlockId,
        arms: &Vec<SwitchArm>,
    ) -> InstructionValue<'ctx> {
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
            .expect("failed to build switch terminator")
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
            .build_struct_gep(pointee_ty, ptr, field as u32, "struce_gep")
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

fn convert_type_spec<'ctx>(
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
            for payload in variants.iter().flatten() {
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

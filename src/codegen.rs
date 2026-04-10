mod builder;
pub mod optimizer;

use std::collections::BTreeMap;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::TargetMachine;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, GlobalValue, PointerValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use crate::blocker::{self, Arch};
use crate::mir::{
    self, ConstValue, GlobalId, Instruction, MirModule, Place, PlaceBase, Projection, TagSize,
    Terminator, TypeSpec, ValueId,
};
use crate::str_store::StrStore;

use builder::FuncBuilder;

struct GlobalData<'ctx> {
    global_value: GlobalValue<'ctx>,
    type_spec: TypeSpec,
}

pub struct Codegen<'ctx, 'str> {
    str_store: &'str StrStore,
    context: &'ctx Context,
    module_name: String,
    global_map: BTreeMap<GlobalId, GlobalData<'ctx>>,
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
            self.global_map.insert(
                global_id,
                GlobalData {
                    global_value,
                    type_spec: global.type_spec.clone(),
                },
            );
        }

        // code gen the init function
        let mut func_builder = FuncBuilder::new(
            self.context,
            &llvm_module,
            &builder,
            &module.init,
            self.str_store,
        );
        self.gen_function(&mut func_builder);

        // code gen the remaining functions
        for function in module.functions {
            let mut func_builder = FuncBuilder::new(
                self.context,
                &llvm_module,
                &builder,
                &function,
                self.str_store,
            );
            self.gen_function(&mut func_builder);
        }

        if let Err(e) = llvm_module.verify() {
            eprintln!("module verification failed: {}", e);
        }

        llvm_module
    }

    pub fn optimize_module(&self, target_machine: &TargetMachine, module: &Module<'ctx>) {
        let passes: &[&str] = &[
            "instcombine",
            "reassociate",
            "gvn",
            "simplifycfg",
            "mem2reg",
        ];

        module
            .run_passes(
                passes.join(",").as_str(),
                &target_machine,
                PassBuilderOptions::create(),
            )
            .expect("failed to optimize module")
    }

    fn gen_function<'a>(&self, func_builder: &mut FuncBuilder<'ctx, 'a>) {
        for block in func_builder.get_blocks() {
            self.gen_block(func_builder, block)
        }
    }

    // TODO: we're getting a lot of params here (6), that probably means we should try to bundle
    // some of these together and expose only the functionality that we need to simplify the code
    fn gen_block<'a>(&self, func_builder: &mut FuncBuilder<'ctx, 'a>, block: &mir::BasicBlock) {
        for value_id in &block.instructions {
            match self.gen_inst(func_builder, *value_id) {
                Some(v) => func_builder.insert_value(*value_id, v),
                None => {
                    eprintln!("TODO: not all instructions are supported")
                }
            }
        }

        match &block.terminator {
            Terminator::Return { value } => match value {
                Some(v) => func_builder.build_value_return(v),
                None => func_builder.build_void_return(),
            },
            Terminator::Jump { target } => func_builder.build_unconditional_branch(target),
            Terminator::Branch {
                cond,
                true_target,
                false_target,
            } => func_builder.build_conditional_branch(cond, true_target, false_target),
            Terminator::SwitchVariant {
                discriminant,
                default,
                arms,
            } => func_builder.build_switch(discriminant, default, arms),
            Terminator::Unreachable => func_builder.build_unreachable(),
            Terminator::Panic => func_builder.build_unreachable(),
        };
    }

    fn gen_inst<'a>(
        &self,
        func_builder: &mut FuncBuilder<'ctx, 'a>,
        value_id: ValueId,
    ) -> Option<BasicValueEnum<'ctx>> {
        let inst = func_builder.get_inst(value_id).clone();
        let type_spec = func_builder.get_value_type(value_id);

        match inst {
            Instruction::Const { value } => {
                let value = self.gen_const(&value, type_spec);
                Some(value)
            }
            Instruction::Add { lhs, rhs } => match type_spec {
                TypeSpec::I8
                | TypeSpec::I16
                | TypeSpec::I32
                | TypeSpec::I64
                | TypeSpec::U8
                | TypeSpec::U16
                | TypeSpec::U32
                | TypeSpec::U64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_int_add(lhs, rhs);
                    Some(value)
                }
                TypeSpec::F32 | TypeSpec::F64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_float_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_float_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_float_add(lhs, rhs);
                    Some(value)
                }
                TypeSpec::String => todo!("concatenating strings is not yet supported"),
                _ => panic!("unsupported arguments for addition"),
            },
            Instruction::Sub { lhs, rhs } => match type_spec {
                TypeSpec::I8
                | TypeSpec::I16
                | TypeSpec::I32
                | TypeSpec::I64
                | TypeSpec::U8
                | TypeSpec::U16
                | TypeSpec::U32
                | TypeSpec::U64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_int_sub(lhs, rhs);
                    Some(value)
                }
                TypeSpec::F32 | TypeSpec::F64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_float_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_float_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_float_sub(lhs, rhs);
                    Some(value)
                }
                _ => panic!("unsupported arguments for addition"),
            },
            Instruction::SDiv { lhs, rhs } => match type_spec {
                TypeSpec::I8 | TypeSpec::I16 | TypeSpec::I32 | TypeSpec::I64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_int_signed_div(lhs, rhs);
                    Some(value)
                }
                TypeSpec::F32 | TypeSpec::F64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_float_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_float_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_float_div(lhs, rhs);
                    Some(value)
                }
                _ => panic!("unsupported arguments for signed division"),
            },
            Instruction::UDiv { lhs, rhs } => match type_spec {
                TypeSpec::U8 | TypeSpec::U16 | TypeSpec::U32 | TypeSpec::U64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_int_unsigned_div(lhs, rhs);
                    Some(value)
                }
                _ => panic!("unsupported arguments for unsigned division"),
            },
            Instruction::Mul { lhs, rhs } => match type_spec {
                TypeSpec::I8
                | TypeSpec::I16
                | TypeSpec::I32
                | TypeSpec::I64
                | TypeSpec::U8
                | TypeSpec::U16
                | TypeSpec::U32
                | TypeSpec::U64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_int_mul(lhs, rhs);
                    Some(value)
                }
                TypeSpec::F32 | TypeSpec::F64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_float_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_float_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_float_mul(lhs, rhs);
                    Some(value)
                }
                _ => panic!("unsupported arguments for multiplication"),
            },
            Instruction::SMod { lhs, rhs } => match type_spec {
                TypeSpec::I8 | TypeSpec::I16 | TypeSpec::I32 | TypeSpec::I64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_int_signed_rem(lhs, rhs);
                    Some(value)
                }
                _ => panic!("unsupported arguments for signed modulous"),
            },
            Instruction::UMod { lhs, rhs } => match type_spec {
                TypeSpec::U8 | TypeSpec::U16 | TypeSpec::U32 | TypeSpec::U64 => {
                    let lhs = func_builder.get_llvm_value(lhs);
                    let lhs = match lhs {
                        Some(lhs) => lhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let rhs = func_builder.get_llvm_value(rhs);
                    let rhs = match rhs {
                        Some(rhs) => rhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_int_unsigned_rem(lhs, rhs);
                    Some(value)
                }
                _ => panic!("unsupported arguments for unsigned modulous"),
            },
            Instruction::Equal { lhs, rhs } => {
                let type_spec = func_builder.get_value_type(lhs);
                match type_spec {
                    TypeSpec::Bool
                    | TypeSpec::I8
                    | TypeSpec::I16
                    | TypeSpec::I32
                    | TypeSpec::I64
                    | TypeSpec::U8
                    | TypeSpec::U16
                    | TypeSpec::U32
                    | TypeSpec::U64 => {
                        self.gen_int_compare(func_builder, IntPredicate::EQ, lhs, rhs)
                    }
                    TypeSpec::F32 | TypeSpec::F64 => {
                        self.gen_float_compare(func_builder, FloatPredicate::OEQ, lhs, rhs)
                    }
                    TypeSpec::Ptr(_) => todo!("pointer comparison is not yet supported"),
                    TypeSpec::OpaquePtr => todo!("opaque pointer comparison is not yet supported"),
                    TypeSpec::Array { .. } => todo!("array comparison is not yet supported"),
                    TypeSpec::String => todo!(),
                    TypeSpec::Slice { .. } => todo!(),
                    TypeSpec::Struct(_) => todo!(),
                    TypeSpec::Enum { .. } => todo!(),
                    TypeSpec::Unit => panic!("can not compare unit types"),
                }
            }
            Instruction::NotEqual { lhs, rhs } => {
                let type_spec = func_builder.get_value_type(lhs);
                match type_spec {
                    TypeSpec::Bool
                    | TypeSpec::I8
                    | TypeSpec::I16
                    | TypeSpec::I32
                    | TypeSpec::I64
                    | TypeSpec::U8
                    | TypeSpec::U16
                    | TypeSpec::U32
                    | TypeSpec::U64 => {
                        self.gen_int_compare(func_builder, IntPredicate::NE, lhs, rhs)
                    }
                    TypeSpec::F32 | TypeSpec::F64 => {
                        self.gen_float_compare(func_builder, FloatPredicate::ONE, lhs, rhs)
                    }
                    TypeSpec::Ptr(_) => todo!("pointer comparison is not yet supported"),
                    TypeSpec::OpaquePtr => todo!("opaque pointer comparison is not yet supported"),
                    TypeSpec::Array { .. } => todo!("array comparison is not yet supported"),
                    TypeSpec::String => todo!(),
                    TypeSpec::Slice { .. } => todo!(),
                    TypeSpec::Struct(_) => todo!(),
                    TypeSpec::Enum { .. } => todo!(),
                    TypeSpec::Unit => panic!("can not compare unit types"),
                }
            }
            Instruction::SLessThan { lhs, rhs } => {
                let type_spec = func_builder.get_value_type(lhs);
                match type_spec {
                    TypeSpec::I8 | TypeSpec::I16 | TypeSpec::I32 | TypeSpec::I64 => {
                        self.gen_int_compare(func_builder, IntPredicate::SLT, lhs, rhs)
                    }
                    TypeSpec::F32 | TypeSpec::F64 => {
                        self.gen_float_compare(func_builder, FloatPredicate::OLT, lhs, rhs)
                    }
                    _ => panic!("unsupported args for signed less than"),
                }
            }
            Instruction::ULessThan { lhs, rhs } => {
                let type_spec = func_builder.get_value_type(lhs);
                match type_spec {
                    TypeSpec::Bool
                    | TypeSpec::U8
                    | TypeSpec::U16
                    | TypeSpec::U32
                    | TypeSpec::U64 => {
                        self.gen_int_compare(func_builder, IntPredicate::ULT, lhs, rhs)
                    }
                    _ => panic!("unsupported args for unsigned less than"),
                }
            }
            Instruction::SGreaterThan { lhs, rhs } => {
                let type_spec = func_builder.get_value_type(lhs);
                match type_spec {
                    TypeSpec::I8 | TypeSpec::I16 | TypeSpec::I32 | TypeSpec::I64 => {
                        self.gen_int_compare(func_builder, IntPredicate::SGT, lhs, rhs)
                    }
                    TypeSpec::F32 | TypeSpec::F64 => {
                        self.gen_float_compare(func_builder, FloatPredicate::OGT, lhs, rhs)
                    }
                    _ => {
                        let lhs = func_builder.get_value_type(lhs);
                        let rhs = func_builder.get_value_type(rhs);
                        panic!(
                            "unsupported args for signed greater than ts:{:?} lhs:{:?} rhs:{:?}",
                            type_spec, lhs, rhs
                        )
                    }
                }
            }
            Instruction::UGreaterThan { lhs, rhs } => {
                let type_spec = func_builder.get_value_type(lhs);
                match type_spec {
                    TypeSpec::Bool
                    | TypeSpec::U8
                    | TypeSpec::U16
                    | TypeSpec::U32
                    | TypeSpec::U64 => {
                        self.gen_int_compare(func_builder, IntPredicate::UGT, lhs, rhs)
                    }
                    _ => panic!("unsupported args for unsigned greater than"),
                }
            }
            Instruction::SLessThanEqual { lhs, rhs } => {
                let type_spec = func_builder.get_value_type(lhs);
                match type_spec {
                    TypeSpec::I8 | TypeSpec::I16 | TypeSpec::I32 | TypeSpec::I64 => {
                        self.gen_int_compare(func_builder, IntPredicate::SLE, lhs, rhs)
                    }
                    TypeSpec::F32 | TypeSpec::F64 => {
                        self.gen_float_compare(func_builder, FloatPredicate::OLE, lhs, rhs)
                    }
                    _ => panic!("unsupported args for signed less than or equal"),
                }
            }
            Instruction::ULessThanEqual { lhs, rhs } => {
                let type_spec = func_builder.get_value_type(lhs);
                match type_spec {
                    TypeSpec::Bool
                    | TypeSpec::U8
                    | TypeSpec::U16
                    | TypeSpec::U32
                    | TypeSpec::U64 => {
                        self.gen_int_compare(func_builder, IntPredicate::ULE, lhs, rhs)
                    }
                    _ => panic!("unsupported args for unsigned less than or equal"),
                }
            }
            Instruction::SGreaterThanEqual { lhs, rhs } => {
                let type_spec = func_builder.get_value_type(lhs);
                match type_spec {
                    TypeSpec::I8 | TypeSpec::I16 | TypeSpec::I32 | TypeSpec::I64 => {
                        self.gen_int_compare(func_builder, IntPredicate::SGE, lhs, rhs)
                    }
                    TypeSpec::F32 | TypeSpec::F64 => {
                        self.gen_float_compare(func_builder, FloatPredicate::OGE, lhs, rhs)
                    }
                    _ => panic!("unsupported args for signed greater than or equal"),
                }
            }
            Instruction::UGreaterThanEqual { lhs, rhs } => {
                let type_spec = func_builder.get_value_type(lhs);
                match type_spec {
                    TypeSpec::Bool
                    | TypeSpec::U8
                    | TypeSpec::U16
                    | TypeSpec::U32
                    | TypeSpec::U64 => {
                        self.gen_int_compare(func_builder, IntPredicate::UGE, lhs, rhs)
                    }
                    _ => panic!("unsupported args for unsigned greater than or equal"),
                }
            }
            Instruction::LogicalAnd { lhs, rhs } => {
                let lhs = func_builder.get_llvm_value(lhs);
                let lhs = match lhs {
                    Some(lhs) => lhs.into_int_value(),
                    None => {
                        eprintln!("TODO: not all instructions are supported yet");
                        return None;
                    }
                };

                let rhs = func_builder.get_llvm_value(rhs);
                let rhs = match rhs {
                    Some(rhs) => rhs.into_int_value(),
                    None => {
                        eprintln!("TODO: not all instructions are supported yet");
                        return None;
                    }
                };

                let value = func_builder.build_and(lhs, rhs);
                Some(value)
            }
            Instruction::LogicalOr { lhs, rhs } => {
                let lhs = func_builder.get_llvm_value(lhs);
                let lhs = match lhs {
                    Some(lhs) => lhs.into_int_value(),
                    None => {
                        eprintln!("TODO: not all instructions are supported yet");
                        return None;
                    }
                };

                let rhs = func_builder.get_llvm_value(rhs);
                let rhs = match rhs {
                    Some(rhs) => rhs.into_int_value(),
                    None => {
                        eprintln!("TODO: not all instructions are supported yet");
                        return None;
                    }
                };

                let value = func_builder.build_or(lhs, rhs);
                Some(value)
            }
            Instruction::Load { place } => {
                let (ptr, current_type) = match place.base {
                    PlaceBase::Local(local_id) => {
                        let ptr = func_builder.get_local_ptr(local_id).clone();
                        let local_type_spec = func_builder.get_local_type_spec(local_id).clone();
                        self.gen_place_ptr(func_builder, place, ptr, local_type_spec)
                    }
                    PlaceBase::Global(global_id) => {
                        let global_data = self
                            .global_map
                            .get(&global_id)
                            .expect("failed to find global");
                        let ptr = global_data.global_value.as_pointer_value();
                        self.gen_place_ptr(func_builder, place, ptr, global_data.type_spec.clone())
                    }
                };

                let load = func_builder.build_load(&current_type, ptr);
                Some(load)
            }
            Instruction::Store { place, value } => {
                let (ptr, current_type) = match place.base {
                    PlaceBase::Local(local_id) => {
                        let ptr = func_builder.get_local_ptr(local_id);
                        let local_type_spec = func_builder.get_local_type_spec(local_id).clone();
                        self.gen_place_ptr(func_builder, place, *ptr, local_type_spec)
                    }
                    PlaceBase::Global(global_id) => {
                        let global_data = self
                            .global_map
                            .get(&global_id)
                            .expect("failed to find global");
                        let ptr = global_data.global_value.as_pointer_value();
                        self.gen_place_ptr(func_builder, place, ptr, global_data.type_spec.clone())
                    }
                };

                let value = func_builder.get_llvm_value(value);
                let value = match value {
                    Some(v) => v,
                    None => {
                        eprintln!(
                            "TODO: not all instructions are supported yet skipping store for now"
                        );
                        return None;
                    }
                };

                func_builder.build_store(&current_type, ptr, *value);
                None
            }
            _ => None,
        }
    }

    fn gen_place_ptr<'a>(
        &self,
        func_builder: &mut FuncBuilder<'ctx, 'a>,
        place: Place,
        ptr: PointerValue<'ctx>,
        type_spec: TypeSpec,
    ) -> (PointerValue<'ctx>, TypeSpec) {
        let mut ptr = ptr;
        let mut current_type = type_spec;
        for proj in &place.projections {
            match proj {
                Projection::Deref => {
                    ptr = func_builder
                        .build_load(&current_type, ptr)
                        .into_pointer_value();

                    current_type = match current_type {
                        TypeSpec::Ptr(ts) => *ts,
                        _ => panic!("can not dereference non-pointer type"),
                    };
                }
                Projection::Field(field) => {
                    ptr = func_builder.build_struct_gep(&current_type, ptr, *field);

                    current_type = match current_type {
                        TypeSpec::Struct(ts) => ts[*field].clone(),
                        _ => panic!("can not access field on non-struct type"),
                    };
                }
                Projection::Index(idx) => {
                    let idx_value = func_builder
                        .get_llvm_value(*idx)
                        .expect("missing index value");
                    let idx_value = idx_value.into_int_value();

                    // use zero to dereference the local ptr first and then index
                    // into the array memory
                    let zero = self.context.i64_type().const_zero();
                    ptr = func_builder.build_gep(&current_type, ptr, &[zero, idx_value]);

                    current_type = match current_type {
                        // TODO: need to add a check here using len to ensure that
                        // access is not out of bound and panic if it is
                        TypeSpec::Array { elem, .. } => *elem,
                        _ => panic!("can not index array type"),
                    };
                }
            }
        }

        (ptr, current_type)
    }

    fn gen_int_compare<'a>(
        &self,
        func_builder: &mut FuncBuilder<'ctx, 'a>,
        op: IntPredicate,
        lhs: ValueId,
        rhs: ValueId,
    ) -> Option<BasicValueEnum<'ctx>> {
        let lhs = func_builder.get_llvm_value(lhs);
        let lhs = match lhs {
            Some(lhs) => lhs.into_int_value(),
            None => {
                eprintln!("TODO: not all instructions are supported");
                return None;
            }
        };

        let rhs = func_builder.get_llvm_value(rhs);
        let rhs = match rhs {
            Some(rhs) => rhs.into_int_value(),
            None => {
                eprintln!("TODO: not all instructions are supported");
                return None;
            }
        };

        let value = func_builder.build_int_compare(op, lhs, rhs);
        Some(value)
    }

    fn gen_float_compare<'a>(
        &self,
        func_builder: &mut FuncBuilder<'ctx, 'a>,
        op: FloatPredicate,
        lhs: ValueId,
        rhs: ValueId,
    ) -> Option<BasicValueEnum<'ctx>> {
        let lhs = func_builder.get_llvm_value(lhs);
        let lhs = match lhs {
            Some(lhs) => lhs.into_float_value(),
            None => {
                eprintln!("TODO: not all instructions are supported");
                return None;
            }
        };

        let rhs = func_builder.get_llvm_value(rhs);
        let rhs = match rhs {
            Some(rhs) => rhs.into_float_value(),
            None => {
                eprintln!("TODO: not all instructions are supported");
                return None;
            }
        };

        let value = func_builder.build_float_compare(op, lhs, rhs);
        Some(value)
    }

    fn gen_const(&self, const_value: &ConstValue, type_spec: &TypeSpec) -> BasicValueEnum<'ctx> {
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
                    let const_value = self.gen_const(value, elem);
                    basic_values.push(const_value);
                }

                let gen_type = self
                    .gen_type_spec(elem)
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

mod builder;
pub mod optimizer;

use std::collections::BTreeMap;

use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::TargetMachine;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FunctionValue, GlobalValue, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};

use crate::blocker::{self, Arch};
use crate::mir::{
    self, ConstValue, GlobalId, Instruction, MirModule, Place, PlaceBase, Projection, TagSize,
    Terminator, TypeSpec, ValueId,
};
use crate::str_store::{self, StrID, StrStore};

use builder::FuncBuilder;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
struct GlobalString<'ctx> {
    ptr: PointerValue<'ctx>,
    len: IntValue<'ctx>,
}

struct GlobalData<'ctx> {
    global_value: GlobalValue<'ctx>,
    type_spec: TypeSpec,
}

struct FuncData<'ctx> {
    function: FunctionValue<'ctx>,
    return_type: TypeSpec,
}

pub struct Codegen<'ctx, 'str> {
    str_store: &'str StrStore,
    context: &'ctx Context,
    module_name: String,
    global_map: BTreeMap<GlobalId, GlobalData<'ctx>>,
    global_strings: BTreeMap<StrID, GlobalString<'ctx>>,
    function_map: BTreeMap<StrID, FuncData<'ctx>>,
}

impl<'ctx, 'str> Codegen<'ctx, 'str> {
    pub fn new(str_store: &'str StrStore, context: &'ctx Context, module_name: String) -> Self {
        Codegen {
            str_store,
            context,
            module_name,
            global_map: BTreeMap::new(),
            global_strings: BTreeMap::new(),
            function_map: BTreeMap::new(),
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
            let global_type = match self.convert_type_spec(&global.type_spec) {
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

        // add all the functions to the module
        let init_func =
            builder::build_func_value(self.context, &llvm_module, self.str_store, &module.init);
        self.function_map.insert(
            module.init.name,
            FuncData {
                function: init_func,
                return_type: module.init.return_type.clone(),
            },
        );

        for func in &module.functions {
            let func_value =
                builder::build_func_value(self.context, &llvm_module, self.str_store, func);
            self.function_map.insert(
                func.name,
                FuncData {
                    function: func_value,
                    return_type: func.return_type.clone(),
                },
            );
        }

        // add c-runtime dependencies to the module
        let c_funcs = builder::build_c_deps(self.context, &llvm_module);

        // build manta module level functions that are built on top of the c-runtime, we need to
        // replace the bulitin placeholders which is why this comes after we've already built all
        // the module functions
        self.function_map.insert(
            str_store::FREE,
            FuncData {
                function: *c_funcs.get("free").expect("failed to get free() c dep"),
                return_type: TypeSpec::Unit,
            },
        );

        let panic_fn = builder::build_manta_panic(
            self.context,
            &builder,
            &llvm_module,
            *c_funcs.get("puts").expect("failed to get puts() c dep"),
            *c_funcs.get("abort").expect("failed to get abort() c dep"),
        );
        self.function_map.insert(
            str_store::PANIC,
            FuncData {
                function: panic_fn,
                return_type: TypeSpec::Unit,
            },
        );

        let alloc_fn = builder::build_manta_alloc(
            self.context,
            &builder,
            &llvm_module,
            *c_funcs.get("malloc").expect("failed to get malloc() c dep"),
        );
        self.function_map.insert(
            str_store::ALLOC,
            FuncData {
                function: alloc_fn,
                return_type: TypeSpec::OpaquePtr,
            },
        );

        let print_fn = builder::build_manta_print(
            self.context,
            &builder,
            &llvm_module,
            *c_funcs.get("write").expect("failed to get write() c dep"),
        );
        self.function_map.insert(
            str_store::PRINT,
            FuncData {
                function: print_fn,
                return_type: TypeSpec::Unit,
            },
        );

        let eprint_fn = builder::build_manta_eprint(
            self.context,
            &builder,
            &llvm_module,
            *c_funcs.get("write").expect("failed to get write() c dep"),
        );
        self.function_map.insert(
            str_store::EPRINT,
            FuncData {
                function: eprint_fn,
                return_type: TypeSpec::Unit,
            },
        );

        // code gen the init function
        let mut func_builder = FuncBuilder::new(
            self.context,
            &builder,
            &module.init,
            init_func,
            self.str_store,
        );
        self.gen_function(&llvm_module, &mut func_builder);

        // code gen the remaining functions
        for func in module.functions {
            let func_value = self
                .function_map
                .get(&func.name)
                .expect("failed to get named function");
            let mut func_builder = FuncBuilder::new(
                self.context,
                &builder,
                &func,
                func_value.function,
                self.str_store,
            );
            self.gen_function(&llvm_module, &mut func_builder);
        }

        if let Err(e) = llvm_module.verify() {
            panic!("module verification failed: {}", e);
        }

        llvm_module
    }

    pub fn optimize_module(&self, target_machine: &TargetMachine, module: &Module<'ctx>) {
        let func_passes: &[&str] = &[
            "instcombine",
            "reassociate",
            "gvn",
            "simplifycfg",
            "mem2reg",
        ];
        let func_passes = func_passes.join(",");
        let func_passes = format!("function({})", func_passes);

        let mod_passes: &[&str] = &["globaldce"];
        let mod_passes = mod_passes.join(",");

        module
            .run_passes(
                [func_passes, mod_passes].join(",").as_str(),
                target_machine,
                PassBuilderOptions::create(),
            )
            .expect("failed to optimize module")
    }

    fn convert_type_spec(&self, type_spec: &TypeSpec) -> Option<BasicTypeEnum<'ctx>> {
        builder::convert_type_spec(self.context, type_spec)
    }

    fn gen_function<'a>(
        &mut self,
        module: &Module<'ctx>,
        func_builder: &mut FuncBuilder<'ctx, 'a>,
    ) {
        for (block_id, block) in func_builder.get_blocks() {
            self.gen_block(module, func_builder, block_id, block)
        }
    }

    // TODO: we're getting a lot of params here (6), that probably means we should try to bundle
    // some of these together and expose only the functionality that we need to simplify the code
    fn gen_block<'a>(
        &mut self,
        module: &Module<'ctx>,
        func_builder: &mut FuncBuilder<'ctx, 'a>,
        block_id: mir::BlockId,
        block: &mir::BasicBlock,
    ) {
        func_builder.position_at_block(block_id);
        for value_id in &block.instructions {
            match self.gen_inst(module, func_builder, *value_id) {
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
            Terminator::Panic { .. } => {
                let panic_data = self
                    .function_map
                    .get(&str_store::PANIC)
                    .expect("failed to get panic function");
                let panic_fn = panic_data.function;

                func_builder.build_panic(panic_fn)
            }
        };
    }

    fn gen_inst<'a>(
        &mut self,
        module: &Module<'ctx>,
        func_builder: &mut FuncBuilder<'ctx, 'a>,
        value_id: ValueId,
    ) -> Option<BasicValueEnum<'ctx>> {
        let inst = func_builder.get_inst(value_id).clone();
        let inst_type_spec = func_builder.get_value_type(value_id);

        match inst {
            Instruction::Const { value } => {
                let value = self.gen_const(module, &value, inst_type_spec);
                Some(value)
            }
            Instruction::Add { lhs, rhs } => match inst_type_spec {
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
            Instruction::Sub { lhs, rhs } => match inst_type_spec {
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
            Instruction::SDiv { lhs, rhs } => match inst_type_spec {
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
            Instruction::UDiv { lhs, rhs } => match inst_type_spec {
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
            Instruction::Mul { lhs, rhs } => match inst_type_spec {
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
            Instruction::SMod { lhs, rhs } => match inst_type_spec {
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
            Instruction::UMod { lhs, rhs } => match inst_type_spec {
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
            Instruction::AddressOf { place } => {
                let ptr = match place.base {
                    PlaceBase::Local(local_id) => {
                        let ptr = func_builder.get_local_ptr(local_id);
                        let local_type_spec = func_builder.get_local_type_spec(local_id).clone();
                        let (ptr, _) =
                            self.gen_place_ptr(func_builder, place, *ptr, local_type_spec);
                        ptr
                    }
                    PlaceBase::Global(global_id) => {
                        let global_data = self
                            .global_map
                            .get(&global_id)
                            .expect("failed to find global");
                        let ptr = global_data.global_value.as_pointer_value();
                        let (ptr, _) = self.gen_place_ptr(
                            func_builder,
                            place,
                            ptr,
                            global_data.type_spec.clone(),
                        );
                        ptr
                    }
                };

                Some(ptr.into())
            }
            Instruction::Call { func, args } => {
                let mut llvm_args = vec![];
                for arg in args {
                    let llvm_value = func_builder.get_llvm_value(arg);
                    let llvm_value = match llvm_value {
                        Some(v) => *v,
                        None => {
                            eprintln!("TODO: not all instructions are supported yet");
                            return None;
                        }
                    };

                    let llvm_value = llvm_value.into();
                    llvm_args.push(llvm_value)
                }

                let func_data = self
                    .function_map
                    .get(&func)
                    .expect("failed to get function by name");

                let func_name = self
                    .str_store
                    .get_string(func)
                    .expect("failed to get function name as string");
                let func_name = func_name.as_str();

                match func_data.return_type {
                    TypeSpec::Unit => {
                        func_builder.build_void_call(func_name, func_data.function, &llvm_args);
                        None
                    }
                    _ => {
                        let value = func_builder.build_value_call(
                            func_name,
                            func_data.function,
                            &llvm_args,
                        );

                        Some(value)
                    }
                }
            }
            Instruction::CallTry { .. } => todo!("call_try instructions are not yet supported"),
            Instruction::VariantGetPayload { src } => {
                // need to do this first to relase func_builder from the borrow
                let payload_ts = inst_type_spec.clone();

                let (ptr, enum_type) = match src.base {
                    PlaceBase::Local(local_id) => {
                        let ptr = *func_builder.get_local_ptr(local_id);
                        let ts = func_builder.get_local_type_spec(local_id).clone();
                        self.gen_place_ptr(func_builder, src, ptr, ts)
                    }
                    PlaceBase::Global(global_id) => {
                        let global_data = self
                            .global_map
                            .get(&global_id)
                            .expect("failed to find global for variant_get_payload");
                        let ptr = global_data.global_value.as_pointer_value();
                        self.gen_place_ptr(func_builder, src, ptr, global_data.type_spec.clone())
                    }
                };

                let data_ptr = func_builder.build_extract_payload(&enum_type, ptr);
                let value = func_builder.build_load(&payload_ts, data_ptr);

                Some(value)
            }
            Instruction::VariantGetTag { src } => {
                let src_value = func_builder
                    .get_llvm_value(src)
                    .expect("src value not found for variant_get_tag")
                    .into_struct_value();

                let value = func_builder.build_extract_tag(src_value);
                Some(value)
            }
            Instruction::MakeVariant { tag, payload } => {
                // clone here so the func_builder borrow ends
                let variant_type = inst_type_spec.clone();

                // start with an poison struct with the correct struct type
                let struct_type = self
                    .convert_type_spec(&variant_type)
                    .expect("can not have struct of unit type")
                    .into_struct_type();
                let poison_struct = struct_type.get_undef();

                let (tag_ts, payload_ts) = match &variant_type {
                    TypeSpec::Enum { tag_size, variants } => {
                        let tag_ts = match &tag_size {
                            TagSize::U8 => TypeSpec::U8,
                            TagSize::U16 => TypeSpec::U16,
                            TagSize::U32 => TypeSpec::U32,
                            TagSize::U64 => TypeSpec::U64,
                        };

                        let payload_ts = match tag {
                            ConstValue::ConstInt(i) => variants
                                .get(i as usize)
                                .expect("failed to get tagged variant")
                                .clone(),
                            _ => panic!("invalid tag value, must be uint"),
                        };

                        // TODO: we can remove this once we update the enum type spec to use Unit
                        // types instead of optional types
                        let payload_ts = match payload_ts {
                            Some(t) => t.clone(),
                            None => TypeSpec::Unit,
                        };

                        (tag_ts, payload_ts)
                    }
                    _ => panic!("type must be an enum"),
                };

                let opaque_ts = match &variant_type {
                    TypeSpec::Enum { variants, .. } => {
                        let mut bytes = 0;
                        for ts in variants.iter().flatten() {
                            // TODO: actually repsect the correct arch here
                            let layout = blocker::type_layout(ts, Arch::W64);
                            if layout.size() > bytes {
                                bytes = layout.size()
                            }
                        }

                        TypeSpec::Array {
                            elem: Box::new(TypeSpec::U8),
                            len: bytes as usize,
                        }
                    }
                    _ => panic!("type must be an enum"),
                };

                let const_tag = self.gen_const(module, &tag, &tag_ts).into_int_value();
                let result = func_builder.build_insert_tag(poison_struct, const_tag);

                let payload = match payload {
                    Some(p) => p,
                    None => return Some(result),
                };

                let tmp_local = func_builder.build_alloca(&opaque_ts, "tmp");
                let payload = func_builder
                    .get_llvm_value(payload)
                    .expect("failed to get enum payload");

                func_builder.build_store(&payload_ts, tmp_local, *payload);

                let payload = func_builder.build_load(&opaque_ts, tmp_local);

                let result_struct = result.into_struct_value();
                let result = func_builder.build_insert_payload(result_struct, payload);

                Some(result)
            }
            Instruction::BitwiseAnd { lhs, rhs } => match inst_type_spec {
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

                    let value = func_builder.build_and(lhs, rhs);
                    Some(value)
                }
                _ => panic!("invalid type for bitwise and"),
            },
            Instruction::BitwiseOr { lhs, rhs } => match inst_type_spec {
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

                    let value = func_builder.build_or(lhs, rhs);
                    Some(value)
                }
                _ => panic!("invalid type for bitwise or"),
            },
            Instruction::BitwiseXOr { lhs, rhs } => match inst_type_spec {
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

                    let value = func_builder.build_xor(lhs, rhs);
                    Some(value)
                }
                _ => panic!("invalid type for bitwise xor"),
            },
            Instruction::BoolNot { op } => match inst_type_spec {
                TypeSpec::Bool => {
                    let op = func_builder.get_llvm_value(op);
                    let op = match op {
                        Some(lhs) => lhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_not(op);
                    Some(value)
                }
                _ => panic!("invalid type for boolean not"),
            },
            Instruction::Negate { op } => match inst_type_spec {
                TypeSpec::I8 | TypeSpec::I16 | TypeSpec::I32 | TypeSpec::I64 => {
                    let op = func_builder.get_llvm_value(op);
                    let op = match op {
                        Some(lhs) => lhs.into_int_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_int_neg(op);
                    Some(value)
                }
                TypeSpec::F32 | TypeSpec::F64 => {
                    let op = func_builder.get_llvm_value(op);
                    let op = match op {
                        Some(lhs) => lhs.into_float_value(),
                        None => {
                            eprintln!("TODO: not all instructions are supported");
                            return None;
                        }
                    };

                    let value = func_builder.build_float_neg(op);
                    Some(value)
                }
                _ => panic!("invalid type for value negation"),
            },
            Instruction::Alloc { meta_type } => {
                let alloc_data = self
                    .function_map
                    .get(&str_store::ALLOC)
                    .expect("failed to get alloc function");
                let alloc_fn = alloc_data.function;

                let meta_value = *func_builder
                    .get_llvm_value(meta_type)
                    .expect("failed to find meta value");

                let value = func_builder.build_value_call("alloc", alloc_fn, &[meta_value.into()]);
                Some(value)
            }
            Instruction::Free { ptr } => {
                let free_data = self
                    .function_map
                    .get(&str_store::FREE)
                    .expect("failed to get free function");
                let free_fn = free_data.function;

                let ptr = func_builder
                    .get_llvm_value(ptr)
                    .expect("failed to get pointer value");
                let ptr = ptr.into_pointer_value();

                func_builder.build_void_call("free", free_fn, &[ptr.into()]);
                None
            }
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

    fn gen_const(
        &mut self,
        module: &Module<'ctx>,
        const_value: &ConstValue,
        type_spec: &TypeSpec,
    ) -> BasicValueEnum<'ctx> {
        match (const_value, type_spec) {
            (ConstValue::ConstInt(i), TypeSpec::I8) => {
                let value = self.context.i8_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstInt(i), TypeSpec::I16) => {
                let value = self.context.i16_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstInt(i), TypeSpec::I32) => {
                let value = self.context.i32_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstInt(i), TypeSpec::I64) => {
                let value = self.context.i64_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstInt(i), TypeSpec::U8) => {
                let value = self.context.i8_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstInt(i), TypeSpec::U16) => {
                let value = self.context.i16_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstInt(i), TypeSpec::U32) => {
                let value = self.context.i32_type().const_int(*i, false);
                value.into()
            }
            (ConstValue::ConstInt(i), TypeSpec::U64) => {
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
                    let const_value = self.gen_const(module, value, elem);
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
            (ConstValue::ConstStruct(values), TypeSpec::Struct(ts)) => {
                let mut fields = vec![];
                for (v, ts) in values.iter().zip(ts.iter()) {
                    let field_const = self.gen_const(module, v, ts);
                    fields.push(field_const);
                }

                let value = self.context.const_struct(&fields, false);
                value.into()
            }
            (ConstValue::ConstString(s), TypeSpec::String) => {
                let global_str = match self.global_strings.get(s) {
                    Some(global) => *global,
                    None => {
                        let const_str = self
                            .str_store
                            .get_string(*s)
                            .expect("failed to get string data");
                        let bytes = const_str.as_bytes();

                        let str_bytes = self.context.const_string(bytes, false);
                        let array_type = self.context.i8_type().array_type(bytes.len() as u32);
                        let global_bytes = module.add_global(array_type, None, "const_str");
                        global_bytes.set_initializer(&str_bytes);
                        global_bytes.set_constant(true);
                        global_bytes.set_linkage(Linkage::Private);

                        let ptr = global_bytes.as_pointer_value();
                        let len = self.context.i64_type().const_int(bytes.len() as u64, false);

                        self.global_strings.insert(*s, GlobalString { ptr, len });
                        GlobalString { ptr, len }
                    }
                };

                let str_struct = self
                    .context
                    .const_struct(&[global_str.len.into(), global_str.ptr.into()], false);
                str_struct.into()
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

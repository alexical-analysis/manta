use std::fs;
use std::process::Command;

use crate::blocker::Blocker;
use crate::noder::node_module;
use crate::parser::Parser;
use crate::str_store::StrStore;

use inkwell::context::Context;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::values::BasicValue;
use inkwell::{AddressSpace, OptimizationLevel, module};

pub struct Compiler {
    _source: String,
}

impl Compiler {
    pub fn new(source: String) -> Self {
        Compiler { _source: source }
    }

    pub fn compile(&self) {
        // TODO: this should actually be created in the parse_module call and
        // be owned by the module itself
        let mut str_store = StrStore::new();

        let parser = Parser::new("mod main".to_string());
        let module = parser.parse_module(&mut str_store);
        if !module.get_errors().is_empty() {
            panic!("errors in the parser: {:?}", module.get_errors())
        }

        // build the HIR from the AST
        let node_tree = node_module(module);

        // build the MIR from the HIR
        let blocker = Blocker::new(&node_tree);
        let _mir_module = blocker.build_module();

        // TODO: build the LLVM IR from the MIR

        // TODO: generate a .o file using llvm

        // TODO: run the system linker to generate a final binary file

        // TODO: print any errors the were encountered during compilation
    }

    pub fn compile_example(&self) {
        let context = Context::create();
        let builder = context.create_builder();
        let module = context.create_module("test");

        let i64_type = context.i64_type();

        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = module.add_function("add", fn_type, None);

        let basic_block = context.append_basic_block(function, "entry");
        builder.position_at_end(basic_block);

        let x = function
            .get_nth_param(0)
            .expect("failed to get param 0")
            .into_int_value();
        let y = function
            .get_nth_param(1)
            .expect("failed to get param 1")
            .into_int_value();
        let z = function
            .get_nth_param(2)
            .expect("failed to get param 2")
            .into_int_value();

        let sum = builder
            .build_int_add(x, y, "sum")
            .expect("failed to add x and y");
        let sum = builder
            .build_int_add(sum, z, "sum")
            .expect("failed to add sum and z");

        builder
            .build_return(Some(&sum))
            .expect("failed to build return");

        // --- finish function add ---

        let fn_value = module.get_last_function().expect("missing function add");

        let fn_type = i64_type.fn_type(&[], false);
        let main_fn = module.add_function("main", fn_type, None);

        let basic_block = context.append_basic_block(main_fn, "entry");
        builder.position_at_end(basic_block);

        let const_a = context.i64_type().const_int(10, false);
        let const_b = context.i64_type().const_int(30, false);
        let const_c = context.i64_type().const_int(2, false);

        let result = builder
            .build_call(
                fn_value,
                &[const_a.into(), const_b.into(), const_c.into()],
                "add",
            )
            .expect("failed to call add");

        let ret_value = result
            .try_as_basic_value()
            .expect_basic("expecting basic value return");

        builder
            .build_return(Some(&ret_value))
            .expect("failed to build return");

        module.verify().expect("the module is wrong");

        module
            .print_to_file("test_module.ll")
            .expect("failed to print to file");

        Target::initialize_native(&InitializationConfig::default()).unwrap();

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let machine = target
            .create_target_machine(
                &target_triple,
                "generic", // or TargetMachine::get_host_cpu_name()
                "",        // features
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        machine
            .write_to_file(&module, FileType::Object, "test_module.o".as_ref())
            .unwrap();
    }

    pub fn compile_hello_world(&self) {
        let context = Context::create();
        let builder = context.create_builder();
        let module = context.create_module("hello");

        let ptr_type = context.ptr_type(AddressSpace::default());
        let i32_type = context.i32_type();

        let puts_type = i32_type.fn_type(&[ptr_type.into()], false);
        let puts_fn = module.add_function("puts", puts_type, Some(module::Linkage::External));

        let main_type = i32_type.fn_type(&[], false);
        let main_fn = module.add_function("main", main_type, None);

        let basic_block = context.append_basic_block(main_fn, "entry");
        builder.position_at_end(basic_block);

        let hello = builder
            .build_global_string_ptr("Hello World!", "hello_str")
            .expect("failed to build global string");
        let result = builder
            .build_call(puts_fn, &[hello.as_pointer_value().into()], "call_puts")
            .expect("failed to call puts");

        let ret_value = result
            .try_as_basic_value()
            .expect_basic("failed to get return value");

        builder
            .build_return(Some(&ret_value))
            .expect("failed to build return");

        module.verify().expect("the module is wrong");

        module
            .print_to_file("hello_module.ll")
            .expect("failed to print to file");

        Target::initialize_native(&InitializationConfig::default()).unwrap();

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let machine = target
            .create_target_machine(
                &target_triple,
                "generic", // or TargetMachine::get_host_cpu_name()
                "",        // features
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        machine
            .write_to_file(&module, FileType::Object, "hello_module.o".as_ref())
            .unwrap();
    }

    pub fn compile_for_loop(&self) {
        let context = Context::create();
        let builder = context.create_builder();
        let module = context.create_module("hello");

        let ptr_type = context.ptr_type(AddressSpace::default());
        let i64_type = context.i64_type();
        let f64_type = context.f64_type();
        let f32_type = context.f32_type();

        let avg_type = f64_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
        let avg_fn = module.add_function("average", avg_type, None);

        let array = avg_fn.get_nth_param(0).expect("failed to get array param");
        let array = array.into_pointer_value();

        let count = avg_fn.get_nth_param(1).expect("failed to get count param");
        let count = count.into_int_value();

        let basic_block = context.append_basic_block(avg_fn, "entry");
        let loop_block = context.append_basic_block(avg_fn, "loop");
        let ret_block = context.append_basic_block(avg_fn, "ret");

        // Start by building the entry block

        builder.position_at_end(basic_block);
        let sum_local = builder
            .build_alloca(f64_type, "sum")
            .expect("failed to build local sum");
        let zero_float = f64_type.const_zero();
        builder
            .build_store(sum_local, zero_float)
            .expect("failed to store value for sum");

        let zero_int = i64_type.const_zero();
        let i_local = builder
            .build_alloca(i64_type, "i")
            .expect("failed to build local i");

        let store = builder
            .build_store(i_local, zero_int)
            .expect("failed to store value for i");
        store.set_alignment(8).expect("set align");

        let count_is_zero = builder
            .build_int_compare(inkwell::IntPredicate::EQ, count, zero_int, "count_is_zero")
            .expect("failed to build comparison");
        builder
            .build_conditional_branch(count_is_zero, ret_block, loop_block)
            .expect("failed to branch");

        // Now build the loop block that we'll jump through multiple times

        builder.position_at_end(loop_block);
        let sum_val = builder
            .build_load(f64_type, sum_local, "sum")
            .expect("failed to load sum")
            .into_float_value();
        let i_val = builder
            .build_load(i64_type, i_local, "i")
            .expect("failed t load i");
        i_val
            .as_instruction_value()
            .expect("failed to get load instruction")
            .set_alignment(8)
            .expect("failed to set alignment");
        let i_val = i_val.into_int_value();

        let elem_ptr = unsafe {
            builder
                .build_gep(f32_type, array, &[i_val], "elem_ptr")
                .expect("failed to get arrray element")
        };
        let elem_val = builder
            .build_load(f32_type, elem_ptr, "elem")
            .expect("failed to get elem value")
            .into_float_value();

        let elem_64_val = builder
            .build_float_ext(elem_val, f64_type, "elem_64_val")
            .expect("failed to extend float");
        let new_sum = builder
            .build_float_add(sum_val, elem_64_val, "new_sum")
            .expect("failed to add values");
        builder
            .build_store(sum_local, new_sum)
            .expect("failed to store new sum value");

        let new_i = builder
            .build_int_add(i_val, i64_type.const_int(1, false), "i_plus_one")
            .expect("failed to incriment i");
        let store = builder
            .build_store(i_local, new_i)
            .expect("failed to re-store i");
        store.set_alignment(8).expect("set align");

        let loop_is_done = builder
            .build_int_compare(inkwell::IntPredicate::UGE, new_i, count, "loop_is_done")
            .expect("failed to build comparison");

        builder
            .build_conditional_branch(loop_is_done, ret_block, loop_block)
            .expect("failed to branch after loop");

        // finally build the return block and return from the function

        builder.position_at_end(ret_block);
        let float_count = builder
            .build_unsigned_int_to_float(count, f64_type, "float_count")
            .expect("failed to conver count to float");
        let sum_value = builder
            .build_load(f64_type, sum_local, "sum_value")
            .expect("failed to load from sum");
        let ret_value = builder
            .build_float_div(sum_value.into_float_value(), float_count, "ret")
            .expect("failed to compute return value");
        builder
            .build_return(Some(&ret_value))
            .expect("failed to build return");

        // Build the main function where we build the arary of floats and call the average function

        let main_type = i64_type.fn_type(&[], false);
        let main_fn = module.add_function("main", main_type, None);

        let basic_block = context.append_basic_block(main_fn, "entry");
        builder.position_at_end(basic_block);

        let values = [
            f32_type.const_float(1.),
            f32_type.const_float(8.),
            f32_type.const_float(42.),
        ];
        let len = i64_type.const_int(values.len() as u64, false);

        let float_array = f32_type.const_array(&values);
        let global = module.add_global(float_array.get_type(), None, "float_array");
        global.set_initializer(&float_array);
        global.set_constant(true);

        let array_ptr = global.as_pointer_value();
        let result = builder
            .build_call(avg_fn, &[array_ptr.into(), len.into()], "avg")
            .expect("failed to make call");

        let ret = result
            .try_as_basic_value()
            .expect_basic("failed to get avg return")
            .into_float_value();
        let ret = builder
            .build_float_to_unsigned_int(ret, i64_type, "ret_cast")
            .expect("failed to cast float to int");

        builder
            .build_return(Some(&ret))
            .expect("failed to return average");

        // validate the module and then create the .ll file for review

        module.verify().expect("failed to verify module");
        module
            .print_to_file("loop_unoptimized.ll")
            .expect("failed to print to file");

        // now run the optimizations and re-print the file so we can see how things change

        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

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
            .expect("failed to optimize module");

        module.verify().expect("failed to verify module");
        module
            .print_to_file("loop_optimize.ll")
            .expect("failed to print to file");

        let machine = target
            .create_target_machine(
                &target_triple,
                "generic", // or TargetMachine::get_host_cpu_name()
                "",        // features
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("failed to get target machine");

        let tmp = env::temp_dir();
        let obj_path = tmp.join("manta_out.o");
        let str_path = obj_path.to_str().expect("failed to get path to obj file");

        machine
            .write_to_file(&module, FileType::Object, &obj_path)
            .expect("failed to write .o file");

        // finally, link that .o file using the "system linker" for mac
        let output = Command::new("clang")
            .args([str_path, "-o", "loop"])
            .output()
            .expect("failed to invoke clang");

        println!("linker output: {:?}", output);

        fs::remove_file(&obj_path).ok(); // clean up, ignore error
    }
}

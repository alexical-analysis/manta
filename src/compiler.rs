use std::fs;
use std::process::Command;

use crate::blocker::Blocker;
use crate::codegen::Codegen;
use crate::codegen::optimizer;
use crate::noder::node_module;
use crate::parser::Parser;
use crate::str_store::StrStore;

use inkwell::context::Context;
use inkwell::targets::{FileType, TargetMachine};

pub struct Compiler {
    source: String,
    output_file: String,
}

impl Compiler {
    pub fn new(source: String, output_file: String) -> Self {
        Compiler {
            source,
            output_file,
        }
    }

    pub fn compile(&self) {
        // TODO: this should actually be created in the parse_module call and
        // be owned by the module itself
        let mut str_store = StrStore::new();

        println!("building ast module...");
        let parser = Parser::new(self.source.clone());
        let module = parser.parse_module(&mut str_store);
        let module_name = module.get_name(&str_store);
        if !module.get_errors().is_empty() {
            panic!("errors in the parser: {:?}", module.get_errors())
        }

        // build the HIR from the AST
        println!("building hir module...");
        let node_tree = node_module(module);

        // build the MIR from the HIR
        println!("building mir module...");
        let blocker = Blocker::new(&node_tree);
        let mir_module = blocker.build_module();

        // build the llvm module from the MIR
        println!("building llvm module...");
        let context = Context::create();
        let mut generator = Codegen::new(&str_store, &context, module_name);
        let llvm_module = generator.gen_module(mir_module);

        // optimize the llvm_module
        println!("optimizing llvm module...");
        let target_triple = TargetMachine::get_default_triple();
        let target_machine = optimizer::create_target_machine(target_triple);
        generator.optimize_module(&target_machine, &llvm_module);

        // create the object file in a tmp dir
        let tmp_dir = env::temp_dir();
        let out_object_file = self.output_file.clone() + ".o";
        let obj_path = tmp_dir.join(out_object_file.as_str());

        println!("writing object file to tmp dir {:?}...", obj_path);
        target_machine
            .write_to_file(&llvm_module, FileType::Object, obj_path.as_path())
            .expect("failed to write object file to tmp dir");

        let obj_path = obj_path.to_str().expect("failed to get path to obj file");

        // run clang (eventually this will be lld but we're just using clang for now)
        println!(
            "running clang to link {:?} (output {:?})",
            obj_path, self.output_file
        );
        let output = Command::new("clang")
            .args([obj_path, "-o", self.output_file.as_str()])
            .output()
            .expect("failed to invoke clang");

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            panic!("clang failed:\n{}", stderr);
        }

        // clean up the temporary object file, ignore error since it is in a tmp dir
        fs::remove_file(obj_path).ok();
    }
}

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

use crate::blocker::Blocker;
use crate::codegen::Codegen;
use crate::codegen::optimizer;
use crate::file_set::FileSet;
use crate::noder::node_module;
use crate::parser::Parser;
use crate::pub_mod::PubMod;
use crate::str_store::StrStore;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{FileType, TargetMachine};

pub struct Compiler<'fs> {
    file_set: &'fs FileSet,
}

impl<'fs> Compiler<'fs> {
    pub fn new(file_set: &'fs FileSet) -> Self {
        Compiler { file_set }
    }

    /// Compiles the source to a binary at `output_file`. When `debug` is true, the intermediate
    /// object file is written to the current directory and not cleaned up; otherwise it is written
    /// to a temp directory and removed after linking.
    pub fn compile(&mut self, output_file: String, debug: bool) -> Result<(), String> {
        let context = Context::create();
        let mut generator = Codegen::new(&context);

        let (module, _) = self.compile_module(&mut generator);

        let target_triple = TargetMachine::get_default_triple();
        let target_machine = optimizer::create_target_machine(target_triple);

        println!("optimizing llvm module...");
        generator.optimize_module(&target_machine, &module);

        let obj_dir = match debug {
            true => match env::current_dir() {
                Ok(path) => path,
                Err(err) => return Err(format!("failed to get current directory: {}", err)),
            },
            false => env::temp_dir(),
        };

        let obj_path = obj_dir.join(format!("{}.o", output_file));

        let result = self.link_module(&target_machine, &module, &obj_path, &output_file);

        if !debug {
            fs::remove_file(&obj_path).ok();
        }

        result.map_err(|err| format!("failed to link module: {}", err))
    }

    pub fn check(&mut self) {
        let context = Context::create();
        let mut generator = Codegen::new(&context);

        self.compile_module(&mut generator);
    }

    fn compile_module<'ctx>(&mut self, generator: &mut Codegen<'ctx>) -> (Module<'ctx>, PubMod) {
        println!("building ast module...");
        let parser = Parser::new(self.file_set);
        let mut str_store = StrStore::new();
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

        let pub_mod = PubMod::new(str_store.clone(), node_tree.get_public_types());

        // build the llvm module from the MIR
        println!("building llvm module...");
        let llvm_module = generator.gen_module(&str_store, module_name, mir_module);

        (llvm_module, pub_mod)
    }

    /// links the module by writing the .o file to the object_file and then calling the system
    /// linker to produce a final binary which will be written to output_file
    fn link_module(
        &self,
        target_machine: &TargetMachine,
        module: &Module,
        object_file: &PathBuf,
        output_file: &String,
    ) -> Result<(), String> {
        println!("writing object file to dir {:?}...", object_file);
        target_machine
            .write_to_file(module, FileType::Object, object_file.as_path())
            .expect("failed to write object file to tmp dir");

        let obj_path = object_file
            .to_str()
            .expect("failed to get path to obj file");

        // run clang (eventually this will be lld but we're just using clang for now)
        println!(
            "running cc to link {:?} (output {:?})",
            obj_path, output_file
        );
        let output = Command::new("cc")
            .args([obj_path, "-o", output_file.as_str()])
            .output()
            .expect("failed to invoke cc");

        match output.status.success() {
            true => Ok(()),
            false => {
                let stderr = String::from_utf8_lossy(&output.stderr);
                Err(format!("clang failed:\n{}", stderr).to_string())
            }
        }
    }
}

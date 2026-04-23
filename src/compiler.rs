use std::collections::HashMap;
use std::error::Error;
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

/// Compiler compiles a single module into a .o file and returns the public interface to that module
pub struct Compiler<'fs> {
    file_set: &'fs FileSet,
}

impl<'fs> Compiler<'fs> {
    pub fn new(file_set: &'fs FileSet) -> Self {
        Compiler { file_set }
    }

    /// Compiles the source to a .o file and write the result to the object_file path.
    pub fn compile(
        &mut self,
        mod_map: &HashMap<String, PubMod>,
        object_file: &PathBuf,
    ) -> Result<PubMod, Box<dyn Error>> {
        let context = Context::create();
        let mut generator = Codegen::new(&context);

        let (module, pub_mod) = self.compile_module(mod_map, &mut generator);

        let target_triple = TargetMachine::get_default_triple();
        let target_machine = optimizer::create_target_machine(target_triple);

        println!("optimizing llvm module...");
        generator.optimize_module(&target_machine, &module);

        println!("writing object file to dir {:?}...", object_file);
        target_machine.write_to_file(&module, FileType::Object, object_file.as_path())?;

        Ok(pub_mod)
    }

    pub fn check(&mut self, mod_map: &HashMap<String, PubMod>) {
        let context = Context::create();
        let mut generator = Codegen::new(&context);

        self.compile_module(mod_map, &mut generator);
    }

    fn compile_module<'ctx>(
        &mut self,
        mod_map: &HashMap<String, PubMod>,
        generator: &mut Codegen<'ctx>,
    ) -> (Module<'ctx>, PubMod) {
        println!("building ast module...");
        let parser = Parser::new(self.file_set);
        let mut str_store = StrStore::new();
        let module = parser.parse_module(&mut str_store);

        if !module.get_errors().is_empty() {
            panic!("errors in the parser: {:?}", module.get_errors())
        }

        // build the HIR from the AST
        println!("building hir module...");
        let node_tree = node_module(&module);

        // build the MIR from the HIR
        println!("building mir module...");
        let blocker = Blocker::new(&node_tree);
        let mir_module = blocker.build_module();

        let pub_mod = PubMod::new(
            str_store.clone(),
            module.name(),
            node_tree.get_public_types(),
        );

        // build the llvm module from the MIR
        println!("building llvm module...");
        let module_name = module.get_name(&str_store);
        let llvm_module = generator.gen_module(&str_store, module_name, mir_module);

        (llvm_module, pub_mod)
    }
}

/// uses the linker to link the given object file writing the result to the output file
pub fn link_module(object_files: &[PathBuf], output_file: &String) -> Result<(), Box<dyn Error>> {
    let mut object_files: Vec<&str> = object_files
        .iter()
        .map(|p| p.to_str().expect("failed to get object file path"))
        .collect();

    // run clang (eventually this will be lld but we're just using clang for now)
    println!(
        "running cc to link {:?} (output {:?})",
        object_files, output_file
    );

    let mut args = vec!["-o", output_file.as_str()];
    args.append(&mut object_files);
    let output = Command::new("cc").args(args).output()?;

    match output.status.success() {
        true => Ok(()),
        false => {
            let stderr = String::from_utf8_lossy(&output.stderr);
            Err(format!("linking failed:\n{}", stderr).to_string().into())
        }
    }
}

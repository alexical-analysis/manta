use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;
use std::process::Command;

use crate::blocker::Blocker;
use crate::codegen::Codegen;
use crate::codegen::optimizer;
use crate::file_set::FileSet;
use crate::noder::Module as HirModule;
use crate::noder::Noder;
use crate::parser::Parser;
use crate::parser::module::Module as ParseModule;
use crate::str_store::StrID;
use crate::str_store::StrStore;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{FileType, TargetMachine};

/// Compiler compiles a single module into a .o file and returns the public interface to that module
pub struct Compiler<'fs> {
    import_path: PathBuf,
    module_name: String,
    file_set: &'fs FileSet,
}

impl<'fs> Compiler<'fs> {
    pub fn new(import_path: PathBuf, module_name: String, file_set: &'fs FileSet) -> Self {
        Compiler {
            import_path,
            module_name,
            file_set,
        }
    }

    /// parse the module associated with this compiler
    pub fn parse(&self, str_store: &mut StrStore) -> Result<ParseModule, Box<dyn Error>> {
        println!("building ast module...");
        let parser = Parser::new(self.file_set);
        let module = parser.parse_module(str_store);

        if !module.get_errors().is_empty() {
            panic!("errors in the parser: {:?}", module.get_errors())
        }

        Ok(module)
    }

    /// Compiles the source to a .o file and write the result to the object_file path.
    pub fn compile(
        &mut self,
        str_store: &StrStore,
        mod_map: &HashMap<StrID, HirModule>,
        module: &ParseModule,
        object_file: &PathBuf,
        save_temps: bool,
    ) -> Result<HirModule, Box<dyn Error>> {
        let context = Context::create();
        let mut generator = Codegen::new(&context);

        let (module, node_tree) = self.pipeline(str_store, mod_map, module, &mut generator);
        if save_temps {
            let mut ll_file = object_file.clone();
            ll_file.set_extension("ll");
            module.print_to_file(ll_file)?;
        }

        let target_triple = TargetMachine::get_default_triple();
        let target_machine = optimizer::create_target_machine(target_triple);

        println!("optimizing llvm module...");
        generator.optimize_module(&target_machine, &module);

        println!("writing object file to dir {:?}...", object_file);
        target_machine.write_to_file(&module, FileType::Object, object_file.as_path())?;

        Ok(node_tree)
    }

    fn pipeline<'ctx>(
        &mut self,
        str_store: &StrStore,
        mod_map: &HashMap<StrID, HirModule>,
        module: &ParseModule,
        generator: &mut Codegen<'ctx>,
    ) -> (Module<'ctx>, HirModule) {
        // build the HIR from the AST
        println!("building hir module...");
        let node_tree = Noder::new().node_module(mod_map, &module);

        // build the MIR from the HIR
        println!("building mir module...");
        let blocker = Blocker::new(&node_tree);
        let mir_module = blocker.build_module();

        // build the llvm module from the MIR
        println!("building llvm module...");
        let llvm_module =
            generator.gen_module(&str_store, &self.module_name, &self.import_path, mir_module);

        (llvm_module, node_tree)
    }
}

/// uses the linker to link the given object file writing the result to the output file
pub fn link_module(object_files: &[PathBuf], output_file: &String) -> Result<(), Box<dyn Error>> {
    let mut object_files: Vec<&str> = object_files
        .iter()
        .map(|p| p.to_str().expect("failed to get object file path"))
        .collect();

    // run cc (eventually this will be lld directly but we're just using cc to making finding lld on
    // the system easier for now)
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

use std::error::Error;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use crate::compiler::context::Context;
use crate::compiler::module::{Module, ModuleDag};

pub struct File {
    path: PathBuf,
    source: String,
}

pub struct Builder<'ctx> {
    ctx: &'ctx mut Context,
}

impl<'ctx> Builder<'ctx> {
    pub fn new(ctx: &'ctx mut Context) -> Self {
        Self { ctx }
    }

    pub fn compile<W: Write>(&mut self, root: &Path, out: &mut W) -> Result<(), Box<dyn Error>> {
        let files = self.get_all_files(root);
        let modules = self.get_all_modules(&files);

        let module_dag = ModuleDag::new(self.ctx, &modules);
        let import_cycles = module_dag.validate(self.ctx);
        if !import_cycles.is_empty() {
            // TODO: need to fold this into an invalid module somehow
            todo!(
                "found import cycles {:?}, Fold this into the ast somehow",
                import_cycles
            )
        }
        for module in module_dag.iter(self.ctx) {
            let mut noder = self.ctx.create_noder(module);
            noder.node();

            // let mut mir_module = self.ctx.get_mir_module(module);
            // let grapher = Grapher::new(&mut mir_module);
            let mut grapher = self.ctx.create_grapher(module);
            grapher.graph();

            // let mut gen_module = self.ctx.get_gen_module(module);
            // let generator = Generator::new(&mut gen_module);
            let mut generator = self.ctx.create_generator(module);
            generator.codegen();
        }

        let object_files = self.ctx.get_object_files();
        self.link_modules(&object_files, out)
    }

    fn get_all_files(&self, root: &Path) -> Vec<File> {
        let mut files = vec![];
        for entry in fs::read_dir(root).expect("failed to read root") {
            let path = match entry {
                Ok(e) => e.path(),
                Err(e) => panic!("failed to get file {:?}", e),
            };

            if path.is_dir() {
                continue;
            }

            let ext = match path.extension() {
                Some(ext) => ext,
                None => continue,
            };

            if ext != "pup" {
                continue;
            }

            let file = match fs::read_to_string(&path) {
                Ok(file) => file,
                Err(e) => panic!("failed to read .pup file {:?}", e),
            };

            files.push(File { path, source: file })
        }

        files
    }

    /// reads, lexes and parses all the files and returns a list of module for each file
    fn get_all_modules(&mut self, files: &[File]) -> Vec<Module> {
        let mut modules = vec![];

        // first parser all the files so we populate the ast and get the modules name
        for file in files {
            let import_path = file.path.to_str().expect("failed to get file path");
            let import_path = self.ctx.get_mstr(&import_path);
            let module = self.ctx.create_module(import_path);
            modules.push(module);

            // let mut ast_module = self.ctx.get_ast_module(module);
            let mut parser = self.ctx.create_parser(module); // Parser::new(&mut ast_module);
            parser.parse(&file.source);
        }

        // now that all the files have been parsed we can get all the dependencies set up
        let module_map = self.ctx.module_map();
        for &module in &modules {
            // create a new parser since it's fairly cheep and that way we don't run into borrow issues
            let mut parser = self.ctx.create_parser(module);
            parser.set_deps(&module_map);
        }

        modules
    }

    /// uses the linker to link the given object file writing the result to the output file
    fn link_modules<W: Write>(
        &self,
        object_files: &[&Path],
        out: &mut W,
    ) -> Result<(), Box<dyn Error>> {
        let mut object_files: Vec<&str> = object_files
            .iter()
            .map(|p| p.to_str().expect("failed to get object file path"))
            .collect();

        // run cc (eventually this will be lld directly but we're just using cc to making finding lld on
        // the system easier for now)
        println!("running cc to link {:?}", object_files);

        // write to stdout so we can use our abitrary out writer
        let mut args = vec!["-o", "-"];
        args.append(&mut object_files);
        let mut cmd = Command::new("cc")
            .args(args)
            .stdout(Stdio::piped())
            .spawn()?;

        if let Some(mut child_stdout) = cmd.stdout.take() {
            io::copy(&mut child_stdout, out)?;
        }

        let status = cmd.wait()?;
        if !status.success() {
            return Err(format!("cc failed with status: {:?}", status).into());
        }

        Ok(())
    }
}

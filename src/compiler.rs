use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::Instant;

use crate::blocker::Blocker;
use crate::codegen::Codegen;
use crate::codegen::optimizer;
use crate::file_set::{self, FileSet};
use crate::noder::Module as HirModule;
use crate::noder::Noder;
use crate::parser::Parser;
use crate::parser::module::Module as ParseModule;
use crate::str_store::{self, StrID, StrStore};

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{FileType, TargetMachine};

// this is a single node in the ModuleDAG
struct Node {
    import_path: StrID,
    deps: Vec<Node>,
}

// this converts the list of all modules in a manta program into a valid DAG
struct ModuleDAG {
    root: Node,
}

// Iterate through a DAG in depth first order
struct DfsIter<'a> {
    stack: Vec<(&'a Node, usize)>,
}

impl<'a> Iterator for DfsIter<'a> {
    type Item = StrID;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (node, child_idx) = self.stack.last_mut()?;
            if *child_idx < node.deps.len() {
                let child = &node.deps[*child_idx];
                *child_idx += 1;
                self.stack.push((child, 0));
            } else {
                let (node, _) = self.stack.pop()?;
                return Some(node.import_path);
            }
        }
    }
}

impl ModuleDAG {
    fn dfs_iter(&self) -> DfsIter<'_> {
        DfsIter {
            stack: vec![(&self.root, 0)],
        }
    }

    fn new(ast_map: &HashMap<StrID, ParseModule>) -> Self {
        let mut visited = HashSet::new();
        let root = Self::build_dag(str_store::EMPTY_STR, &ast_map, &mut visited);

        ModuleDAG { root }
    }

    fn build_dag(
        import_path: StrID,
        ast_map: &HashMap<StrID, ParseModule>,
        visited: &mut HashSet<StrID>,
    ) -> Node {
        if visited.contains(&import_path) {
            // TODO: need to return this as an actual error
            panic!("import cycle detected {:?}", visited);
        }

        visited.insert(import_path);

        let parse_module = ast_map
            .get(&import_path)
            .expect(format!("failed to get parse module {:?}", import_path).as_str());
        let deps = parse_module.get_using_modules();

        let mut dep_nodes = vec![];
        for dep in deps {
            let node = Self::build_dag(*dep, ast_map, visited);
            dep_nodes.push(node)
        }

        visited.remove(&import_path);

        Node {
            import_path,
            deps: dep_nodes,
        }
    }
}

/// compile a full manta program and write the result to the out_file (or the default location). If
/// save_temps is true then the incremental .o and .ll files are saved the the .artifacts dir
pub fn compile_program(
    save_temps: bool,
    target_dir: &Option<PathBuf>,
    out_file: &Option<String>,
) -> Result<(), Box<dyn Error>> {
    let workspace = match target_dir {
        Some(dir) => match dir.is_dir() {
            true => dir.clone(),
            false => return Err(String::from("target dir must be a valid directory").into()),
        },
        None => env::current_dir()?,
    };

    if !workspace.join("manta.mod").exists() {
        return Err(String::from("path does not contain manta.mod file").into());
    }

    let obj_dir = match save_temps {
        true => {
            let artifacts_dir = workspace.join(".artifacts");
            // try to remove existing artifacts to prevent old files from getting linked
            fs::remove_dir_all(&artifacts_dir).ok();
            artifacts_dir
        }
        false => env::temp_dir(),
    };

    let modules = file_set::gather_file_sets(workspace.clone())?;

    let mut line_count = 0;
    let start = Instant::now();

    let mut str_store = StrStore::new();
    let mut ast_map = HashMap::new();
    let mut compiler_map = HashMap::new();

    // first parse all the modules in the project
    for module in &modules {
        line_count += module.line_count();

        let import_path = module.root_dir().strip_prefix(&workspace)?;
        let root = obj_dir.join("root").join(import_path);
        fs::create_dir_all(&root)?;

        let mod_name = root.file_name().expect("failed to get module name");
        let mod_name = mod_name.to_string_lossy().to_string();
        println!("building module {:?}", &mod_name);

        let compiler = Compiler::new(import_path.into(), mod_name.clone(), module);
        let parse_module = compiler
            .parse(&mut str_store)
            .expect("failed to parse module");

        let import_str = import_path.to_string_lossy();
        let import_id = str_store.get_id(&import_str);

        ast_map.insert(import_id, parse_module);
        compiler_map.insert(import_id, compiler);
    }

    let module_dag = ModuleDAG::new(&ast_map);

    let mut mod_map = HashMap::new();
    let mut object_files = vec![];

    // now actually compile all the modules
    for import_path in module_dag.dfs_iter() {
        if mod_map.contains_key(&import_path) {
            // this module was already compiled as part of a different dependency path
            continue;
        }

        let import_str = str_store
            .get_string(import_path)
            .expect("missing import path");
        let root = obj_dir.join("root").join(import_str);

        let mod_name = root.file_name().expect("failed to get module name");
        let mod_name = mod_name.to_string_lossy().to_string();
        println!("building module {:?}", &mod_name);

        let object_file = root.join(mod_name.clone() + ".o");

        let compiler = compiler_map
            .get_mut(&import_path)
            .expect("failed to get compiler");
        let parse_module = ast_map.get(&import_path).expect("failed to get the ast");

        let node_tree = compiler
            .compile(&str_store, &mod_map, parse_module, &object_file, save_temps)
            .expect("failed to compile module");

        mod_map.insert(import_path, node_tree);

        object_files.push(object_file)
    }

    let out_file = match out_file {
        Some(f) => f.clone(),
        None => match workspace.file_name() {
            Some(dir) => dir.to_string_lossy().to_string(),
            None => "main".to_string(),
        },
    };

    link_module(&object_files, &out_file)?;

    if !save_temps {
        for f in object_files {
            // removal is best effort since they're written to a tmp dir
            fs::remove_file(&f).ok();
        }
    }

    let total_time = start.elapsed().as_secs_f64();
    println!("compiled {:?} lines in {:.4}s", line_count, total_time);

    Ok(())
}

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

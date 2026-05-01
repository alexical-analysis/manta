mod ast;
mod blocker;
mod codegen;
mod compiler;
mod file_set;
mod hir;
mod mir;
mod noder;
mod parser;
mod pub_mod;
mod str_store;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;

use clap::{Parser, Subcommand};
use compiler::Compiler;

use crate::parser::module::Module as ParseModule;
use crate::str_store::{StrID, StrStore};

/// The CLI for the Manta programming language
#[derive(Parser, Debug)]
#[command(name = "manta")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    #[command(
        about = "Compile the project and produce build artifacts (use --out-dir to change output)"
    )]
    Build {
        #[arg(short, long, value_name = "OUT_DIR")]
        out_file: Option<String>,

        #[arg(short, long, value_name = "SAVE_TEMPS")]
        save_temps: bool,

        #[arg(value_name = "TARGET_DIR")]
        target_dir: Option<PathBuf>,
    },
    #[command(about = "Init a new manta project in the current directory")]
    Init {
        #[arg(value_name = "MOD_NAME")]
        mod_name: String,
    },
    #[command(
        about = "Run complies the project and immediately executes the resulting artifact (use --out-dir to preserve the compile binary)"
    )]
    Run {
        #[arg(short, long, value_name = "OUT_DIR")]
        out_file: Option<String>,

        #[arg(value_name = "TARGET_DIR")]
        target_dir: Option<PathBuf>,

        #[arg(value_name = "ARGS...")]
        args: Vec<String>,
    },

    #[command(about = "Format the specified source files using the standard manta formatter")]
    Fmt {
        #[arg(short = 'w', long)]
        write: bool,

        #[arg(value_name = "FILES...")]
        inputs: Vec<PathBuf>,
    },
}

struct Node {
    import_path: StrID,
    deps: Vec<Node>,
}

struct ModuleDAG {
    root: Node,
}

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

fn build(
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

    compiler::link_module(&object_files, &out_file)?;

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

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build {
            save_temps,
            target_dir,
            out_file,
        } => {
            build(*save_temps, target_dir, out_file)?;
        }
        Commands::Run {
            out_file,
            target_dir,
            args,
        } => {
            // if out_file is provided, use it and leave it on disk; otherwise write to a temp path
            // and clean it up after running
            let (out_path, cleanup) = match out_file {
                Some(f) => (f.clone(), false),
                None => {
                    let path = env::temp_dir().join("tmp");
                    (
                        path.to_str()
                            .expect("failed to build temp path")
                            .to_string(),
                        true,
                    )
                }
            };

            build(false, target_dir, &Some(out_path.clone()))?;

            let output = Command::new(&out_path)
                .args(args.iter())
                .output()
                .expect("failed to run");

            print!("{}", String::from_utf8_lossy(&output.stdout));
            eprint!("{}", String::from_utf8_lossy(&output.stderr));

            if cleanup {
                fs::remove_file(&out_path).ok();
            }
        }
        Commands::Init { mod_name } => {
            // check if manta.mod already exists before we try to init the project
            if Path::new("manta.mod").exists() {
                println!("manta.mod file already exists");
                return Ok(());
            }

            // TODO: need to actually track the correct semver here once I start using semver to
            // track manta versions
            let contents = format!("manta 0.0.0\n{}\n", mod_name);
            let mut file = File::create("manta.mod")?;
            file.write_all(contents.as_bytes())?;
        }
        Commands::Fmt { .. } => {
            todo!("format command");
        }
    }

    Ok(())
}

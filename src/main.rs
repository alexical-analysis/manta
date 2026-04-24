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

use std::collections::HashMap;
use std::error::Error;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;

use clap::{Parser, Subcommand};
use compiler::Compiler;

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

    let mut mod_map = HashMap::new();
    let mut object_files = vec![];
    for module in &modules {
        line_count += module.line_count();

        let import_path = module.root_dir().strip_prefix(&workspace)?;
        let root = obj_dir.join("root").join(import_path);
        fs::create_dir_all(&root)?;

        let mod_name = root.file_name().expect("failed to get module name");
        let mod_name = mod_name.to_string_lossy().to_string();
        println!("building module {:?}", &mod_name);

        let object_file = root.join(mod_name.clone() + ".o");

        let mut compiler = Compiler::new(import_path.into(), mod_name, module);
        let pub_mod = compiler
            .compile(&mod_map, &object_file, save_temps)
            .expect("failed to compile module");

        mod_map.insert(pub_mod.name(), pub_mod);

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

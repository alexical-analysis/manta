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

use std::error::Error;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use clap::{Parser, Subcommand};

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

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build {
            save_temps,
            target_dir,
            out_file,
        } => {
            compiler::compile_program(*save_temps, target_dir, out_file)?;
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

            compiler::compile_program(false, target_dir, &Some(out_path.clone()))?;

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

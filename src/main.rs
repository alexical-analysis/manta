mod ast;
mod blocker;
mod codegen;
mod compiler;
mod hir;
mod mir;
mod noder;
mod parser;
mod str_store;

use std::error::Error;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;

use compiler::Compiler;

use clap::{Parser, Subcommand};

/// The CLI for the Manta programming language
#[derive(Parser, Debug)]
#[command(name = "manta")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Path to the project or file to operate on (defaults to current directory)
    #[arg(short, long, value_name = "PATH")]
    path: Option<PathBuf>,

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

        #[arg(short, long, value_name = "DEBUG")]
        debug: bool,

        #[arg(value_name = "ARGS...")]
        args: Vec<String>,
    },

    #[command(
        about = "Check complies the project but stops once all checks have been performed and reported"
    )]
    Check {},
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

fn build(debug: bool, in_file: &String, out_file: &Option<String>) {
    if !in_file.ends_with(".manta") {
        panic!("can only compile .manta files")
    }

    let source = fs::read_to_string(in_file).expect("failed to read input file");
    let out_file = match out_file {
        Some(f) => f.clone(),
        None => Path::new(in_file)
            .file_stem()
            .and_then(|s| s.to_str())
            .expect("failed to get output file name")
            .to_string(),
    };

    println!("compiling file {:?}", in_file);
    let line_count = source
        .lines()
        .filter(|l| !l.trim().is_empty())
        .filter(|l| !l.trim_start().starts_with("//"))
        .count();

    let mut compiler = Compiler::new(source);
    let start = Instant::now();

    match compiler.compile(out_file, debug) {
        Ok(_) => {}
        Err(err) => panic!("{}", err),
    }

    let total_time = start.elapsed().as_secs_f64();
    println!("compiled {:?} lines in {:.4}s", line_count, total_time);
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    let workspace = match cli.path {
        Some(p) => p,
        None => env::current_dir()?,
    };

    match &cli.command {
        Commands::Build {
            debug,
            out_file,
            args,
        } => {
            let in_file = match args.first() {
                Some(f) => f,
                None => panic!("missing file to compile"),
            };

            build(*debug, in_file, out_file);
        }
        Commands::Run { out_file, args } => {
            let in_file = match args.first() {
                Some(f) => f,
                None => panic!("missing file to compile"),
            };

            // if out_file is provided, use it and leave it on disk; otherwise write to a temp path
            // derived from the input file stem and clean it up after running
            let (out_path, cleanup) = match out_file {
                Some(f) => (f.clone(), false),
                None => {
                    let stem = Path::new(in_file)
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .expect("failed to get output file name");
                    let path = env::temp_dir().join(stem);
                    (
                        path.to_str()
                            .expect("failed to build temp path")
                            .to_string(),
                        true,
                    )
                }
            };

            build(false, in_file, &Some(out_path.clone()));

            let output = Command::new(&out_path)
                .args(args.iter().skip(1))
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
            let contents = format!("manta 0.0.0\n{}", mod_name);
            let mut file = File::create("manta.mod")?;
            file.write_all(contents.as_bytes())?;
        }
        Commands::Check {} => {
            println!(
                "stub: check -> workspace={:?}, verbose={}",
                workspace, cli.verbose
            );
        }
        Commands::Fmt { write, inputs } => {
            println!(
                "stub: fmt -> workspace={:?}, write={}, inputs={:?}, verbose={}",
                workspace, write, inputs, cli.verbose
            );
        }
    }

    Ok(())
}

mod ast;
mod blocker;
mod codegen;
mod compiler;
mod hir;
mod mir;
mod noder;
mod parser;
mod str_store;

use clap::{Parser, Subcommand};
use std::error::Error;
use std::fs;
use std::path::Path;
use std::path::PathBuf;

use compiler::Compiler;

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

        #[arg(value_name = "ARGS...")]
        args: Vec<String>,
    },

    #[command(
        about = "Check complies the project but stops once all checks have been performed and reported"
    )]
    Check {},

    #[command(about = "Run complies the project and immediately executes the resulting artifact")]
    Run {},

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

    let workspace = match cli.path {
        Some(p) => p,
        None => std::env::current_dir()?,
    };

    match &cli.command {
        Commands::Build { out_file, args } => {
            let in_file = match args.first() {
                Some(f) => f,
                None => panic!("missing file to compile"),
            };

            println!(
                "stub: build -> workspace={:?}, in_file={:?}, out_file={:?}, args={:?}, verbose={}",
                workspace, in_file, out_file, args, cli.verbose
            );

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
            let compiler = Compiler::new(source, out_file);
            compiler.compile();
        }
        Commands::Check {} => {
            println!(
                "stub: check -> workspace={:?}, verbose={}",
                workspace, cli.verbose
            );
        }
        Commands::Run {} => {
            println!(
                "stub: run -> workspace={:?}, verbose={}",
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

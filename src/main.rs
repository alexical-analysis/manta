mod ast;
mod parser;

use clap::{Parser, Subcommand};
use std::error::Error;
use std::path::PathBuf;

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
        out_dir: Option<PathBuf>,

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
        Commands::Build { out_dir, args } => {
            println!(
                "stub: build -> workspace={:?}, out_dir={:?}, args={:?}, verbose={}",
                workspace, out_dir, args, cli.verbose
            );
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

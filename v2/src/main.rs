use std::error::Error;
use std::path::PathBuf;

use clap::{Parser, Subcommand};

/// The CLI for the Manta programming language
#[derive(Parser)]
#[command(name = "manta")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long)]
    verbose: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Complile the project and product build artifacts include a complied binary")]
    Build {
        #[arg(short, long, value_name = "OUT_DIR")]
        out_file: Option<String>,

        #[arg(short, long, value_name = "SAVE_TEMPS")]
        save_temps: bool,

        #[arg(value_name = "TARGET_DIR")]
        target_dir: Option<PathBuf>,
    },

    #[command(about = "Initalize a new Manta project in the current directory")]
    Init {
        #[arg(value_name = "PROJECT_NAME")]
        project_name: String,
    },

    #[command(about = "Compiles the project into a temp dir and immediately executes the result")]
    Run {
        #[arg(short, long, value_name = "OUT_DIR")]
        out_file: Option<String>,

        #[arg(value_name = "TARGET_DIR")]
        target_dir: Option<PathBuf>,

        #[arg(value_name = "ARGS...")]
        args: Vec<String>,
    },
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build {
            out_file,
            save_temps,
            target_dir,
        } => {
            println!("Build...")
        }
        Commands::Run {
            out_file,
            target_dir,
            args,
        } => {
            println!("Run...")
        }
        Commands::Init { project_name } => {
            println!("Init...")
        }
    }

    Ok(())
}

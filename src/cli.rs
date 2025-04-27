use clap::{Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(
    name = "Lux",
    version = "0.0.1-alpha.1",
    about = "Lux language CLI – compile and run .lux programs"
)]
pub struct Cli {
    /// Enable Prism VM debug trace
    #[arg(short, long, global = true)]
    pub debug: bool,

    #[command(subcommand)]
    pub command: Cmd,
}

#[derive(Debug, Subcommand)]
pub enum Cmd {
    /// Compile & run a Lux program
    Run {
        /// Source file to execute
        file: PathBuf,

        /// Emit intermediate artefact instead of executing
        #[arg(long, value_enum)]
        emit: Option<EmitStage>,
    },

    /// Interactive REPL (NOT IMPLEMENTED YET)
    Repl,
}

#[derive(Debug, Copy, Clone, ValueEnum)]
pub enum EmitStage {
    Ast,
    Ir,
    Bytecode,
}

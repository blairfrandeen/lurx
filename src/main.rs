pub mod compiler;

use clap::{CommandFactory, Parser};
use compiler::lexer;

use std::fs;
use std::path::PathBuf;

/// A Lox Implementation in Rust
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Execute code from the specified path
    #[clap(value_parser)]
    source_path: Option<PathBuf>,

    /// Execute code directly from the command line
    #[clap(short, long)]
    code: Option<String>,
}

fn main() {
    let args = Args::parse();
    let source: String;
    if let Some(source_file) = args.source_path {
        source = fs::read_to_string(source_file).unwrap();
    } else if let Some(source_code) = args.code {
        source = source_code;
    } else {
        Args::command().print_help().unwrap();
        std::process::exit(1);
    }
    let toks = lexer::scan_source(&source);
    match toks {
        Ok(tokens) => {
            for token in tokens.iter() {
                println!("{token:?}");
            }
        }
        Err(invalid_tokens) => {
            println!("{invalid_tokens:?}");
        }
    }
}

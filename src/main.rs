pub mod compiler;

use clap::{CommandFactory, Parser};
use compiler::errors::ErrorReport;
use compiler::{lexer, parser};

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
    let tokens = match lexer::scan_source(&source) {
        Ok(ref toks) => {
            for token in toks.iter() {
                println!("{token:?}");
            }
            toks.clone()
        }
        Err(ref scan_err) => {
            scan_err.report(&source);
            std::process::exit(1);
        }
    };

    let ast = match parser::parse_tokens(tokens) {
        Ok(tree) => dbg!(tree),
        Err(parse_err) => {
            parse_err.report(&source);
            std::process::exit(1);
        }
    };
    dbg!(ast);
}

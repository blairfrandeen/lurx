pub mod compiler;

use clap::Parser;
use compiler::errors::ErrorReport;
use compiler::{interpreter, lexer, parser};

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
        execute(&source);
    } else if let Some(source_code) = args.code {
        source = source_code;
        execute(&source);
    } else {
        let _ = repl();
        // Args::command().print_help().unwrap();
        // std::process::exit(1);
    }
}

fn execute(source: &String) {
    let tokens = match lexer::scan_source(&source) {
        Ok(ref toks) => {
            // for token in toks.iter() {
            //     println!("{token:?}");
            // }
            toks.clone()
        }
        Err(ref scan_err) => {
            scan_err.report(&source);
            return;
        }
    };

    let program = match parser::program(tokens) {
        Ok(tree) => tree,
        Err(parse_err) => {
            parse_err.report(&source);
            return;
        }
    };
    let interp = interpreter::Interpreter {};
    interp.run(&program);
}
use rustyline;
fn repl() -> rustyline::Result<()> {
    let mut rl = rustyline::DefaultEditor::new()?;
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                if line == "quit".to_string() {
                    break;
                } else {
                    execute(&line)
                }
            }
            Err(err) => match err {
                rustyline::error::ReadlineError::Interrupted => break,
                _ => continue,
            },
        }
    }
    Ok(())
}

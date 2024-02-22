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
    let mut interp = interpreter::Interpreter::new();
    if let Some(source_file) = args.source_path {
        source = fs::read_to_string(source_file).unwrap();
        execute(&source, &mut interp);
    } else if let Some(source_code) = args.code {
        source = source_code;
        execute(&source, &mut interp);
    } else {
        let _ = repl();
    }
}

fn execute(source: &String, interp: &mut interpreter::Interpreter) {
    let tokens = match lexer::scan_source(&source) {
        Ok(ref toks) => toks.clone(),
        Err(ref scan_err) => {
            scan_err.report(&source);
            return;
        }
    };

    let program = parser::program(tokens, source.clone());
    if program.errors.is_empty() {
        interp.run(&program);
    } else {
        for error in program.errors.iter() {
            error.report(&source);
        }
    }
}
use rustyline;
fn repl() -> rustyline::Result<()> {
    let mut interp = interpreter::Interpreter::new();
    let mut rl = rustyline::DefaultEditor::new()?;
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                if line == "quit".to_string() {
                    break;
                } else {
                    execute(&line, &mut interp)
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

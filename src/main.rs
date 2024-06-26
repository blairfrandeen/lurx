pub mod compiler;

use std::{fs, path::PathBuf};

use clap::Parser;
use rustyline;

use compiler::{errors::ErrorReport, interpreter, lexer, parser, resolver};

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

    /// Print the AST
    #[clap(long, action)]
    ast: bool,
}

fn main() {
    let args = Args::parse();
    let source: String;
    let mut interp = interpreter::Interpreter::new();
    if let Some(source_file) = args.source_path {
        source = fs::read_to_string(source_file).unwrap();
        execute(&source, &mut interp, args.ast);
    } else if let Some(source_code) = args.code {
        source = source_code;
        execute(&source, &mut interp, args.ast);
    } else {
        let _ = repl();
    }
}

fn execute(source: &String, interp: &mut interpreter::Interpreter, show_ast: bool) {
    let tokens = match lexer::scan_source(&source) {
        Ok(ref toks) => toks.clone(),
        Err(ref scan_err) => {
            scan_err.report(&source);
            return;
        }
    };

    let program = parser::program(tokens, source.clone());
    if show_ast {
        dbg!(&program);
    }
    if !program.errors.is_empty() {
        for error in program.errors.iter() {
            error.report(&source);
        }
        return;
    }
    let mut res = resolver::Resolver::new(interp);
    res.resolve(&program.statements);
    if !res.errors.is_empty() {
        for error in res.errors.iter() {
            // TODO: implement report for resolver errors
            dbg!(&error);
        }
        return;
    }

    interp.run(&program);
    interp.flush();
}
fn repl() -> rustyline::Result<()> {
    let mut interp = interpreter::Interpreter::new();
    interp.set_print_expr(true);
    let mut rl = rustyline::DefaultEditor::new()?;
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                if line == "quit".to_string() {
                    break;
                } else {
                    execute(&line, &mut interp, false)
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

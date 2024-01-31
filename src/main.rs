pub mod compiler;

use clap::{CommandFactory, Parser};
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
    let toks = lexer::scan_source(&source);
    match toks {
        Ok(ref tokens) => {
            for token in tokens.iter() {
                println!("{token:?}");
            }
        }
        Err(ref scan_err) => {
            handle_scan_error(scan_err, &source);
        }
    }
    parser::parse_tokens(toks.unwrap());
}

fn handle_scan_error(scan_error: &lexer::ScanError, source: &String) {
    match scan_error {
        lexer::ScanError::InvalidToken(ref tokens) => handle_invalid_tokens(&tokens, &source),
        lexer::ScanError::UnterminatedStringLiteral(line_num) => {
            handle_unterminated_literal(*line_num, &source)
        }
    }
}

fn handle_unterminated_literal(line_num: usize, source: &String) {
    let current_line = &source
        .lines()
        .nth(line_num - 1)
        .expect("source should have correct number of lines");
    let char_index = &current_line
        .rfind('"')
        .expect("character should be in line");
    println!(
        "Syntax Error line {}: Unterminated string literal",
        line_num
    );
    println!("\t{current_line}");
    println!("\t{: >1$}", "^", char_index + 1);
    println!();
}

fn handle_invalid_tokens(invalid_tokens: &Vec<lexer::Token>, source: &String) {
    for token in invalid_tokens.iter() {
        let current_line = &source
            .lines()
            .nth(token.line_num - 1)
            .expect("source should have correct number of lines");
        let char_index = &current_line
            .find(&token.lexeme)
            .expect("character should be in line");
        println!(
            "Syntax Error line {}: Invalid token ('{}')",
            token.line_num, token.lexeme
        );
        println!("\t{current_line}");
        println!("\t{: >1$}", "^", char_index + 1);
        println!();
    }
}

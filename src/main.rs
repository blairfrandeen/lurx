pub mod compiler;

use compiler::lexer;

use std::fs;

fn main() {
    let source = fs::read_to_string("tests/lox.lox").unwrap();
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

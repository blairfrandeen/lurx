mod compiler;

use compiler::lexer;

fn main() {
    let source = "andy formless fo _ _123 _abc ab123\n
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
        .to_string();
    let source =
        "and class else false for fun if nil or return super this true var while".to_string();
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

mod compiler;

use compiler::lexer;

fn main() {
    let source = "+\n+\n!=!>=<====wtf and or while//banana\nhi+".to_string();
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

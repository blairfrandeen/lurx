mod compiler;

use compiler::lexer;

fn main() {
    let source = "+\n+".to_string();
    let toks = lexer::scan_source(&source).unwrap();

    for token in toks.iter() {
        println!("{token:?}");
    }
}

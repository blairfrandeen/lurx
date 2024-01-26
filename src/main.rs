mod compiler;

use compiler::lexer::{Literal, Token, TokenType};

fn main() {
    let tok = Token {
        type_: TokenType::StringLit,
        line_num: 0,
        literal: Some(Literal::StringLit("world".to_string())),
    };

    println!("Hello, {tok}");
}

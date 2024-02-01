use crate::compiler::{lexer, parser};

pub trait ErrorReport {
    fn report(&self, source: &String);
}

impl ErrorReport for lexer::ScanError {
    fn report(&self, source: &String) {
        match &self {
            lexer::ScanError::InvalidToken(ref tokens) => report_invalid_tokens(&tokens, &source),
            lexer::ScanError::UnterminatedStringLiteral(line_num) => {
                report_unterminated_literal(*line_num, &source)
            }
        }
    }
}

impl ErrorReport for parser::ParseError {
    fn report(&self, source: &String) {
        match &self {
            parser::ParseError::UnclosedParenthesis(token) => report_unclosed(&token, &source),
            parser::ParseError::UnexpectedToken(token) => {
                println!(
                    "Syntax error line line {}: unexpected token",
                    token.line_num
                );
                show_error_token(&token, &source);
            }
        }
    }
}

fn report_unclosed(token: &lexer::Token, source: &String) {
    println!(
        "Syntax Error line {}: Unclosed Parenthesis!",
        token.line_num
    );
    show_error_token(&token, &source);
    println!();
}

fn report_unterminated_literal(line_num: usize, source: &String) {
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

fn report_invalid_tokens(invalid_tokens: &Vec<lexer::Token>, source: &String) {
    for token in invalid_tokens.iter() {
        println!(
            "Syntax Error line {}: Invalid token ('{}')",
            token.line_num, token.lexeme
        );
        show_error_token(&token, &source);
        println!();
    }
}

fn show_error_token(token: &lexer::Token, source: &String) {
    // TODO: This function currently only shows the first matching invalid token in a line; if
    // there are more than one of the same type of invalid token, e.g. the source "dude? wtf?", only
    // the instance of '?' following "dude" will be found. Implement a solution that shows both
    // invalid tokens separately.
    let current_line = &source
        .lines()
        .nth(token.line_num - 1)
        .expect("source should have correct number of lines");
    let char_index = &current_line
        .find(&token.lexeme)
        .expect("character should be in line");
    println!("\t{current_line}");
    let arrows = format!("{:^>1$}", "", &token.lexeme.len());
    print!("\t{: >1$}", "", char_index);
    println!("{arrows}");
}

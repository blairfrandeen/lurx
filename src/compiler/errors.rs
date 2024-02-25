use crate::compiler::{interpreter, lexer, parser};

pub trait ErrorReport {
    fn report(&self, source: &String);
}

impl ErrorReport for lexer::ScanError {
    fn report(&self, source: &String) {
        match &self {
            lexer::ScanError::InvalidToken(ref tokens) => report_invalid_tokens(&tokens, &source),
            lexer::ScanError::UnterminatedStringLiteral(token) => {
                report_unterminated_literal(&token, &source)
            }
        }
    }
}

impl ErrorReport for parser::ParseError {
    fn report(&self, source: &String) {
        match &self {
            parser::ParseError::UnclosedParenthesis(token) => report_unclosed(&token, &source),
            parser::ParseError::UnexpectedToken(token) => {
                println!("Syntax error line {:?}: unexpected token", token.loc);
                show_error_token(&token, &source);
            }
            parser::ParseError::NotImplemented(token) => {
                println!("ParseError: Not Implemented ({token})")
            }
            parser::ParseError::MissingEof => println!("Missing EOF Token!"),
            parser::ParseError::ExpectedToken { expected, found } => {
                println!("Expected Token: {expected:?}");
                show_error_token(found, &source);
            }
            parser::ParseError::InvalidAssignmentTarget(target) => {
                println!("Invalid Assignment Target: {:?}", target);
                // TODO: Show target more cleanly than current debug output.
                // See GitHub issue #2
            }
            parser::ParseError::InvalidStatement(stmt) => {
                println!("Invalid Statement: {:?}", stmt);
            }
        }
    }
}

impl ErrorReport for interpreter::RuntimeError {
    fn report(&self, source: &String) {
        match &self {
            interpreter::RuntimeError::TypeError {
                left: _,
                operator,
                right: _,
            } => {
                println!("TypeError on or near line {}:", operator.line_num(&source));
                show_error_token(&operator, &source);
            }
            interpreter::RuntimeError::ZeroDivision {
                left: _,
                operator,
                right: _,
            } => {
                println!("ZeroDivision Error, Line {}:", operator.line_num(&source));
                show_error_token(&operator, &source);
            }
            interpreter::RuntimeError::InvalidOperand {
                operator,
                operand: _,
            } => {
                println!("InvalidOperand Error, Line {}:", operator.line_num(&source));
                show_error_token(&operator, &source);
            }
            interpreter::RuntimeError::NotImplemented => println!("Not implemented!"),
            interpreter::RuntimeError::NameError(name) => {
                let ident = match name.literal.as_ref().unwrap() {
                    lexer::Literal::Ident(s) => s,
                    _ => panic!(),
                };
                println!(
                    "Undefined Name, line {}: '{}'",
                    name.line_num(&source),
                    ident
                );
                show_error_token(&name, &source);
            }
        }
    }
}

fn report_unclosed(token: &lexer::Token, source: &String) {
    println!(
        "Syntax Error line {}: Unclosed Parenthesis!",
        token.line_num(&source)
    );
    show_error_token(&token, &source);
    println!();
}

fn report_unterminated_literal(token: &lexer::Token, source: &String) {
    let line_num = token.line_num(&source);
    let current_line = &source
        .lines()
        .nth(line_num - 1)
        .expect("source should have correct number of lines");
    println!(
        "Syntax Error line {}: Unterminated string literal",
        line_num
    );
    println!("\t{current_line}");
    println!("\t{: >1$}", "^", &token.line_index(&source) + 1);
    println!();
}

fn report_invalid_tokens(invalid_tokens: &Vec<lexer::Token>, source: &String) {
    for token in invalid_tokens.iter() {
        println!(
            "Syntax Error line {}: Invalid token",
            token.line_num(&source)
        );
        show_error_token(&token, &source);
        println!();
    }
}

fn show_error_token(token: &lexer::Token, source: &String) {
    let current_line = &source
        .lines()
        .nth(&token.line_num(&source) - 1)
        .expect("source should have correct number of lines");
    println!("\t{current_line}");
    let arrows = format!("{:^>1$}", "", &token.loc.1 - &token.loc.0);
    print!("\t{: >1$}", "", &token.line_index(&source));
    println!("{arrows}");
}

#[allow(unused)]
fn show_previous_error_token(token: &lexer::Token, source: &String) {
    // TODO: Work in progress, not currently working
    // See issue 6
    let mut line_num = match token.loc.0 {
        0 => token.line_num(&source) - 1,
        _ => token.line_num(&source),
    };
    if source.lines().count() == 1 {
        line_num += 1;
    }
    let current_line = &source
        .lines()
        .nth(line_num - 1)
        .expect("source should have correct number of lines");
    let line_index = match token.loc.0 {
        0 => current_line.len(),
        _ => token.line_index(&source),
    };
    println!("\t{current_line}");
    let arrows = format!("{:^>1$}", "", 1);
    print!("\t{: >1$}", "", line_index);
    println!("{arrows}");
}

impl lexer::Token {
    /// Get the line number that the token appears on in a given string of source code
    fn line_num(&self, source: &String) -> usize {
        source[0..=self.loc.0].lines().count()
    }

    /// Get the character index for the start of the token on the line in which it appears in the
    /// source code
    fn line_index(&self, source: &String) -> usize {
        match &source[0..=self.loc.0].rfind('\n') {
            Some(prev_newline_index) => self.loc.0 - prev_newline_index - 1,
            None => self.loc.0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_token_offset() {
        let source = "?".to_string();
        let mut token = lexer::Token::from_type(lexer::TokenType::INVALID);
        token.loc = (0, 1);
        assert_eq!(token.line_index(&source), 0);

        let source = "abc 123\n456\n".to_string();
        let mut token = lexer::Token::numlit(456.0);
        token.loc = (8, 10);
        assert_eq!(token.line_index(&source), 0);

        let source = "abc 123\n456==\n".to_string();
        let mut token = lexer::Token::from_type(lexer::TokenType::EQUAL_EQUAL);
        token.loc = (11, 12);
        assert_eq!(token.line_index(&source), 3);

        let source = "abc 123\n\n\n\n456==\n".to_string();
        let mut token = lexer::Token::from_type(lexer::TokenType::EQUAL_EQUAL);
        token.loc = (14, 15);
        assert_eq!(token.line_index(&source), 3);

        let source = "var a = 1;\na = a+7;\n\nprint abc;".to_string();
        let toks = lexer::scan_source(&source).unwrap();
        assert_eq!(toks[11].line_index(&source), 0); // print
        assert_eq!(toks[12].line_index(&source), 6); // abc
        assert_eq!(toks[13].line_index(&source), 9); // semicolon
    }
}

#![allow(dead_code, unused_imports, unused_variables)]
use std::fmt::{Display, Formatter};
use std::io::Write;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ScanError {
    #[error("invalid tokens")]
    InvalidToken(Vec<Token>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Operators
    Equals,
    NotEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    Add,
    Multiply,
    Divide,
    Subtract,
    // TODO

    // Literals
    Identifier,
    StringLit,
    NumLit,
    // Keywords
    // TODO

    // InvalidToken for error handling
    InvalidToken,
}

#[derive(Debug, Clone)]
pub enum Literal {
    StringLit(String),
    NumLit(i32),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub type_: TokenType,
    pub line_num: u32,
    pub literal: Option<Literal>,
}

pub fn scan_source(source: &String) -> Result<Vec<Token>, ScanError> {
    let mut chars = source.chars().peekable();
    let mut tokens: Vec<Token> = Vec::new();
    let mut line_num: u32 = 1;
    while let Some(current_char) = chars.next() {
        let token = match current_char {
            '\n' => {
                line_num += 1;
                continue;
            }
            '+' => Token {
                type_: TokenType::Add,
                line_num,
                literal: None,
            },
            // logic for creating tokens based on the current character goes here
            _ => todo!(),
        };
        tokens.push(token);
    }
    let invalid_tokens: Vec<Token> = tokens
        .clone()
        .into_iter()
        .filter(|tok| tok.type_ == TokenType::InvalidToken)
        .collect();

    if invalid_tokens.len() > 0 {
        Err(ScanError::InvalidToken(invalid_tokens))
    } else {
        Ok(tokens)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.literal {
            Some(lit) => write!(f, "{:?} ({})", self.type_, lit)?,
            None => write!(f, "{:?}", self.type_)?,
        }
        Ok(())
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self {
            Literal::StringLit(s) => write!(f, "\"{s}\"")?,
            Literal::NumLit(n) => write!(f, "{n}")?,
        }
        Ok(())
    }
}

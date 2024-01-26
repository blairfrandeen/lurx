#![allow(non_camel_case_types, dead_code, unused_imports, unused_variables)]
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
    // Single-character operators
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    PLUS,
    STAR,
    SLASH,
    MINUS,
    COMMA,
    DOT,
    SEMICOLON,

    // One or two character operators
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // TODO

    // Literals
    IDENTIFIER,
    STRINGLIT,
    NUMLIT,
    // Keywords
    // TODO

    // InvalidToken for error handling
    INVALID,
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
    pub lexeme: String,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            type_: TokenType::INVALID,
            line_num: 0,
            literal: None,
            lexeme: "".to_string(),
        }
    }
}

pub fn scan_source(source: &String) -> Result<Vec<Token>, ScanError> {
    let mut chars = source.chars().peekable();
    let mut tokens: Vec<Token> = Vec::new();
    // let mut invalid_tokens: Vec<Token> = Vec::new();
    let mut line_num: u32 = 1;
    while let Some(current_char) = chars.next() {
        let lexeme = String::from(current_char);
        let mut token = Token {
            line_num,
            lexeme,
            ..Default::default()
        };
        match current_char {
            '\n' => {
                // newlines don't count as tokens, so we just increment the line number.
                line_num += 1;
                continue;
            }
            // Single character tokens
            '+' => token.type_ = TokenType::PLUS,
            '-' => token.type_ = TokenType::MINUS,
            '*' => token.type_ = TokenType::STAR,
            '/' => token.type_ = TokenType::SLASH,
            '(' => token.type_ = TokenType::LEFT_PAREN,
            ')' => token.type_ = TokenType::RIGHT_PAREN,
            '{' => token.type_ = TokenType::LEFT_BRACE,
            '}' => token.type_ = TokenType::RIGHT_BRACE,
            ',' => token.type_ = TokenType::COMMA,
            '.' => token.type_ = TokenType::DOT,
            ';' => token.type_ = TokenType::SEMICOLON,

            // One or two character tokens
            '!' => match chars.next_if(|c| *c == '=') {
                Some(c) => {
                    token.type_ = TokenType::BANG_EQUAL;
                    token.lexeme.push(c);
                }
                None => token.type_ = TokenType::BANG,
            },
            '=' => match chars.next_if(|c| *c == '=') {
                Some(c) => {
                    token.type_ = TokenType::EQUAL_EQUAL;
                    token.lexeme.push(c);
                }
                None => token.type_ = TokenType::EQUAL,
            },
            '>' => match chars.next_if(|c| *c == '=') {
                Some(c) => {
                    token.type_ = TokenType::GREATER_EQUAL;
                    token.lexeme.push(c);
                }
                None => token.type_ = TokenType::GREATER,
            },
            '<' => match chars.next_if(|c| *c == '=') {
                Some(c) => {
                    token.type_ = TokenType::LESS_EQUAL;
                    token.lexeme.push(c);
                }
                None => token.type_ = TokenType::LESS,
            },
            _ => token.type_ = TokenType::INVALID,
            // logic for creating tokens based on the current character goes here
        };
        tokens.push(token);
    }
    let invalid_tokens: Vec<Token> = tokens
        .clone()
        .into_iter()
        .filter(|tok| tok.type_ == TokenType::INVALID)
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

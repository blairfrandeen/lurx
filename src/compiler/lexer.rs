#![allow(non_camel_case_types)]
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ScanError {
    #[error("invalid tokens")]
    InvalidToken(Vec<Token>),

    #[error("Unterminated string literal starting on line {0}")]
    UnterminatedStringLiteral(u32),
}

fn keywords() -> HashMap<&'static str, TokenType> {
    let mut keywords: HashMap<&'static str, TokenType> = HashMap::new();
    keywords.insert("and", TokenType::AND);
    keywords.insert("class", TokenType::CLASS);
    keywords.insert("else", TokenType::ELSE);
    keywords.insert("false", TokenType::FALSE);
    keywords.insert("for", TokenType::FOR);
    keywords.insert("fun", TokenType::FUN);
    keywords.insert("if", TokenType::IF);
    keywords.insert("nil", TokenType::NIL);
    keywords.insert("or", TokenType::OR);
    keywords.insert("print", TokenType::PRINT);
    keywords.insert("return", TokenType::RETURN);
    keywords.insert("super", TokenType::SUPER);
    keywords.insert("this", TokenType::THIS);
    keywords.insert("true", TokenType::TRUE);
    keywords.insert("var", TokenType::VAR);
    keywords.insert("while", TokenType::WHILE);
    keywords
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

    // Literals
    IDENTIFIER,
    STRINGLIT,
    NUMLIT,

    // Keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    // InvalidToken for error handling
    INVALID,

    // End of file
    EOF,
}

#[derive(Debug, Clone)]
pub enum Literal {
    StringLit(String),
    NumLit(f32),
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

    let keywords: HashMap<&str, TokenType> = keywords();
    let mut line_num: u32 = 1;

    while let Some(current_char) = chars.next() {
        let lexeme = String::from(current_char);
        let mut token = Token {
            line_num,
            lexeme,
            ..Default::default()
        };
        match current_char {
            // newlines don't count as tokens, so we just increment the line number.
            '\n' => {
                line_num += 1;
                continue;
            }

            // ignore whitespace
            ' ' => continue,
            '\t' => continue,
            '\r' => continue,

            // Single character tokens
            '+' => token.type_ = TokenType::PLUS,
            '-' => token.type_ = TokenType::MINUS,
            '*' => token.type_ = TokenType::STAR,
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

            // slash or single line comment
            '/' => match chars.next_if(|c| *c == '/') {
                Some(_) => {
                    while let Some(next_chr) = chars.next() {
                        if next_chr == '\n' {
                            break;
                        }
                    }
                    continue;
                }
                None => token.type_ = TokenType::SLASH,
            },

            // String Literal
            '"' => {
                token.type_ = TokenType::STRINGLIT;
                let mut str_lit = String::new();
                while let Some(next_char) = chars.next_if(|c| *c != '"') {
                    // TODO: allow for escaped quotes
                    if next_char == '\n' {
                        line_num += 1;
                    }
                    token.lexeme.push(next_char);
                    str_lit.push(next_char);
                }
                match chars.next() {
                    Some(next_char) => {
                        token.lexeme.push(next_char);
                        token.literal = Some(Literal::StringLit(str_lit));
                    }
                    None => return Err(ScanError::UnterminatedStringLiteral(token.line_num)),
                }
            }

            // everything else
            _ => {
                // number literal
                if current_char.is_digit(10) {
                    let mut found_decimal = false;
                    while let Some(next_char) =
                        chars.next_if(|c| ((*c == '.') & !found_decimal) | c.is_digit(10))
                    {
                        token.lexeme.push(next_char);
                        if next_char == '.' {
                            found_decimal = true;
                        }
                    }
                    let num_value: f32 = token.lexeme.parse().expect("Error parsing float!");
                    token.literal = Some(Literal::NumLit(num_value));
                    token.type_ = TokenType::NUMLIT;

                // all keywords start and identifiers start with a letter
                } else if current_char.is_ascii_alphabetic() | (current_char == '_') {
                    while let Some(next_char) =
                        chars.next_if(|c| c.is_ascii_alphabetic() | c.is_digit(10) | (*c == '_'))
                    {
                        token.lexeme.push(next_char);
                    }

                    // check for keyword
                    match keywords.get(&token.lexeme.as_str()) {
                        Some(token_type) => token.type_ = token_type.clone(),
                        None => token.type_ = TokenType::IDENTIFIER,
                    }
                } else {
                    token.type_ = TokenType::INVALID;
                }
            }
        };
        tokens.push(token);
    }
    tokens.push(Token {
        type_: TokenType::EOF,
        line_num,
        ..Default::default()
    });
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
        match &self.type_ {
            TokenType::IDENTIFIER => write!(f, "{:?} ({})", self.type_, self.lexeme)?,
            _ => match &self.literal {
                Some(lit) => write!(f, "{:?} ({})", self.type_, lit)?,
                None => write!(f, "{:?}", self.type_)?,
            },
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

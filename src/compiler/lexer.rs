#![allow(non_camel_case_types, unused)]
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum ScanError {
    #[error("invalid tokens")]
    InvalidToken(Vec<Token>),

    #[error("Unterminated string literal starting on line {0}")]
    UnterminatedStringLiteral(Token),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    StringLit(String),
    Ident(String),
    NumLit(f32),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub type_: TokenType,
    pub loc: (usize, usize),
    pub literal: Option<Literal>,
}

/// Convenience / helper functions for quickly creating tokens for use in testing
impl Token {
    pub fn from_type(type_: TokenType) -> Self {
        Token {
            type_,
            ..Default::default()
        }
    }
    pub fn identifier(literal: String) -> Self {
        Token {
            type_: TokenType::IDENTIFIER,
            literal: Some(Literal::StringLit(literal)),
            ..Default::default()
        }
    }
    pub fn stringlit(literal: String) -> Self {
        Token {
            type_: TokenType::STRINGLIT,
            literal: Some(Literal::StringLit(literal)),
            ..Default::default()
        }
    }
    pub fn numlit(literal: f32) -> Self {
        Token {
            type_: TokenType::NUMLIT,
            literal: Some(Literal::NumLit(literal)),
            ..Default::default()
        }
    }

    /// Make a token from a string. If the string results in multiple tokens,
    /// only the first token will be made available.
    /// This function will panic with invalid input and is intended for testing only.
    pub fn from(source: &str) -> Self {
        let results = scan_source(&source.to_string()).expect("Invalid Token!");
        results.get(0).expect("No tokens found!").to_owned()
    }
}

impl Default for Token {
    fn default() -> Self {
        Token {
            type_: TokenType::INVALID,
            loc: (0, 0),
            literal: None,
        }
    }
}

pub fn scan_source(source: &String) -> Result<Vec<Token>, ScanError> {
    let mut chars = source.chars().enumerate().peekable();
    let mut tokens: Vec<Token> = Vec::new();

    let keywords: HashMap<&str, TokenType> = keywords();
    let mut line_num: usize = 1;

    while let Some((loc_start, current_char)) = chars.next() {
        let mut lexeme = String::from(current_char);
        let mut loc_end = loc_start + 1;
        let mut token = Token {
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
            '!' => match chars.next_if(|(_, c)| *c == '=') {
                Some(c) => {
                    token.type_ = TokenType::BANG_EQUAL;
                    loc_end += 1;
                }
                None => token.type_ = TokenType::BANG,
            },
            '=' => match chars.next_if(|(_, c)| *c == '=') {
                Some(c) => {
                    token.type_ = TokenType::EQUAL_EQUAL;
                    loc_end += 1;
                }
                None => token.type_ = TokenType::EQUAL,
            },
            '>' => match chars.next_if(|(_, c)| *c == '=') {
                Some(c) => {
                    token.type_ = TokenType::GREATER_EQUAL;
                    loc_end += 1;
                }
                None => token.type_ = TokenType::GREATER,
            },
            '<' => match chars.next_if(|(_, c)| *c == '=') {
                Some(c) => {
                    token.type_ = TokenType::LESS_EQUAL;
                    loc_end += 1;
                }
                None => token.type_ = TokenType::LESS,
            },

            // slash or single line comment
            '/' => match chars.next_if(|(_, c)| *c == '/') {
                Some(_) => {
                    while let Some((_, next_chr)) = chars.next() {
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
                while let Some((_, next_char)) = chars.next_if(|(_, c)| *c != '"') {
                    // TODO: allow for escaped quotes
                    if next_char == '\n' {
                        line_num += 1;
                    }
                    loc_end += 1;
                    str_lit.push(next_char);
                }
                match chars.next() {
                    Some((_, next_char)) => {
                        loc_end += 1; // TODO: necessary to increment here??
                        token.literal = Some(Literal::StringLit(str_lit));
                    }
                    None => {
                        token.loc = (loc_start, loc_end);
                        return Err(ScanError::UnterminatedStringLiteral(token.clone()));
                    }
                }
            }

            // everything else
            _ => {
                // number literal
                if current_char.is_digit(10) {
                    let mut found_decimal = false;
                    while let Some((_, next_char)) =
                        chars.next_if(|(_, c)| ((*c == '.') & !found_decimal) | c.is_digit(10))
                    {
                        lexeme.push(next_char);
                        loc_end += 1;
                        if next_char == '.' {
                            found_decimal = true;
                        }
                    }
                    let num_value: f32 = lexeme.parse().expect("Error parsing float!");
                    token.literal = Some(Literal::NumLit(num_value));
                    token.type_ = TokenType::NUMLIT;

                // all keywords start and identifiers start with a letter
                } else if current_char.is_ascii_alphabetic() | (current_char == '_') {
                    while let Some((_, next_char)) = chars
                        .next_if(|(_, c)| c.is_ascii_alphabetic() | c.is_digit(10) | (*c == '_'))
                    {
                        lexeme.push(next_char);
                    }

                    // check for keyword
                    match keywords.get(&lexeme.as_str()) {
                        Some(token_type) => {
                            token.type_ = token_type.clone();
                            loc_end = loc_start + lexeme.len();
                        }
                        None => {
                            token.type_ = TokenType::IDENTIFIER;
                            token.literal = Some(Literal::Ident(lexeme));
                        }
                    }
                } else {
                    while let Some((_, next_char)) = chars.next_if(|(_, c)| {
                        !c.is_ascii_alphabetic()
                            & !c.is_digit(10)
                            & !(*c == '_')
                            & !c.is_ascii_whitespace()
                    }) {
                        lexeme.push(next_char);
                    }
                    token.type_ = TokenType::INVALID;
                    loc_end = loc_start + lexeme.len();
                }
            }
        };
        token.loc = (loc_start, loc_end);
        tokens.push(token);
    }
    tokens.push(Token {
        type_: TokenType::EOF,
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
            TokenType::IDENTIFIER => {
                write!(f, "{:?} ({})", self.type_, self.literal.as_ref().unwrap())?
            }
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
            Literal::Ident(s) => write!(f, "{s}")?,
            Literal::NumLit(n) => write!(f, "{n}")?,
        }
        Ok(())
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        (self.type_ == other.type_) & (self.literal == other.literal)
    }
}

/// for testing purposes
pub fn token_iter(source: &str) -> std::iter::Peekable<std::vec::IntoIter<Token>> {
    let tokens = crate::lexer::scan_source(&source.to_string()).unwrap();
    let token_iter = tokens.into_iter().peekable();
    token_iter
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifiers() {
        let input = "
   andy formless fo _ _123 _abc ab123
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
            .to_string();
        let result = scan_source(&input).expect("should work");
        let mut results = result.iter();

        let lexemes = vec![
            "andy",
            "formless",
            "fo",
            "_",
            "_123",
            "_abc",
            "ab123",
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_",
        ];
        let mut lex_iter = lexemes.iter();

        while let Some(lex) = lex_iter.next() {
            let test_tok = Token {
                type_: TokenType::IDENTIFIER,
                literal: Some(Literal::Ident(lex.to_string())),
                ..Default::default()
            };
            let res = results.next().expect("should have enough tokens");
            assert_eq!(&test_tok, res,)
        }
        assert_eq!(
            results.next().expect("Should have EOF").type_,
            TokenType::EOF
        )
    }

    #[test]
    fn test_keywords() {
        let input =
            String::from("and class else false for fun if nil or return super this true var while");
        let result = scan_source(&input).expect("should work");
        let types = vec![
            TokenType::AND,
            TokenType::CLASS,
            TokenType::ELSE,
            TokenType::FALSE,
            TokenType::FOR,
            TokenType::FUN,
            TokenType::IF,
            TokenType::NIL,
            TokenType::OR,
            TokenType::RETURN,
            TokenType::SUPER,
            TokenType::THIS,
            TokenType::TRUE,
            TokenType::VAR,
            TokenType::WHILE,
            TokenType::EOF,
        ];
        for i in 0..types.len() {
            assert_eq!(types[i], result[i].type_);
        }
    }

    #[test]
    fn test_loxlox() {
        let loxlox = std::fs::read_to_string("tests/lox.lox").expect("file should exist");
        assert!(scan_source(&loxlox).is_ok())
    }

    #[test]
    fn test_invalid() {
        // invalid token lexemes should be all consecutive invalid characters
        let invalid_source = String::from("?@^#");
        let result = scan_source(&invalid_source);
        assert_eq!(
            result,
            Err(ScanError::InvalidToken(vec![Token {
                type_: TokenType::INVALID,
                ..Default::default()
            }]))
        );
    }
}

#![allow(dead_code, unused_imports, unused_variables)]
use std::fmt::{Display, Formatter};
use std::io::Write;

#[derive(Debug)]
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
}

#[derive(Debug)]
pub enum Literal {
    StringLit(String),
    NumLit(i32),
}

pub struct Token {
    pub type_: TokenType,
    pub line_num: u32,
    pub literal: Option<Literal>,
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

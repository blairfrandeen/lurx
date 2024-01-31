#![allow(unused)]
use crate::lexer::{Token, TokenType};

use std::iter::{Iterator, Peekable};

pub enum ParseError {}

pub trait Parse {
    fn build<'a>(
        &self,
        tokens: &mut Peekable<impl Iterator<Item = &'a Token>>,
    ) -> Result<Self, ParseError>
    where
        Self: Sized,
    {
        todo!();
    }
}
struct Operator {
    kind: TokenType,
}

enum Expression {
    Literal(Literal),
    Unary(Unary),
    Binary(Binary),
    Grouping(Grouping),
}

enum Literal {
    Number(f32),
    StrLit(String),
    True,
    False,
    Nil,
}

enum Unary {
    Minus(Box<Expression>),
    Not(Box<Expression>),
}

struct Grouping {
    expr: Box<Expression>,
}

struct Binary {
    left: Box<Expression>,
    op: Operator,
    right: Box<Expression>,
}

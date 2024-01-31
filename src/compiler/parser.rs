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

enum Expression {
    Equality(Equality),
    // TODO!
}

struct Equality {
    comparison: Comparison,
    components: Vec<EqualityComponent>,
}

enum EqualityComponent {
    Equals(Comparison),
    NotEquals(Comparison),
}

struct Factor {
    unary: Unary,
    components: Vec<FactorComponent>,
}

enum FactorComponent {
    Mul(Unary),
    Div(Unary),
}

enum Unary {
    Minus(Box<Unary>),
    Not(Box<Unary>),
    Primary(Primary),
}

enum Primary {
    Number(f32),
    StrLit(String),
    True,
    False,
    Nil,
    Group(Box<Expression>),
}

struct Comparison {
    term: Term,
    components: Vec<ComparisonComponent>,
}

enum ComparisonComponent {
    Greater(Term),
    GreaterEquals(Term),
    Less(Term),
    LessEquals(Term),
}

struct Term {
    factor: Factor,
    components: Vec<TermComponent>,
}

enum TermComponent {
    Add(Factor),
    Sub(Factor),
}

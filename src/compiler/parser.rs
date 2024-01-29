#![allow(unused)]
use crate::compiler::lexer;

struct Operator {
    kind: lexer::TokenType,
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

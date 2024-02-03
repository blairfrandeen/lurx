#![allow(unused)]
use crate::compiler::lexer::TokenType;
use crate::compiler::parser;

use std::rc::Rc;

#[derive(Debug)]
pub enum RuntimeError {}

#[derive(Debug)]
pub struct LoxObject {
    value: LoxValue,
}

#[derive(Debug)]
enum LoxValue {
    StrLit(String),
    Number(f32),
    True,
    False,
    Nil,
}

pub enum Expression {
    Literal(parser::Primary),
    Unary(Unary),
    Binary(Binary),
    Grouping(parser::Expression),
}

pub struct Unary {
    pub operator: Option<TokenType>,
    pub expression: Rc<Expression>,
}

pub struct Binary {
    pub left: Rc<Expression>,
    pub operator: Option<TokenType>,
    pub right: Rc<Expression>,
}

pub trait Evaluate {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError>;
}

impl Evaluate for parser::Primary {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError> {
        let value = match &self {
            parser::Primary::Number(val) => LoxValue::Number(*val),
            parser::Primary::StrLit(strlit) => LoxValue::StrLit(strlit.clone()),
            parser::Primary::True => LoxValue::True,
            parser::Primary::False => LoxValue::False,
            parser::Primary::Nil => LoxValue::Nil,
            _ => todo!(),
        };
        Ok(LoxObject { value })
    }
}

impl Evaluate for parser::Unary {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError> {
        let primary = match self {
            parser::Unary::Primary(primary) => primary.evaluate()?,
            parser::Unary::Not(unary) => {
                let mut obj = unary.evaluate()?;
                obj.value = match obj.value {
                    LoxValue::True => LoxValue::False,
                    LoxValue::False => LoxValue::True,
                    _ => todo!(),
                };
                obj
            }
            _ => todo!(),
        };
        Ok(primary)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::Parse;

    #[test]
    fn test_eval_unary() {
        let unary_source = String::from("!true");
        let unary_tokens = crate::lexer::scan_source(&unary_source).unwrap();
        let mut token_iter = unary_tokens.into_iter().peekable();
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate();
        dbg!(eval);
        assert!(false);
    }
}

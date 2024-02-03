#![allow(unused)]
use crate::compiler::lexer::TokenType;
use crate::compiler::parser;

use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    InvalidOperand {
        operator: TokenType,
        operand: LoxValue,
    },
}

#[derive(Debug, PartialEq)]
pub struct LoxObject {
    value: LoxValue,
}

#[derive(Debug, PartialEq)]
pub enum LoxValue {
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
            parser::Primary::Group(expr) => todo!(),
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
                    _ => {
                        return Err(RuntimeError::InvalidOperand {
                            operator: TokenType::BANG,
                            operand: obj.value,
                        })
                    }
                };
                obj
            }
            parser::Unary::Minus(unary) => {
                let mut obj = unary.evaluate()?;
                obj.value = match obj.value {
                    LoxValue::Number(n) => LoxValue::Number(-n),
                    _ => {
                        return Err(RuntimeError::InvalidOperand {
                            operator: TokenType::MINUS,
                            operand: obj.value,
                        })
                    }
                };
                obj
            }
        };
        Ok(primary)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::Parse;

    #[test]
    fn test_eval_unary_not_true() {
        let unary_source = String::from("!true");
        let unary_tokens = crate::lexer::scan_source(&unary_source).unwrap();
        let mut token_iter = unary_tokens.into_iter().peekable();
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate().unwrap();
        assert_eq!(eval.value, LoxValue::False);
    }
    #[test]
    fn test_eval_unary_not_false() {
        let unary_source = String::from("!false");
        let unary_tokens = crate::lexer::scan_source(&unary_source).unwrap();
        let mut token_iter = unary_tokens.into_iter().peekable();
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate().unwrap();
        assert_eq!(eval.value, LoxValue::True);
    }
    #[test]
    fn test_eval_unary_minus() {
        let unary_source = String::from("---2");
        let unary_tokens = crate::lexer::scan_source(&unary_source).unwrap();
        let mut token_iter = unary_tokens.into_iter().peekable();
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate().unwrap();
        assert_eq!(eval.value, LoxValue::Number(-2.0));
    }

    #[test]
    fn test_eval_unary_not_invalid() {
        let unary_source = String::from("!2");
        let unary_tokens = crate::lexer::scan_source(&unary_source).unwrap();
        let mut token_iter = unary_tokens.into_iter().peekable();
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate();
        assert_eq!(
            eval,
            Err(RuntimeError::InvalidOperand {
                operator: TokenType::BANG,
                operand: LoxValue::Number(2.0),
            })
        );
    }

    #[test]
    fn test_eval_unary_minus_invalid() {
        let unary_source = String::from("-true");
        let unary_tokens = crate::lexer::scan_source(&unary_source).unwrap();
        let mut token_iter = unary_tokens.into_iter().peekable();
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate();
        assert_eq!(
            eval,
            Err(RuntimeError::InvalidOperand {
                operator: TokenType::MINUS,
                operand: LoxValue::True,
            })
        );
    }
}

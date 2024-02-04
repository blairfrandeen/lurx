#![allow(unused)]
use crate::compiler::lexer::{Literal, Token, TokenType};
use crate::compiler::parser;

use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    InvalidOperand { operator: Token, operand: LoxObject },
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
        let value = match &self.token.type_ {
            TokenType::NUMLIT => LoxValue::Number(
                match &self
                    .token
                    .literal
                    .as_ref()
                    .expect("NUMLIT token without a literal!")
                {
                    Literal::NumLit(val) => *val,
                    _ => panic!("NUMLIT token with incorrect literal enum!"),
                },
            ),
            TokenType::STRINGLIT => LoxValue::StrLit(
                match &self
                    .token
                    .literal
                    .as_ref()
                    .expect("STRINGLIT token without a literal!")
                {
                    Literal::StringLit(val) => val.to_string(),
                    _ => panic!("STRINGLIT token with incorrect literal enum!"),
                },
            ),
            TokenType::TRUE => LoxValue::True,
            TokenType::FALSE => LoxValue::False,
            TokenType::NIL => LoxValue::Nil,
            TokenType::LEFT_PAREN => todo!(),
            _ => panic!("Invalid operator in Unary!"),
        };
        Ok(LoxObject { value })
    }
}

impl Evaluate for parser::Unary {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError> {
        let primary = match self {
            parser::Unary::Primary(primary) => primary.evaluate()?,
            parser::Unary::Unary { operator, unary } => {
                let mut obj = unary.evaluate()?;
                obj.value = match operator.type_ {
                    TokenType::BANG => match obj.value {
                        LoxValue::True => LoxValue::False,
                        LoxValue::False => LoxValue::True,
                        _ => {
                            return Err(RuntimeError::InvalidOperand {
                                operator: operator.clone(),
                                operand: obj,
                            })
                        }
                    },
                    TokenType::MINUS => match obj.value {
                        LoxValue::Number(n) => LoxValue::Number(-n),
                        _ => {
                            return Err(RuntimeError::InvalidOperand {
                                operator: operator.clone(),
                                operand: obj,
                            })
                        }
                    },
                    _ => panic!("Invalid operator in Unary!"),
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
    use crate::lexer::token_iter;

    #[test]
    fn test_eval_unary_not_true() {
        let mut token_iter = token_iter("!true");
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate().unwrap();
        assert_eq!(eval.value, LoxValue::False);
    }
    #[test]
    fn test_eval_unary_not_false() {
        let mut token_iter = token_iter("!false");
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate().unwrap();
        assert_eq!(eval.value, LoxValue::True);
    }
    #[test]
    fn test_eval_unary_minus() {
        let mut token_iter = token_iter("---2");
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate().unwrap();
        assert_eq!(eval.value, LoxValue::Number(-2.0));
    }

    #[test]
    fn test_eval_unary_not_invalid() {
        let mut token_iter = token_iter("!2");
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate();
        assert_eq!(
            eval,
            Err(RuntimeError::InvalidOperand {
                operator: Token::from_type(TokenType::BANG),
                operand: LoxObject {
                    value: LoxValue::Number(2.0)
                },
            })
        );
    }

    #[test]
    fn test_eval_unary_minus_invalid() {
        let mut token_iter = token_iter("-true");
        let un = parser::Unary::parse(&mut token_iter).unwrap();
        let eval = un.evaluate();
        assert_eq!(
            eval,
            Err(RuntimeError::InvalidOperand {
                operator: Token::from("-"),
                operand: LoxObject {
                    value: LoxValue::True
                },
            })
        );
    }
}

#![allow(unused)]
use crate::compiler::lexer::{Literal, Token, TokenType};
use crate::compiler::parser;

use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    InvalidOperand {
        operator: Token,
        operand: LoxObject,
    },
    ZeroDivision {
        left: LoxObject,
        operator: Token,
        right: LoxObject,
    },
    TypeError {
        left: LoxObject,
        operator: Token,
        right: LoxObject,
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

impl Display for LoxValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self {
            LoxValue::StrLit(s) => write!(f, "{}", s)?,
            LoxValue::Number(n) => write!(f, "{}", n)?,
            LoxValue::True => write!(f, "True")?,
            LoxValue::False => write!(f, "False")?,
            LoxValue::Nil => write!(f, "Nil")?,
        }
        Ok(())
    }
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

impl Evaluate for parser::Term {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError> {
        let mut factor = self.factor.evaluate()?;
        let mut components = self.components.iter();
        while let Some(component) = components.next() {
            let component_factor = component.factor.evaluate()?;
            let is_str = match factor.value {
                LoxValue::Number(_) => false,
                LoxValue::StrLit(_) => true,
                _ => {
                    return Err(RuntimeError::TypeError {
                        left: factor,
                        operator: component.operator.clone(),
                        right: component_factor,
                    })
                }
            };
            factor.value = match component_factor.value {
                LoxValue::Number(comp_value) => {
                    if !is_str {
                        let factor_value = match factor.value {
                            LoxValue::Number(n) => n,
                            _ => panic!("Unexpected Type!"),
                        };
                        match component.operator.type_ {
                            TokenType::PLUS => LoxValue::Number(factor_value + comp_value),
                            TokenType::MINUS => LoxValue::Number(factor_value - comp_value),
                            _ => panic!("Unexpected token in FactorComponent!"),
                        }
                    } else {
                        return Err(RuntimeError::TypeError {
                            left: factor,
                            operator: component.operator.clone(),
                            right: component_factor,
                        });
                    }
                }
                LoxValue::StrLit(ref s) => {
                    if is_str & (component.operator.type_ == TokenType::PLUS) {
                        LoxValue::StrLit(format!("{}{}", factor.value, s))
                    } else {
                        return Err(RuntimeError::TypeError {
                            left: factor,
                            operator: component.operator.clone(),
                            right: component_factor,
                        });
                    }
                }
                _ => {
                    return Err(RuntimeError::TypeError {
                        left: factor,
                        operator: component.operator.clone(),
                        right: component_factor,
                    })
                }
            };
        }
        Ok(factor)
    }
}

impl Evaluate for parser::Factor {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError> {
        let mut unary = self.unary.evaluate()?;
        let mut components = self.components.iter();
        while let Some(component) = components.next() {
            let component_unary = component.unary.evaluate()?;
            let unary_value = match unary.value {
                LoxValue::Number(n) => n,
                _ => {
                    return Err(RuntimeError::TypeError {
                        left: unary,
                        operator: component.operator.clone(),
                        right: component_unary,
                    })
                }
            };
            unary.value = match component_unary.value {
                LoxValue::Number(comp_value) => match component.operator.type_ {
                    TokenType::SLASH => {
                        if comp_value == 0.0 {
                            return Err(RuntimeError::ZeroDivision {
                                left: unary,
                                operator: component.operator.clone(),
                                right: component_unary,
                            });
                        }
                        LoxValue::Number(unary_value / comp_value)
                    }
                    TokenType::STAR => LoxValue::Number(unary_value * comp_value),
                    _ => panic!("Unexpected token in FactorComponent!"),
                },

                _ => {
                    return Err(RuntimeError::TypeError {
                        left: unary,
                        operator: component.operator.clone(),
                        right: component_unary,
                    })
                }
            };
        }
        Ok(unary)
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

    #[test]
    fn test_zero_div_error() {
        let factor = parser::Factor::from_str("1/0");
        let result = factor.evaluate();
        assert_eq!(
            result,
            Err(RuntimeError::ZeroDivision {
                left: LoxObject {
                    value: LoxValue::Number(1.0),
                },
                operator: Token::from_type(TokenType::SLASH),
                right: LoxObject {
                    value: LoxValue::Number(0.0),
                },
            })
        )
    }

    #[test]
    fn test_mult() {
        let factor = parser::Factor::from_str("7*7");
        let result = factor.evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::Number(49.0)
            },)
        )
    }

    #[test]
    fn test_div() {
        let factor = parser::Factor::from_str("-49/-7");
        let result = factor.evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::Number(7.0)
            },)
        )
    }

    #[test]
    fn test_factor_type_err() {
        let factor = parser::Factor::from_str("-49/\"seven\"");
        let result = factor.evaluate();
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxObject {
                    value: LoxValue::Number(-49.0),
                },
                operator: Token::from_type(TokenType::SLASH),
                right: LoxObject {
                    value: LoxValue::StrLit("seven".to_string()),
                },
            })
        );
        let factor = parser::Factor::from_str("true * nil");
        let result = factor.evaluate();
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxObject {
                    value: LoxValue::True,
                },
                operator: Token::from_type(TokenType::STAR),
                right: LoxObject {
                    value: LoxValue::Nil,
                },
            })
        )
    }

    #[test]
    fn test_term_type_err() {
        let factor = parser::Term::from_str("-49+true");
        let result = factor.evaluate();
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxObject {
                    value: LoxValue::Number(-49.0),
                },
                operator: Token::from_type(TokenType::PLUS),
                right: LoxObject {
                    value: LoxValue::True,
                },
            })
        )
    }
    #[test]
    fn test_term_addition() {
        let factor = parser::Term::from_str("5*8/4 + 8/2");
        let result = factor.evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::Number(14.0)
            })
        )
    }
    #[test]
    fn test_string_concat() {
        let factor = parser::Term::from_str("\"Hello \" + \"World!\"");
        let result = factor.evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::StrLit("Hello World!".to_string())
            })
        )
    }

    #[test]
    fn test_string_sub_error() {
        let factor = parser::Term::from_str("\"Hello \" - \"World!\"");
        let result = factor.evaluate();
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxObject {
                    value: LoxValue::StrLit("Hello ".to_string()),
                },
                operator: Token::from_type(TokenType::MINUS),
                right: LoxObject {
                    value: LoxValue::StrLit("World!".to_string()),
                },
            })
        )
    }
}

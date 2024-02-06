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

pub trait Evaluate {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError>;
}

/*
impl Evaluate for parser::Comparison {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError> {
        let mut term = self.term.evaluate()?;
        let mut components = self.components.iter();
        while let Some(component) = components.next() {
            let comp_value = match component.term.evaluate()?.value {
                LoxValue::Number(n) => {}
                _ => todo!(),
            };
        }
        Ok(term)
    }
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
*/

impl Evaluate for parser::Expr {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError> {
        match &self {
            parser::Expr::Unary { operator, right } => eval_unary(operator, right),
            parser::Expr::Binary {
                left,
                operator,
                right,
            } => eval_binary(left, operator, right),
            parser::Expr::Grouping(expr) => eval_grouping(expr),
            parser::Expr::Literal(token) => Ok(eval_literal(token)),
        }
    }
}

fn eval_grouping(expr: &parser::Expr) -> Result<LoxObject, RuntimeError> {
    expr.evaluate()
}

fn eval_binary(
    left: &parser::Expr,
    operator: &Token,
    right: &parser::Expr,
) -> Result<LoxObject, RuntimeError> {
    let right = right.evaluate();
    let left = left.evaluate();

    match operator.type_ {
        _ => todo!(),
    }
}

fn eval_unary(operator: &Token, right: &parser::Expr) -> Result<LoxObject, RuntimeError> {
    let mut right = right.evaluate()?;
    right.value = match operator.type_ {
        TokenType::BANG => match right.value {
            LoxValue::True => LoxValue::False,
            LoxValue::False => LoxValue::True,
            _ => {
                return Err(RuntimeError::InvalidOperand {
                    operator: operator.clone(),
                    operand: right,
                })
            }
        },
        TokenType::MINUS => match right.value {
            LoxValue::Number(n) => LoxValue::Number(-n),
            _ => {
                return Err(RuntimeError::InvalidOperand {
                    operator: operator.clone(),
                    operand: right,
                })
            }
        },
        _ => panic!("Invalid operator in Unary!"),
    };
    Ok(right)
}

impl Token {
    /// Get the LoxValue associated with a token, if any.
    /// Panic if invalid token chosen
    fn value(&self) -> LoxValue {
        match self.type_ {
            TokenType::NUMLIT => match &self.literal {
                Some(Literal::NumLit(n)) => LoxValue::Number(*n),
                _ => panic!("NUMLIT token without a literal!"),
            },
            TokenType::STRINGLIT => match &self.literal {
                Some(Literal::StringLit(s)) => LoxValue::StrLit(s.to_string()),
                _ => panic!("STRINGLIT token without a literal!"),
            },
            TokenType::TRUE => LoxValue::True,
            TokenType::FALSE => LoxValue::False,
            TokenType::NIL => LoxValue::Nil,
            _ => panic!("Token does not have an associated value!"),
        }
    }
}

fn eval_literal(token: &Token) -> LoxObject {
    LoxObject {
        value: token.value(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token_iter;

    #[test]
    fn test_eval_unary_not_true() {
        let mut token_iter = token_iter("!true");
        let un = parser::expression(&mut token_iter).unwrap();
        let eval = un.evaluate().unwrap();
        assert_eq!(eval.value, LoxValue::False);
    }
    #[test]
    fn test_eval_unary_not_false() {
        let mut token_iter = token_iter("!false");
        let un = parser::expression(&mut token_iter).unwrap();
        let eval = un.evaluate().unwrap();
        assert_eq!(eval.value, LoxValue::True);
    }
    #[test]
    fn test_eval_unary_minus() {
        let mut token_iter = token_iter("---2");
        let un = parser::expression(&mut token_iter).unwrap();
        let eval = un.evaluate().unwrap();
        assert_eq!(eval.value, LoxValue::Number(-2.0));
    }

    #[test]
    fn test_eval_unary_not_invalid() {
        let mut token_iter = token_iter("!2");
        let un = parser::expression(&mut token_iter).unwrap();
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
        let un = parser::expression(&mut token_iter).unwrap();
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
    /*

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
    */
}

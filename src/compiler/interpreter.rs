use crate::compiler::environment::Environment;
use crate::compiler::lexer::{Literal, Token, TokenType};
use crate::compiler::object::{LoxObject, LoxValue};
use crate::compiler::parser::{Decl, Expr, Program, Stmt};

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
    NameError(Token),
    NotImplemented,
}

pub struct Interpreter {
    globals: Environment,
}

impl Interpreter {
    pub fn run(&mut self, prgm: &Program) {
        let mut decls = prgm.declarations.iter();
        while let Some(decl) = decls.next() {
            match &self.execute_decl(decl) {
                Ok(()) => {}
                Err(err) => println!("{err:?}"),
            }
        }
    }

    pub fn new() -> Self {
        Interpreter {
            globals: Environment::new(),
        }
    }

    fn execute_decl(&mut self, decl: &Decl) -> Result<(), RuntimeError> {
        match decl {
            Decl::Statement(stmt) => Ok(stmt.execute()?),
            Decl::VarDecl { name, initializer } => {
                self.globals.set(name, initializer.evaluate()?);
                Ok(())
            }
        }
    }
}

impl Stmt {
    fn execute(&self) -> Result<(), RuntimeError> {
        match &self {
            Stmt::Print(expr) => {
                println!("{}", expr.evaluate()?);
                Ok(())
            }
            Stmt::Expression(_) => Ok(()),
        }
    }
}

pub trait Evaluate {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError>;
}

fn eval_binary(left: &Expr, operator: &Token, right: &Expr) -> Result<LoxObject, RuntimeError> {
    let right = right.evaluate()?;
    let left = left.evaluate()?;

    let value = match operator.type_ {
        TokenType::SLASH => {
            let right_value = right.number();
            let left_value = left.number();
            if left_value.is_some() & (right_value == Some(0.0)) {
                return Err(RuntimeError::ZeroDivision {
                    left,
                    operator: operator.clone(),
                    right,
                });
            } else if right_value.is_none() | left_value.is_none() {
                return Err(RuntimeError::TypeError {
                    left,
                    operator: operator.clone(),
                    right,
                });
            } else {
                LoxValue::Number(left_value.unwrap() / right_value.unwrap())
            }
        }
        TokenType::STAR => {
            let right_value = right.number();
            let left_value = left.number();
            if right_value.is_none() | left_value.is_none() {
                return Err(RuntimeError::TypeError {
                    left,
                    operator: operator.clone(),
                    right,
                });
            } else {
                LoxValue::Number(right_value.unwrap() * left_value.unwrap())
            }
        }
        TokenType::MINUS => {
            let right_value = right.number();
            let left_value = left.number();
            if right_value.is_none() | left_value.is_none() {
                return Err(RuntimeError::TypeError {
                    left,
                    operator: operator.clone(),
                    right,
                });
            } else {
                LoxValue::Number(right_value.unwrap() - left_value.unwrap())
            }
        }
        TokenType::PLUS => {
            if right.is_str() & left.is_str() {
                LoxValue::StrLit(format!(
                    "{}{}",
                    left.strlit().unwrap(),
                    right.strlit().unwrap()
                ))
            } else if right.is_number() & left.is_number() {
                let right_value = right.number().unwrap();
                let left_value = left.number().unwrap();
                LoxValue::Number(left_value + right_value)
            } else {
                return Err(RuntimeError::TypeError {
                    left,
                    operator: operator.clone(),
                    right,
                });
            }
        }
        TokenType::GREATER => compare_lox_value(&left, &operator, &right)?,
        TokenType::GREATER_EQUAL => compare_lox_value(&left, &operator, &right)?,
        TokenType::LESS => compare_lox_value(&left, &operator, &right)?,
        TokenType::LESS_EQUAL => compare_lox_value(&left, &operator, &right)?,
        TokenType::EQUAL_EQUAL => compare_lox_value(&left, &operator, &right)?,
        TokenType::BANG_EQUAL => compare_lox_value(&left, &operator, &right)?,
        _ => return Err(RuntimeError::NotImplemented),
    };
    Ok(LoxObject { value })
}

fn compare_lox_value(
    left: &LoxObject,
    operator: &Token,
    right: &LoxObject,
) -> Result<LoxValue, RuntimeError> {
    let result: bool;
    if right.is_number() & left.is_number() {
        result = match operator.type_ {
            TokenType::GREATER => left > right,
            TokenType::GREATER_EQUAL => left >= right,
            TokenType::LESS => left < right,
            TokenType::LESS_EQUAL => left <= right,
            TokenType::EQUAL_EQUAL => left == right,
            TokenType::BANG_EQUAL => left != right,
            _ => panic!("unexpected token passed to compare_num"),
        };
    } else if (right.is_str() & left.is_str()) | (right.is_bool() & left.is_bool()) {
        result = match operator.type_ {
            TokenType::EQUAL_EQUAL => left == right,
            TokenType::BANG_EQUAL => left != right,
            _ => {
                return Err(RuntimeError::TypeError {
                    left: left.clone(),
                    operator: operator.clone(),
                    right: right.clone(),
                })
            }
        }
    } else {
        return Err(RuntimeError::TypeError {
            left: left.clone(),
            operator: operator.clone(),
            right: right.clone(),
        });
    }
    match result {
        true => Ok(LoxValue::True),
        false => Ok(LoxValue::False),
    }
}

impl Evaluate for Expr {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError> {
        match &self {
            Expr::Unary { operator, right } => eval_unary(operator, right),
            Expr::Binary {
                left,
                operator,
                right,
            } => eval_binary(left, operator, right),
            Expr::Grouping(expr) => eval_grouping(expr),
            Expr::Literal(token) => Ok(eval_literal(token)),
        }
    }
}

fn eval_grouping(expr: &Expr) -> Result<LoxObject, RuntimeError> {
    expr.evaluate()
}

fn eval_unary(operator: &Token, right: &Expr) -> Result<LoxObject, RuntimeError> {
    let mut right = right.evaluate()?;
    right.value = match operator.type_ {
        TokenType::BANG => match right.value {
            LoxValue::True => LoxValue::False,
            LoxValue::False => LoxValue::True,
            LoxValue::Nil => LoxValue::True,
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
    use crate::interpreter;
    use crate::lexer::{self, token_iter};
    use crate::parser;

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
    fn test_eval_unary_not_nil() {
        let mut token_iter = token_iter("!nil");
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

    #[test]
    fn test_zero_div_error() {
        let mut token_iter = token_iter("1/0");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
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
    fn test_type_before_zero_div_error() {
        let mut token_iter = token_iter("\"anything\"/0");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxObject {
                    value: LoxValue::StrLit("anything".to_string())
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
        let mut token_iter = token_iter("7*7");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::Number(49.0)
            },)
        )
    }

    #[test]
    fn test_div() {
        let mut token_iter = token_iter("-49/-7");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::Number(7.0)
            },)
        )
    }

    #[test]
    fn test_factor_type_err() {
        let mut token_iter_ = token_iter("-49/\"seven\"");
        let result = parser::expression(&mut token_iter_).unwrap().evaluate();
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
        let mut token_iter = token_iter("true * nil");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
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
        let mut token_iter = token_iter("-49+true");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
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
        let mut token_iter = token_iter("5*8/4 + 8/2");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::Number(14.0)
            })
        )
    }
    #[test]
    fn test_string_concat() {
        let mut token_iter = token_iter("\"Hello \" + \"World!\"");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::StrLit("Hello World!".to_string())
            })
        )
    }

    #[test]
    fn test_string_sub_error() {
        let mut token_iter = token_iter("\"Hello \" - \"World!\"");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
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

    #[test]
    fn test_cmp() {
        let mut token_iter = token_iter("3>2");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::True
            })
        )
    }

    #[test]
    fn test_cmp_err() {
        let mut token_iter = token_iter("3>true");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxObject {
                    value: LoxValue::Number(3.0),
                },
                operator: Token::from_type(TokenType::GREATER),
                right: LoxObject {
                    value: LoxValue::True,
                },
            })
        )
    }
    #[test]
    fn test_eq() {
        let mut token_iter = token_iter("(2+2)==5");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::False
            })
        )
    }

    #[test]
    fn test_str_eq() {
        let mut token_iter = token_iter("\"hello\"==\"hello\"");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::True
            })
        )
    }

    #[test]
    fn test_str_not_eq() {
        let mut token_iter = token_iter("\"parrot\"!=\"alive\"");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::True
            })
        )
    }

    #[test]
    fn test_str_num_cmp() {
        let mut token_iter = token_iter("\"parrot\"!=33");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxObject {
                    value: LoxValue::StrLit("parrot".to_string()),
                },
                operator: Token::from_type(TokenType::BANG_EQUAL),
                right: LoxObject {
                    value: LoxValue::Number(33.0),
                },
            })
        )
    }

    #[test]
    fn test_str_gt_cmp() {
        let mut token_iter = token_iter("\"coffee\"<=\"tea\"");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxObject {
                    value: LoxValue::StrLit("coffee".to_string()),
                },
                operator: Token::from_type(TokenType::LESS_EQUAL),
                right: LoxObject {
                    value: LoxValue::StrLit("tea".to_string()),
                },
            })
        )
    }

    #[test]
    fn test_cmp_bool() {
        let mut token_iter = token_iter("true==false");
        let result = parser::expression(&mut token_iter).unwrap().evaluate();
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::False
            })
        )
    }

    #[test]
    fn test_hello_world() {
        let source = "print \"hello world!\";".to_string();
        let tokens = lexer::scan_source(&source).unwrap();
        let program = parser::program(tokens).unwrap();
        let mut interp = interpreter::Interpreter::new();
        interp.run(&program); // should not panic
                              // TODO: Test standard output
    }
}

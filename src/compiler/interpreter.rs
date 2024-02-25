use crate::compiler::environment::Environment;
use crate::compiler::errors::ErrorReport;
use crate::compiler::lexer::{Literal, Token, TokenType};
use crate::compiler::object::{LoxObject, LoxValue};
use crate::compiler::parser::{Expr, Program, Stmt};

use std::io::Write;

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
    Break,
    NotImplemented,
}

pub struct Interpreter {
    env: Environment,
    out: Vec<u8>,
    flush: bool,
    print_expr: bool,
}

impl Interpreter {
    pub fn run(&mut self, prgm: &Program) {
        let mut stmts = prgm.statements.iter();
        while let Some(stmt) = stmts.next() {
            match &self.execute_stmt(stmt) {
                Ok(()) => {
                    if self.flush {
                        self.flush()
                    }
                }
                Err(err) => err.report(&prgm.source),
            }
        }
    }

    pub fn new() -> Self {
        Interpreter {
            env: Environment::new(),
            out: vec![],
            flush: false,
            print_expr: false,
        }
    }

    pub fn flush(&mut self) {
        let out = String::from_utf8(self.out.clone()).unwrap();
        print!("{}", out);
        self.out = vec![];
    }

    pub fn set_flush(&mut self, flush: bool) {
        self.flush = flush;
    }

    pub fn set_print_expr(&mut self, print_expr: bool) {
        self.print_expr = print_expr;
    }

    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match &stmt {
            Stmt::Print(expr) => {
                let _ = writeln!(self.out, "{}", self.evaluate(expr)?);
                Ok(())
            }
            Stmt::VarDecl { name, initializer } => {
                self.env.set(name, self.evaluate(initializer)?);
                Ok(())
            }
            Stmt::Expression(expr) => match expr {
                Expr::Assign { name, value } => {
                    self.env.update(name, self.evaluate(value)?)?;
                    Ok(())
                }
                _ => {
                    if self.print_expr {
                        let _ = writeln!(self.out, "{}", self.evaluate(expr)?);
                    }
                    Ok(())
                }
            },
            Stmt::Block(stmts) => {
                self.env = Environment::enclosed(self.env.clone());
                for stmt in stmts.into_iter() {
                    self.execute_stmt(stmt)?;
                }
                self.env = self.env.enclosing();
                Ok(())
            }
            Stmt::Conditional {
                condition,
                true_branch,
                false_branch,
            } => {
                if is_truthy(&self.evaluate(condition)?) {
                    self.execute_stmt(true_branch)
                } else {
                    match false_branch {
                        Some(branch) => self.execute_stmt(branch),
                        None => Ok(()),
                    }
                }
            }
            Stmt::WhileLoop {
                condition,
                statements,
            } => {
                while is_truthy(&self.evaluate(condition)?) {
                    match self.execute_stmt(statements) {
                        Ok(()) => {}
                        Err(err) => match err {
                            RuntimeError::Break => break,
                            _ => return Err(err),
                        },
                    }
                }
                Ok(())
            }
            Stmt::Break => Err(RuntimeError::Break),
        }
    }

    fn evaluate(&self, expr: &Expr) -> Result<LoxObject, RuntimeError> {
        match &expr {
            Expr::Unary { operator, right } => self.eval_unary(operator, right),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.eval_binary(left, operator, right),
            Expr::Grouping(expr) => self.eval_grouping(expr),
            Expr::Literal(token) => self.eval_literal(token),
            Expr::Variable(token) => self.eval_literal(token),
            Expr::Assign { name: _, value: _ } => todo!(),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.eval_logical(left, operator, right),
        }
    }

    fn eval_logical(
        &self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<LoxObject, RuntimeError> {
        let right = self.evaluate(right)?;
        let left = self.evaluate(left)?;
        let result = match operator.type_ {
            TokenType::AND => is_truthy(&left) & is_truthy(&right),
            TokenType::OR => is_truthy(&left) | is_truthy(&right),
            _ => panic!("Invalid operator for logical: {}", operator),
        };
        let value = match result {
            true => LoxValue::True,
            false => LoxValue::False,
        };
        Ok(LoxObject { value })
    }

    fn eval_binary(
        &self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<LoxObject, RuntimeError> {
        let right = self.evaluate(right)?;
        let left = self.evaluate(left)?;

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
                    LoxValue::Number(left_value.unwrap() - right_value.unwrap())
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

    fn eval_grouping(&self, expr: &Expr) -> Result<LoxObject, RuntimeError> {
        self.evaluate(expr)
    }

    fn eval_unary(&self, operator: &Token, right: &Expr) -> Result<LoxObject, RuntimeError> {
        let mut right = self.evaluate(right)?;
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

    fn eval_literal(&self, token: &Token) -> Result<LoxObject, RuntimeError> {
        match &token.type_ {
            TokenType::IDENTIFIER => self.env.get(&token).cloned(),
            _ => Ok(LoxObject {
                value: token.value(),
            }),
        }
    }
}

fn is_truthy(obj: &LoxObject) -> bool {
    match &obj.value {
        LoxValue::False => false,
        LoxValue::Nil => false,
        _ => true,
    }
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

#[allow(unused_mut)]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter;
    use crate::lexer::{self, token_iter};
    use crate::parser;

    #[test]
    fn test_logical_or() {
        let true_exp = parser::Expr::Literal(Token::from_type(TokenType::TRUE));
        let false_exp = parser::Expr::Literal(Token::from_type(TokenType::FALSE));
        let mut interp = Interpreter::new();
        let result = interp.eval_logical(&true_exp, &Token::from_type(TokenType::OR), &false_exp);
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::True
            })
        );
        let result = interp.eval_logical(&false_exp, &Token::from_type(TokenType::OR), &false_exp);
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::False
            })
        );
    }
    #[test]
    fn test_logical_and() {
        let true_exp = parser::Expr::Literal(Token::from_type(TokenType::TRUE));
        let false_exp = parser::Expr::Literal(Token::from_type(TokenType::FALSE));
        let mut interp = Interpreter::new();
        let result = interp.eval_logical(&true_exp, &Token::from_type(TokenType::AND), &true_exp);
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::True
            })
        );
        let result = interp.eval_logical(&true_exp, &Token::from_type(TokenType::AND), &false_exp);
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::False
            })
        );
    }

    #[test]
    fn test_eval_unary_not_true() {
        let mut token_iter = token_iter("!true");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let eval = interp.evaluate(&un).unwrap();
        assert_eq!(eval.value, LoxValue::False);
    }
    #[test]
    fn test_eval_unary_not_false() {
        let mut token_iter = token_iter("!false");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let eval = interp.evaluate(&un).unwrap();
        assert_eq!(eval.value, LoxValue::True);
    }
    #[test]
    fn test_eval_unary_not_nil() {
        let mut token_iter = token_iter("!nil");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let eval = interp.evaluate(&un).unwrap();
        assert_eq!(eval.value, LoxValue::True);
    }
    #[test]
    fn test_eval_unary_minus() {
        let mut token_iter = token_iter("---2");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let eval = interp.evaluate(&un).unwrap();
        assert_eq!(eval.value, LoxValue::Number(-2.0));
    }

    #[test]
    fn test_eval_unary_not_invalid() {
        let mut token_iter = token_iter("!2");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let eval = interp.evaluate(&un);
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
        let mut interp = Interpreter::new();
        let eval = interp.evaluate(&un);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter_).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut token_iter_ = token_iter("true * nil");
        let expr = parser::expression(&mut token_iter_).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
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
        let mut interp = Interpreter::new();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr);
        assert_eq!(
            result,
            Ok(LoxObject {
                value: LoxValue::False
            })
        )
    }

    #[test]
    fn test_hello_world() {
        test_output("print \"hello world!\";", "hello world!\n");
    }

    #[test]
    fn test_assignment() {
        test_output("var a = 7; a = 9*a; print a;", "63\n");
    }

    #[test]
    fn test_scopes() {
        test_output(
            "var a = 7; var b = 5; { var a = 1; print a; {var a = 2; print a; print b;}} print a;",
            "1\n2\n5\n7\n",
        );
    }

    #[test]
    fn test_ch8_scopes() {
        let scopes = std::fs::read_to_string("tests/scopes.lox").expect("file should exist");
        test_output(
            scopes.as_str(),
            "inner a\nouter b\nglobal c\nouter a\nouter b\nglobal c\nglobal a\nglobal b\nglobal c\n",
        )
    }

    #[test]
    fn test_fib_while() {
        let scopes = std::fs::read_to_string("tests/fib_while.lox").expect("file should exist");
        test_output(scopes.as_str(), "12586269025\n")
    }

    #[test]
    fn test_fib_for() {
        let scopes = std::fs::read_to_string("tests/fib_for.lox").expect("file should exist");
        test_output(scopes.as_str(), "12586269025\n")
    }

    #[test]
    fn test_conditionals() {
        // most basic conditional
        test_output("if true print 1;", "1\n");

        // else statement
        test_output("if false print 2; else print 1;", "1\n");

        // with braces
        test_output("if false { print 2; } else print 1;", "1\n");

        // some assignment and more complex cases
        test_output(
            "var a = 0; if 5*5==26 { a = 2; } else { a = 1; } print a;",
            "1\n",
        );

        // else if
        test_output(
            "var a = 0; var b = 5; if b*5==26 { a = 2; } else if b == 5 { a = 1; } else { a = 4; } print a;",
            "1\n",
        );

        // nesting
        test_output(
            "if true { if true { if false { print 2; } else { print 1; } } }",
            "1\n",
        );
    }

    #[test]
    fn test_assign_in_scope() {
        test_output("var a = 1; { a = 2; print a; } print a;", "2\n2\n");
    }

    #[test]
    fn test_while() {
        test_output(
            "var a = 0; while (a < 5) { a = a + 1; print a; }",
            "1\n2\n3\n4\n5\n",
        );
    }

    #[test]
    fn test_for() {
        test_output(
            "for (var a = 0; a < 5; a = a + 1) { print a+1; }",
            "1\n2\n3\n4\n5\n",
        );
    }

    #[test]
    fn test_for_no_incr() {
        test_output(
            "for (var a = 0; a < 5;) { a = a + 1; print a; }",
            "1\n2\n3\n4\n5\n",
        );
    }

    #[test]
    fn test_for_with_break() {
        test_output(
            "for (var a = 0; ;) { if (a >= 5) break; a = a + 1; print a; }",
            "1\n2\n3\n4\n5\n",
        );
    }

    #[test]
    fn test_dangling_else() {
        test_output("if (true) if (false) print 0; else print 1;", "1\n");
    }

    fn test_output(source: &str, expected: &str) {
        let tokens = lexer::scan_source(&source.to_string()).unwrap();
        let program = parser::program(tokens, source.to_string());
        let mut interp = interpreter::Interpreter::new();
        interp.set_flush(false);
        interp.run(&program);
        assert_eq!(interp.out, expected.as_bytes());
    }
}

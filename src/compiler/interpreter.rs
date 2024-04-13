use crate::compiler::{
    builtins::builtins,
    environment::Environment,
    errors::ErrorReport,
    function::Callable,
    lexer::{Literal, Token, TokenType},
    object::LoxValue,
    parser::{Expr, Program, Stmt},
};

use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    InvalidOperand {
        operator: Token,
        operand: LoxValue,
    },
    ZeroDivision {
        left: LoxValue,
        operator: Token,
        right: LoxValue,
    },
    TypeError {
        left: LoxValue,
        operator: Token,
        right: LoxValue,
    },
    NameError(Token),
    Break,
    Return(LoxValue),
    NotImplemented,
}

pub struct Interpreter {
    pub globals: Rc<RefCell<Environment>>,
    out: Vec<u8>,
    flush: bool,
    print_expr: bool,
}

impl Interpreter {
    pub fn run(&mut self, prgm: &Program) {
        let mut stmts = prgm.statements.iter();
        while let Some(stmt) = stmts.next() {
            match &self.execute_stmt(stmt, self.globals.clone()) {
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
        let globals = Rc::new(RefCell::new(Environment::new()));
        for builtin_func in builtins().into_iter() {
            match builtin_func {
                LoxValue::Callable(ref func, _) => {
                    globals.borrow_mut().set(&func.name(), builtin_func)
                }
                _ => panic!(),
            }
        }
        Interpreter {
            globals,
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

    pub fn execute_block(
        &mut self,
        stmts: Vec<Stmt>,
        env: Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        for stmt in stmts.iter() {
            self.execute_stmt(stmt, env.clone())?;
        }
        Ok(())
    }

    pub fn execute_stmt(
        &mut self,
        stmt: &Stmt,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        match &stmt {
            Stmt::Print(expr) => {
                let output = self.evaluate(expr, environment)?;
                let _ = writeln!(self.out, "{}", output);
                Ok(())
            }
            Stmt::VarDecl { name, initializer } => {
                match initializer {
                    Some(init) => {
                        let assignment = self.evaluate(init, environment.clone())?;
                        environment.borrow_mut().set(name, assignment);
                    }
                    None => environment.borrow_mut().set(name, LoxValue::Nil),
                }
                Ok(())
            }
            Stmt::FunDecl {
                name,
                parameters,
                statements,
            } => {
                environment.borrow_mut().set(
                    name,
                    LoxValue::Callable(
                        Callable::Function {
                            name: name.clone(),
                            parameters: parameters.clone(),
                            statements: *statements.clone(),
                        },
                        Some(environment.clone()),
                    ),
                );
                Ok(())
            }
            Stmt::Expression(expr) => match expr {
                Expr::Assign { name, value } => {
                    let assignment = self.evaluate(value, environment.clone())?;
                    environment.borrow_mut().update(name, assignment)?;
                    Ok(())
                }
                _ => {
                    let output = self.evaluate(expr, environment)?;
                    if self.print_expr && output != LoxValue::Nil {
                        let _ = writeln!(self.out, "{}", output);
                    }
                    Ok(())
                }
            },
            Stmt::Block(stmts) => {
                let env = Rc::new(RefCell::new(Environment::enclosed(environment)));
                self.execute_block(stmts.to_vec(), env)?;
                Ok(())
            }
            Stmt::Conditional {
                condition,
                true_branch,
                false_branch,
            } => {
                if is_truthy(&self.evaluate(condition, environment.clone())?) {
                    self.execute_stmt(true_branch, environment)
                } else {
                    match false_branch {
                        Some(branch) => self.execute_stmt(branch, environment),
                        None => Ok(()),
                    }
                }
            }
            Stmt::WhileLoop {
                condition,
                statements,
            } => {
                while is_truthy(&self.evaluate(condition, environment.clone())?) {
                    match self.execute_stmt(statements, environment.clone()) {
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
            Stmt::Return(expr) => Err(RuntimeError::Return(
                self.evaluate(expr, environment.clone())?,
            )),
        }
    }

    pub fn evaluate(
        &mut self,
        expr: &Expr,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<LoxValue, RuntimeError> {
        match &expr {
            Expr::Unary { operator, right } => self.eval_unary(operator, right, environment),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.eval_binary(left, operator, right, environment),
            Expr::Grouping(expr) => self.eval_grouping(expr, environment),
            Expr::Literal(token) => self.eval_literal(token, environment),
            Expr::Variable(token) => self.eval_literal(token, environment),
            Expr::Assign { name: _, value: _ } => todo!(),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.eval_logical(left, operator, right, environment),
            Expr::Call {
                callee,
                arguments,
                paren: _,
            } => {
                let callee_obj = self.evaluate(callee, environment.clone())?;
                let (callable, closure) = match callee_obj {
                    LoxValue::Callable(clbe, closure) => (clbe, closure),
                    _ => panic!("{:?} is not callable!", callee_obj),
                    // TODO: Runtime error if not callable
                };
                if arguments.len() != callable.arity() {
                    panic!("Incorrect number of arguments!");
                    // TODO: Runtime error for incorrect # of args
                }
                let mut args = Vec::new();
                for arg in arguments {
                    let arg_val = self.evaluate(arg, environment.clone())?;
                    args.push(arg_val);
                }

                callable.call(self, closure, args)
            }
        }
    }

    fn eval_logical(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<LoxValue, RuntimeError> {
        let right = self.evaluate(right, environment.clone())?;
        let left = self.evaluate(left, environment.clone())?;
        let result = match operator.type_ {
            TokenType::AND => is_truthy(&left) & is_truthy(&right),
            TokenType::OR => is_truthy(&left) | is_truthy(&right),
            _ => panic!("Invalid operator for logical: {}", operator),
        };
        let value = match result {
            true => LoxValue::True,
            false => LoxValue::False,
        };
        Ok(value)
    }

    fn eval_binary(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<LoxValue, RuntimeError> {
        let right = self.evaluate(right, environment.clone())?;
        let left = self.evaluate(left, environment.clone())?;

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
        Ok(value)
    }

    fn eval_grouping(
        &mut self,
        expr: &Expr,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<LoxValue, RuntimeError> {
        self.evaluate(expr, environment)
    }

    fn eval_unary(
        &mut self,
        operator: &Token,
        right: &Expr,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<LoxValue, RuntimeError> {
        let mut right = self.evaluate(right, environment)?;
        right = match operator.type_ {
            TokenType::BANG => match right {
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
            TokenType::MINUS => match right {
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

    fn eval_literal(
        &mut self,
        token: &Token,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<LoxValue, RuntimeError> {
        match &token.type_ {
            TokenType::IDENTIFIER => environment.borrow().get(&token),
            _ => Ok(token.value()),
        }
    }
}

fn is_truthy(obj: &LoxValue) -> bool {
    match &obj {
        LoxValue::False => false,
        LoxValue::Nil => false,
        _ => true,
    }
}

fn compare_lox_value(
    left: &LoxValue,
    operator: &Token,
    right: &LoxValue,
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
        let env = interp.globals.clone();
        let result = interp.eval_logical(
            &true_exp,
            &Token::from_type(TokenType::OR),
            &false_exp,
            env.clone(),
        );
        assert_eq!(result, Ok(LoxValue::True));
        let result = interp.eval_logical(
            &false_exp,
            &Token::from_type(TokenType::OR),
            &false_exp,
            env,
        );
        assert_eq!(result, Ok(LoxValue::False));
    }
    #[test]
    fn test_logical_and() {
        let true_exp = parser::Expr::Literal(Token::from_type(TokenType::TRUE));
        let false_exp = parser::Expr::Literal(Token::from_type(TokenType::FALSE));
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let result = interp.eval_logical(
            &true_exp,
            &Token::from_type(TokenType::AND),
            &true_exp,
            env.clone(),
        );
        assert_eq!(result, Ok(LoxValue::True));
        let result = interp.eval_logical(
            &true_exp,
            &Token::from_type(TokenType::AND),
            &false_exp,
            env,
        );
        assert_eq!(result, Ok(LoxValue::False));
    }

    #[test]
    fn test_eval_unary_not_true() {
        let mut token_iter = token_iter("!true");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let eval = interp.evaluate(&un, env).unwrap();
        assert_eq!(eval, LoxValue::False);
    }
    #[test]
    fn test_eval_unary_not_false() {
        let mut token_iter = token_iter("!false");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let eval = interp.evaluate(&un, env).unwrap();
        assert_eq!(eval, LoxValue::True);
    }
    #[test]
    fn test_eval_unary_not_nil() {
        let mut token_iter = token_iter("!nil");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let eval = interp.evaluate(&un, env).unwrap();
        assert_eq!(eval, LoxValue::True);
    }
    #[test]
    fn test_eval_unary_minus() {
        let mut token_iter = token_iter("---2");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let eval = interp.evaluate(&un, env).unwrap();
        assert_eq!(eval, LoxValue::Number(-2.0));
    }

    #[test]
    fn test_eval_unary_not_invalid() {
        let mut token_iter = token_iter("!2");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let eval = interp.evaluate(&un, env);
        assert_eq!(
            eval,
            Err(RuntimeError::InvalidOperand {
                operator: Token::from_type(TokenType::BANG),
                operand: LoxValue::Number(2.0),
            })
        );
    }

    #[test]
    fn test_eval_unary_minus_invalid() {
        let mut token_iter = token_iter("-true");
        let un = parser::expression(&mut token_iter).unwrap();
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let eval = interp.evaluate(&un, env);
        assert_eq!(
            eval,
            Err(RuntimeError::InvalidOperand {
                operator: Token::from("-"),
                operand: LoxValue::True,
            })
        );
    }

    #[test]
    fn test_zero_div_error() {
        let mut token_iter = token_iter("1/0");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(
            result,
            Err(RuntimeError::ZeroDivision {
                left: LoxValue::Number(1.0),
                operator: Token::from_type(TokenType::SLASH),
                right: LoxValue::Number(0.0),
            })
        )
    }

    #[test]
    fn test_type_before_zero_div_error() {
        let mut token_iter = token_iter("\"anything\"/0");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxValue::StrLit("anything".to_string()),
                operator: Token::from_type(TokenType::SLASH),
                right: LoxValue::Number(0.0),
            })
        )
    }

    #[test]
    fn test_mult() {
        let mut token_iter = token_iter("7*7");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(result, Ok(LoxValue::Number(49.0)))
    }

    #[test]
    fn test_div() {
        let mut token_iter = token_iter("-49/-7");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(result, Ok(LoxValue::Number(7.0)))
    }

    #[test]
    fn test_factor_type_err() {
        let mut token_iter_ = token_iter("-49/\"seven\"");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter_).unwrap();
        let result = interp.evaluate(&expr, env.clone());
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxValue::Number(-49.0),
                operator: Token::from_type(TokenType::SLASH),
                right: LoxValue::StrLit("seven".to_string()),
            })
        );
        let mut token_iter_ = token_iter("true * nil");
        let expr = parser::expression(&mut token_iter_).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxValue::True,
                operator: Token::from_type(TokenType::STAR),
                right: LoxValue::Nil,
            })
        )
    }

    #[test]
    fn test_term_type_err() {
        let mut token_iter = token_iter("-49+true");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxValue::Number(-49.0),
                operator: Token::from_type(TokenType::PLUS),
                right: LoxValue::True,
            })
        )
    }
    #[test]
    fn test_term_addition() {
        let mut token_iter = token_iter("5*8/4 + 8/2");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(result, Ok(LoxValue::Number(14.0)))
    }
    #[test]
    fn test_string_concat() {
        let mut token_iter = token_iter("\"Hello \" + \"World!\"");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(result, Ok(LoxValue::StrLit("Hello World!".to_string())))
    }

    #[test]
    fn test_string_sub_error() {
        let mut token_iter = token_iter("\"Hello \" - \"World!\"");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxValue::StrLit("Hello ".to_string()),
                operator: Token::from_type(TokenType::MINUS),
                right: LoxValue::StrLit("World!".to_string()),
            })
        )
    }

    #[test]
    fn test_cmp() {
        let mut token_iter = token_iter("3>2");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(result, Ok(LoxValue::True))
    }

    #[test]
    fn test_cmp_err() {
        let mut token_iter = token_iter("3>true");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxValue::Number(3.0),
                operator: Token::from_type(TokenType::GREATER),
                right: LoxValue::True,
            })
        )
    }
    #[test]
    fn test_eq() {
        let mut token_iter = token_iter("(2+2)==5");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(result, Ok(LoxValue::False))
    }

    #[test]
    fn test_str_eq() {
        let mut token_iter = token_iter("\"hello\"==\"hello\"");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(result, Ok(LoxValue::True))
    }

    #[test]
    fn test_str_not_eq() {
        let mut token_iter = token_iter("\"parrot\"!=\"alive\"");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(result, Ok(LoxValue::True))
    }

    #[test]
    fn test_str_num_cmp() {
        let mut token_iter = token_iter("\"parrot\"!=33");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxValue::StrLit("parrot".to_string()),
                operator: Token::from_type(TokenType::BANG_EQUAL),
                right: LoxValue::Number(33.0),
            })
        )
    }

    #[test]
    fn test_str_gt_cmp() {
        let mut token_iter = token_iter("\"coffee\"<=\"tea\"");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(
            result,
            Err(RuntimeError::TypeError {
                left: LoxValue::StrLit("coffee".to_string()),
                operator: Token::from_type(TokenType::LESS_EQUAL),
                right: LoxValue::StrLit("tea".to_string()),
            })
        )
    }

    #[test]
    fn test_cmp_bool() {
        let mut token_iter = token_iter("true==false");
        let mut interp = Interpreter::new();
        let env = interp.globals.clone();
        let expr = parser::expression(&mut token_iter).unwrap();
        let result = interp.evaluate(&expr, env);
        assert_eq!(result, Ok(LoxValue::False))
    }

    #[test]
    fn test_nil_var_decl() {
        let mut token_iter = token_iter("var a;").collect();
        let mut interp = Interpreter::new();
        let prgm = parser::program(token_iter, "var a;".to_string());
        let _ = interp.run(&prgm);
        let atok = Token::identifier("a".to_string());
        assert_eq!(interp.globals.borrow().get(&atok), Ok(LoxValue::Nil));
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
        let fib_while = std::fs::read_to_string("tests/fib_while.lox").expect("file should exist");
        test_output(fib_while.as_str(), "12586269025\n")
    }

    #[test]
    fn test_fib_for() {
        let fib_for = std::fs::read_to_string("tests/fib_for.lox").expect("file should exist");
        test_output(fib_for.as_str(), "12586269025\n")
    }

    #[test]
    fn test_is_even() {
        let iseven = std::fs::read_to_string("tests/iseven.lox").expect("file should exist");
        test_output(iseven.as_str(), "True\nFalse\n")
    }

    #[test]
    fn counter() {
        let counter = std::fs::read_to_string("tests/counter.lox").expect("file should exist");
        test_output(counter.as_str(), "1\n2\n")
    }

    #[ignore = "chapter 10 challenge"]
    #[test]
    fn anonymous_func() {
        let anon = std::fs::read_to_string("tests/anonymous_func.lox").expect("file should exist");
        test_output(anon.as_str(), "it's a beautiful day!\n")
    }

    #[ignore = "chapter 10 challenge"]
    #[test]
    fn thrice_anonymous_func() {
        let thrice = std::fs::read_to_string("tests/thrice.lox").expect("file should exist");
        test_output(thrice.as_str(), "1\n2\n3\n")
    }

    #[test]
    fn static_scope() {
        let static_scope =
            std::fs::read_to_string("tests/static_scope.lox").expect("file should exist");
        test_output(static_scope.as_str(), "global\nglobal\n")
    }

    #[test]
    fn test_cond_true() {
        // most basic conditional
        test_output("if (true) print 1;", "1\n");
    }
    #[test]
    fn test_cond_false() {
        // else statement
        test_output("if (false) print 2; else print 1;", "1\n");
        // with braces
        test_output("if (false) { print 2; } else print 1;", "1\n");
    }

    #[test]
    fn test_cond_assignment() {
        // some assignment and more complex cases
        test_output(
            "var a = 0; if (5*5==26) { a = 2; } else { a = 1; } print a;",
            "1\n",
        );
    }

    #[test]
    fn test_cond_assignment_2() {
        // else if
        test_output(
            "var a = 0; var b = 5; if (b*5==26) { a = 2; } else if (b == 5) { a = 1; } else { a = 4; } print a;",
            "1\n",
        );

        // nesting
        test_output(
            "if (true) { if (true) { if (false) { print 2; } else { print 1; } } }",
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

    #[test]
    fn fun_return_from_loop() {
        assert_eq!(
            fun_fixt(
                "fun loop5() { var a = 0; while (true) { if (a == 5) return 5; a = a + 1; }}",
                vec![]
            ),
            Ok(LoxValue::Number(5.0)),
        );
    }
    #[test]
    fn test_fn_eval_envs() {
        test_output(
            "fun three(x) { return 3*x; } var x = 1; print three(x)+three(x-1);",
            "3\n",
        );
    }

    fn test_output(source: &str, expected: &str) {
        let tokens = lexer::scan_source(&source.to_string()).unwrap();
        let program = parser::program(tokens, source.to_string());
        assert!(&program.errors.is_empty());
        let mut interp = interpreter::Interpreter::new();
        interp.set_flush(false);
        interp.run(&program);
        assert_eq!(interp.out, expected.as_bytes());
    }

    #[test]
    fn fun_call() {
        assert_eq!(
            fun_fixt("fun one() { return 1; }", vec![]),
            Ok(LoxValue::Number(1.0))
        );
    }

    #[test]
    fn fib_fun_recursive() {
        assert_eq!(
            fun_fixt(
                "fun fib(n) { if (n <= 1) return n; return fib(n-2) + fib(n-1); }",
                vec![LoxValue::Number(9.0)]
            ),
            Ok(LoxValue::Number(34.0))
        );
    }

    #[test]
    fn fun_add() {
        assert_eq!(
            fun_fixt(
                "fun additionetc(a, b) { return a+b; }",
                vec![LoxValue::Number(1.0), LoxValue::Number(3.0)],
            ),
            Ok(LoxValue::Number(4.0)),
        );
    }

    /// Test the result of a function call in the interpreter. The `fn_decl` must be the
    /// declaration for a single function only. The args should be the expected evaluation results
    /// of expressions.
    /// ```
    ///assert_eq!(
    ///    fun_fixt(
    ///        "fun add(a, b) { return a+b; }",
    ///        vec![LoxValue::Number(1.0), LoxValue::Number(3.0)],
    ///    ),
    ///    Ok(LoxValue::Number(4.0)),
    ///);
    /// ```
    fn fun_fixt(fn_decl: &str, args: Vec<LoxValue>) -> Result<LoxValue, RuntimeError> {
        // scan, parse, and run the interpreter so that a function call is defined.
        let tokens = lexer::scan_source(&fn_decl.to_string()).unwrap();
        let program = parser::program(tokens, fn_decl.to_string());
        let mut interp = interpreter::Interpreter::new();
        interp.set_flush(false);
        interp.run(&program);

        // turn the arguments into expressions that can be passed to the function call
        let arguments: Vec<Expr> = args
            .iter()
            .map(|arg| {
                Expr::Literal(match arg {
                    LoxValue::Number(n) => Token::numlit(*n),
                    LoxValue::StrLit(s) => Token::stringlit(s.to_string()),
                    LoxValue::Nil => Token::from_type(TokenType::NIL),
                    LoxValue::False => Token::from_type(TokenType::FALSE),
                    LoxValue::True => Token::from_type(TokenType::TRUE),
                    _ => todo!(), // TODO: add callable support
                })
            })
            .collect();

        let fn_name = match &program.statements[0] {
            Stmt::FunDecl { name, .. } => name.clone(),
            _ => panic!("Expect first statement to be function declaration!"),
        };

        // construct the function call
        let fn_call = Expr::Call {
            callee: Box::new(Expr::Literal(fn_name)),
            arguments,
            paren: Token::from_type(TokenType::RIGHT_PAREN),
        };

        // call the function in the interpreter and return it
        let binding = interp.globals.borrow().clone();
        interp.evaluate(&fn_call, Rc::new(RefCell::new(binding)))
    }
}

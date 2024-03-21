use crate::compiler::{
    environment::Environment,
    interpreter::{Interpreter, RuntimeError},
    lexer::Token,
    object::LoxValue,
    parser::{Expr, Stmt},
};

use std::{cell::RefCell, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum Callable {
    Function {
        arity: u8,
        name: Token,
        parameters: Vec<Token>,
        statements: Stmt,
    },
}

impl Callable {
    pub fn name(&self) -> String {
        "bob".to_string()
    }

    pub fn arity(&self) -> u8 {
        match &self {
            Callable::Function { arity, .. } => *arity,
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        environment: Rc<RefCell<Environment>>,
        args: Vec<Expr>,
    ) -> Result<LoxValue, RuntimeError> {
        match &self {
            Callable::Function {
                arity,
                name: _,
                parameters,
                statements,
            } => {
                if args.len() as u8 != *arity {
                    panic!("Incorrect number of arguments!");
                    // TODO: Runtime error for incorrect # of args
                }
                let mut env = Environment::enclosed(environment.clone());
                for arg in std::iter::zip(parameters, args) {
                    // TODO: Consider evaluating all arguments individually
                    // BEFORE setting them in the environment?
                    let arg_value = interpreter.evaluate(&arg.1, environment.clone())?;
                    env.set(&arg.0, arg_value);
                }
                match interpreter
                    .execute_block(vec![statements.clone()], Rc::new(RefCell::new(env.clone())))
                {
                    Ok(_) => Ok(LoxValue::Nil),
                    Err(err) => match err {
                        RuntimeError::Return(expr) => {
                            let retval = interpreter.evaluate(&expr, Rc::new(RefCell::new(env)))?;
                            Ok(retval)
                        }
                        _ => Err(err),
                    },
                }
            }
        }
    }
}

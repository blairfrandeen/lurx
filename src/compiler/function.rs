use crate::compiler::{
    environment::Environment,
    interpreter::{Interpreter, RuntimeError},
    lexer::Token,
    object::LoxValue,
    parser::Stmt,
};

use std::{cell::RefCell, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum Callable {
    Function {
        name: Token,
        parameters: Vec<Token>,
        statements: Stmt,
    },
    BuiltIn {
        name: Token,
        parameters: Vec<LoxValue>,
        function: fn(&[LoxValue]) -> LoxValue,
    },
}

impl Callable {
    pub fn name(&self) -> Token {
        match &self {
            Callable::Function { name, .. } => name.clone(),
            Callable::BuiltIn { name, .. } => name.clone(),
        }
    }

    pub fn arity(&self) -> usize {
        match &self {
            Callable::Function { parameters, .. } => parameters.len(),
            Callable::BuiltIn { parameters, .. } => parameters.len(),
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        environment: Option<Rc<RefCell<Environment>>>,
        args: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        match &self {
            Callable::Function {
                parameters,
                statements,
                ..
            } => {
                let mut env = match environment {
                    Some(env) => Environment::enclosed(env.clone()),
                    None => interpreter.globals.borrow().clone(),
                };
                for arg in std::iter::zip(parameters, args) {
                    // TODO: Consider evaluating all arguments individually
                    // BEFORE setting them in the environment?
                    env.set(&arg.0, arg.1);
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
            Callable::BuiltIn {
                parameters,
                function,
                ..
            } => Ok(function(parameters)),
        }
    }
}

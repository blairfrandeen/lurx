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
                let call_env_rc = Rc::new(RefCell::new(match environment {
                    Some(env) => Environment::enclosed(env),
                    None => interpreter.globals.borrow().clone(),
                }));
                for arg in std::iter::zip(parameters, args) {
                    // TODO: Consider evaluating all arguments individually
                    // BEFORE setting them in the environment?
                    call_env_rc.borrow_mut().set(&arg.0, arg.1);
                }
                let result =
                    interpreter.execute_block(vec![statements.clone()], call_env_rc.clone());
                match result {
                    Ok(_) => Ok(LoxValue::Nil),
                    Err(err) => match err {
                        RuntimeError::Return(retval) => Ok(retval),
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

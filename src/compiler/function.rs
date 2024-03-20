use crate::compiler::environment::Environment;
use crate::compiler::interpreter::{Interpreter, RuntimeError};
use crate::compiler::lexer::Token;
use crate::compiler::object::LoxValue;
use crate::compiler::parser::Stmt;

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

    #[allow(unused)]
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        environment: Environment,
        args: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        match &self {
            Callable::Function {
                arity: _,
                name: _,
                parameters,
                statements,
            } => {
                todo!()
                // let mut env = environment.enclosed();
                // for arg in std::iter::zip(parameters, args) {
                //     env.set(&arg.0, arg.1);
                // }
                // interpreter.execute_stmt(&statements)?;
            }
        }
        Ok(LoxValue::Nil)
    }
}

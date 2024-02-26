use crate::compiler::lexer::Token;
use crate::compiler::parser::Stmt;
use crate::compiler::LoxFloat;

use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct LoxObject {
    pub value: LoxValue,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LoxValue {
    StrLit(String),
    Number(LoxFloat),
    True,
    False,
    Nil,
    Callable(LoxCallable),
}

#[allow(unused)]
#[derive(Debug, PartialEq, Clone)]
pub struct LoxCallable {
    arity: u8,
    name: Token,
    parameters: Vec<Token>,
    statements: Stmt,
}

#[allow(unused)]
impl LoxObject {
    pub fn callable(name: Token, parameters: Vec<Token>, statements: Stmt) -> Self {
        let arity = parameters.len() as u8; // TODO: Check for too many params
        let value = LoxValue::Callable(LoxCallable {
            arity,
            name,
            parameters,
            statements,
        });
        LoxObject { value }
    }
}

impl PartialOrd for LoxValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let self_value = match self {
            LoxValue::Number(n) => n,
            _ => return None,
        };
        let other_value = match other {
            LoxValue::Number(n) => n,
            _ => return None,
        };
        self_value.partial_cmp(other_value)
    }
}

impl LoxObject {
    pub fn is_number(&self) -> bool {
        match &self.value {
            LoxValue::Number(_) => true,
            _ => false,
        }
    }
    pub fn number(&self) -> Option<LoxFloat> {
        match &self.value {
            LoxValue::Number(n) => Some(*n),
            _ => None,
        }
    }
    pub fn is_str(&self) -> bool {
        match &self.value {
            LoxValue::StrLit(_) => true,
            _ => false,
        }
    }
    pub fn strlit(&self) -> Option<&String> {
        match &self.value {
            LoxValue::StrLit(s) => Some(s),
            _ => None,
        }
    }
    pub fn is_bool(&self) -> bool {
        match &self.value {
            LoxValue::True => true,
            LoxValue::False => true,
            _ => false,
        }
    }
}

impl Display for LoxObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self {
            LoxValue::StrLit(s) => write!(f, "{}", s)?,
            LoxValue::Number(n) => write!(f, "{}", n)?,
            LoxValue::True => write!(f, "True")?,
            LoxValue::False => write!(f, "False")?,
            LoxValue::Nil => write!(f, "Nil")?,
            LoxValue::Callable(callable) => write!(f, "<function {}>", callable.name)?,
        }
        Ok(())
    }
}

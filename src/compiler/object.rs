use crate::compiler::{environment::Environment, function::Callable, LoxFloat};

use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    rc::Rc,
};

#[derive(Debug, PartialEq, Clone)]
pub enum LoxValue {
    StrLit(String),
    Number(LoxFloat),
    True,
    False,
    Nil,
    Callable(Callable, Option<Rc<RefCell<Environment>>>),
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

impl LoxValue {
    pub fn is_number(&self) -> bool {
        match &self {
            LoxValue::Number(_) => true,
            _ => false,
        }
    }
    pub fn number(&self) -> Option<LoxFloat> {
        match &self {
            LoxValue::Number(n) => Some(*n),
            _ => None,
        }
    }
    pub fn is_str(&self) -> bool {
        match &self {
            LoxValue::StrLit(_) => true,
            _ => false,
        }
    }
    pub fn strlit(&self) -> Option<&String> {
        match &self {
            LoxValue::StrLit(s) => Some(s),
            _ => None,
        }
    }
    pub fn is_bool(&self) -> bool {
        match &self {
            LoxValue::True => true,
            LoxValue::False => true,
            _ => false,
        }
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
            LoxValue::Callable(callable, _) => match callable {
                Callable::Function { name, .. } => write!(f, "<function {}>", name)?,
                Callable::BuiltIn { name, .. } => write!(f, "<builtin function {}>", name)?,
            },
        }
        Ok(())
    }
}

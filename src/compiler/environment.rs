use std::cell::RefCell;
use std::collections::HashMap;

use crate::compiler::interpreter::RuntimeError;
use crate::compiler::lexer::{Literal, Token};
use crate::compiler::object::LoxValue;

#[derive(Debug, Clone)]
pub struct Environment {
    data: RefCell<HashMap<String, LoxValue>>,
    pub enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn get(&self, name: &Token) -> Result<LoxValue, RuntimeError> {
        match self.data.borrow().get(&Self::get_ident(&name)) {
            Some(obj) => Ok(obj.clone()),
            None => match &self.enclosing {
                Some(enc) => enc.get(&name),
                None => Err(RuntimeError::NameError(name.clone())),
            },
        }
    }

    pub fn set(&self, name: &Token, value: LoxValue) {
        let _ = &self.data.borrow_mut().insert(Self::get_ident(&name), value);
    }

    pub fn update(&self, name: &Token, value: LoxValue) -> Result<(), RuntimeError> {
        let has_key = &self.data.borrow().contains_key(&Self::get_ident(&name));
        match has_key {
            true => {
                let _ = &self.data.borrow_mut().insert(Self::get_ident(&name), value);
                Ok(())
            }
            false => match &self.enclosing {
                Some(enc) => enc.update(name, value),
                None => Err(RuntimeError::NameError(name.clone())),
            },
        }
    }

    pub fn new() -> Self {
        Environment {
            data: HashMap::new().into(),
            enclosing: None,
        }
    }

    pub fn enclosed(self) -> Self {
        Environment {
            data: HashMap::new().into(),
            enclosing: Some(Box::new(self)),
        }
    }

    // pub fn enclosing(&self) -> Option<Self> {
    //     match &self.enclosing {
    //         Some(enc) => Some(*enc),
    //         None => None,
    //     }
    // }

    fn get_ident(name: &Token) -> String {
        match name
            .literal
            .as_ref()
            .expect("Attempt to get name from invalid token type!")
        {
            Literal::Ident(id) => id.to_string(),
            _ => panic!("Attempt to get name from invalid token type!"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::object::*;

    fn env_fixture() -> Environment {
        let var_a = LoxValue::Number(7.0);
        let var_b = LoxValue::True;
        let mut global_data = HashMap::new();
        global_data.insert("a".to_string(), var_a);
        let mut local_data = HashMap::new();
        local_data.insert("b".to_string(), var_b);
        let global = Environment {
            data: global_data.into(),
            enclosing: None,
        };
        let local = Environment {
            data: local_data.into(),
            enclosing: Some(Box::new(global)),
        };

        local
    }

    #[test]
    fn test_get_from_local() {
        let local = env_fixture();
        let target = Token::identifier("b".to_string());
        let expected = LoxValue::True;
        assert_eq!(local.get(&target), Ok(expected));
    }

    #[test]
    fn test_get_from_global() {
        let local = env_fixture();
        let target = Token::identifier("a".to_string());
        let expected = LoxValue::Number(7.0);
        assert_eq!(local.get(&target), Ok(expected));
    }

    #[test]
    fn test_err_from_local() {
        let local = env_fixture();
        let target = Token::identifier("q".to_string());
        assert_eq!(local.get(&target), Err(RuntimeError::NameError(target)));
    }

    #[test]
    fn test_set() {
        let local = env_fixture();
        let c_value = LoxValue::Number(3.0);
        let c_token = Token::identifier("c".to_string());
        local.set(&c_token, c_value.clone());
        assert_eq!(local.get(&c_token), Ok(c_value));
    }

    #[test]
    fn test_scope() {
        let local = env_fixture();
        let a_value = LoxValue::Number(3.0);
        let a_token = Token::identifier("a".to_string());
        local.set(&a_token, a_value.clone());
        assert_eq!(local.get(&a_token), Ok(a_value));
    }
}

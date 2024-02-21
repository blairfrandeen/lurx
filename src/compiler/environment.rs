use std::collections::HashMap;

use crate::compiler::interpreter::RuntimeError;
use crate::compiler::lexer::{Literal, Token};
use crate::compiler::object::LoxObject;

#[derive(Debug)]
pub struct Environment {
    data: HashMap<String, LoxObject>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn get(&self, name: &Token) -> Result<&LoxObject, RuntimeError> {
        match &self.data.get(&Self::get_ident(&name)) {
            Some(obj) => Ok(*obj),
            None => match &self.enclosing {
                Some(enc) => enc.get(&name),
                None => Err(RuntimeError::NameError(name.clone())),
            },
        }
    }

    pub fn set(&mut self, name: &Token, value: LoxObject) {
        let _ = &self.data.insert(Self::get_ident(&name), value);
    }

    pub fn new() -> Self {
        Environment {
            data: HashMap::new(),
            enclosing: None,
        }
    }

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
        let var_a = LoxObject {
            value: LoxValue::Number(7.0),
        };
        let var_b = LoxObject {
            value: LoxValue::True,
        };
        let mut global_data = HashMap::new();
        global_data.insert("a".to_string(), var_a);
        let mut local_data = HashMap::new();
        local_data.insert("b".to_string(), var_b);
        let global = Environment {
            data: global_data,
            enclosing: None,
        };
        let local = Environment {
            data: local_data,
            enclosing: Some(Box::new(global)),
        };

        local
    }

    #[test]
    fn test_get_from_local() {
        let local = env_fixture();
        let target = Token::identifier("b".to_string());
        let expected = LoxObject {
            value: LoxValue::True,
        };
        assert_eq!(local.get(&target), Ok(&expected));
    }

    #[test]
    fn test_get_from_global() {
        let local = env_fixture();
        let target = Token::identifier("a".to_string());
        let expected = LoxObject {
            value: LoxValue::Number(7.0),
        };
        assert_eq!(local.get(&target), Ok(&expected));
    }

    #[test]
    fn test_err_from_local() {
        let local = env_fixture();
        let target = Token::identifier("q".to_string());
        assert_eq!(local.get(&target), Err(RuntimeError::NameError(target)));
    }

    #[test]
    fn test_set() {
        let mut local = env_fixture();
        let c_value = LoxObject {
            value: LoxValue::Number(3.0),
        };
        let c_token = Token::identifier("c".to_string());
        local.set(&c_token, c_value.clone());
        assert_eq!(local.get(&c_token), Ok(&c_value));
    }

    #[test]
    fn test_scope() {
        let mut local = env_fixture();
        let a_value = LoxObject {
            value: LoxValue::Number(3.0),
        };
        let a_token = Token::identifier("a".to_string());
        local.set(&a_token, a_value.clone());
        assert_eq!(local.get(&a_token), Ok(&a_value));
    }
}

use std::collections::HashMap;

use crate::compiler::interpreter::{LoxObject, RuntimeError};
use crate::compiler::lexer::{Literal, Token};

pub struct Environment {
    data: HashMap<String, LoxObject>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn get(&self, name: &Token) -> Result<&LoxObject, RuntimeError> {
        let ident = match name
            .literal
            .as_ref()
            .expect("Attempt to get name from invalid token type!")
        {
            Literal::Ident(id) => id.to_string(),
            _ => panic!("Attempt to get name from invalid token type!"),
        };
        match &self.data.get(&ident) {
            Some(obj) => Ok(*obj),
            None => match &self.enclosing {
                Some(enc) => enc.get(&name),
                None => Err(RuntimeError::NameError(name.clone())),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::LoxValue;

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
}

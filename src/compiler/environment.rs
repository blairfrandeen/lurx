use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

use crate::compiler::interpreter::RuntimeError;
use crate::compiler::lexer::Token;
use crate::compiler::object::LoxValue;

#[derive(PartialEq, Clone)]
pub struct Environment {
    pub data: HashMap<String, LoxValue>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn get(&self, name: &Token) -> Result<LoxValue, RuntimeError> {
        match self.data.get(name.ident()) {
            Some(obj) => Ok(obj.clone()),
            None => match &self.enclosing {
                Some(enc) => enc.borrow().get(&name),
                None => Err(RuntimeError::NameError(name.clone())),
            },
        }
    }

    pub fn get_at(&self, distance: usize, name: &Token) -> LoxValue {
        if distance > 0 {
            return self
                .enclosing
                .clone()
                .expect("Enclosing environment should exist per resolver")
                .borrow()
                .get_at(distance - 1, name);
        }
        self.data
            .get(name.ident())
            .expect("Name should exist per resolver")
            .clone()
    }

    pub fn set_at(&mut self, distance: usize, name: &Token, value: LoxValue) {
        if distance > 0 {
            return self
                .enclosing
                .clone()
                .expect("Enclosing environment should exist per resolver")
                .borrow_mut()
                .set_at(distance - 1, name, value);
        }
        self.update(name, value)
            .expect("Name should exist per resolver");
    }

    pub fn set(&mut self, name: &Token, value: LoxValue) {
        let _ = &self.data.insert(name.ident().to_string(), value);
    }

    pub fn update(&mut self, name: &Token, value: LoxValue) -> Result<(), RuntimeError> {
        let has_key = &self.data.contains_key(name.ident());
        match has_key {
            true => {
                let _ = &self.data.insert(name.ident().to_string(), value);
                Ok(())
            }
            false => match &mut self.enclosing {
                Some(enc) => enc.borrow_mut().update(name, value),
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

    pub fn enclosed(enclosing: Rc<RefCell<Environment>>) -> Self {
        Environment {
            data: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }
}
impl Debug for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_with_indent(0))
    }
}

impl Environment {
    fn fmt_with_indent(&self, indent_level: usize) -> String {
        let indent = "  ".repeat(indent_level);
        let mut data_string = String::new();
        if !self.data.is_empty() {
            data_string.push('\n');
            for key in self.data.keys() {
                let val = self.data.get(key).unwrap();
                data_string = format!("{data_string}{indent}  {key:<12} {val}\n")
            }
        } else {
            data_string = "None".to_string();
        }
        let enc_string = match &self.enclosing {
            Some(enc) => format!("{}", enc.borrow().fmt_with_indent(indent_level + 1)),
            None => "None".to_string(),
        };
        format!(
            "{}Environment: \n{}data: {}\n{}enclosing: {}",
            indent, indent, data_string, indent, enc_string,
        )
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
            enclosing: Some(Rc::new(RefCell::new(global))),
        };

        local
    }

    #[should_panic]
    #[test]
    fn get_at_panic() {
        let local = env_fixture();
        let _ = local.get_at(2, &Token::identifier("b".to_string()));
    }

    #[test]
    fn get_at() {
        let local = env_fixture();
        let mut inner_data = HashMap::new();
        let var_c = LoxValue::False;
        inner_data.insert("c".to_string(), var_c);
        let inner = Environment {
            data: inner_data,
            enclosing: Some(Rc::new(RefCell::new(local))),
        };

        assert_eq!(
            inner.get_at(0, &Token::identifier("c".to_string())),
            LoxValue::False
        );
        assert_eq!(
            inner.get_at(1, &Token::identifier("b".to_string())),
            LoxValue::True
        );
        assert_eq!(
            inner.get_at(2, &Token::identifier("a".to_string())),
            LoxValue::Number(7.0)
        );
    }

    #[should_panic]
    #[test]
    fn set_at_panic() {
        let mut local = env_fixture();
        local.set_at(5, &Token::identifier("q".to_string()), LoxValue::Nil);
    }

    #[test]
    fn set_at() {
        let local = env_fixture();
        let mut inner_data = HashMap::new();
        let var_c = LoxValue::False;
        inner_data.insert("c".to_string(), var_c);
        let mut inner = Environment {
            data: inner_data,
            enclosing: Some(Rc::new(RefCell::new(local))),
        };

        inner.set_at(0, &Token::identifier("c".to_string()), LoxValue::Nil);
        inner.set_at(1, &Token::identifier("b".to_string()), LoxValue::Nil);
        inner.set_at(2, &Token::identifier("a".to_string()), LoxValue::Nil);
        assert_eq!(
            inner.get_at(0, &Token::identifier("c".to_string())),
            LoxValue::Nil
        );
        assert_eq!(
            inner.get_at(1, &Token::identifier("b".to_string())),
            LoxValue::Nil
        );
        assert_eq!(
            inner.get_at(2, &Token::identifier("a".to_string())),
            LoxValue::Nil
        );
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
        let mut local = env_fixture();
        let c_value = LoxValue::Number(3.0);
        let c_token = Token::identifier("c".to_string());
        local.set(&c_token, c_value.clone());
        assert_eq!(local.get(&c_token), Ok(c_value));
    }

    #[test]
    fn test_scope() {
        let mut local = env_fixture();
        let a_value = LoxValue::Number(3.0);
        let a_token = Token::identifier("a".to_string());
        local.set(&a_token, a_value.clone());
        assert_eq!(local.get(&a_token), Ok(a_value));
    }
}

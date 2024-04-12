use crate::compiler::{
    environment::Environment, function::Callable, lexer::Token, object::LoxValue, LoxFloat,
};

use std::{
    cell::RefCell,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

pub fn builtins(env: Rc<RefCell<Environment>>) -> Vec<LoxValue> {
    let mut builtin_vec = Vec::new();
    builtin_vec.push(clock());
    builtin_vec
}

fn clock() -> LoxValue {
    let name = Token::identifier("clock".to_string());
    LoxValue::Callable(
        Callable::BuiltIn {
            name,
            parameters: vec![],
            function: clock_impl,
        },
        None,
    )
}

fn clock_impl(_args: &[LoxValue]) -> LoxValue {
    LoxValue::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Before UNIX_EPOCH!")
            .as_secs_f64() as LoxFloat,
    )
}

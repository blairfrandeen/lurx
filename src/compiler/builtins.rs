use crate::compiler::lexer::Token;
use crate::compiler::object::{LoxCallable, LoxValue};
use crate::compiler::parser::{Expr, Stmt};
use crate::compiler::LoxFloat;

use std::time::{SystemTime, UNIX_EPOCH};

pub fn builtins() -> Vec<(Token, LoxValue)> {
    let mut builtin_vec = Vec::new();
    builtin_vec.push(clock());
    builtin_vec
}

fn clock() -> (Token, LoxValue) {
    let name = Token::identifier("clock".to_string());
    let t = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Before UNIX_EPOCH!")
        .as_secs_f64();
    let clock_fun = LoxCallable {
        arity: 0,
        name: name.clone(),
        parameters: vec![],
        statements: Stmt::Print(Expr::Literal(Token::numlit(t as LoxFloat))),
    };

    (name, LoxValue::Callable(clock_fun))
}
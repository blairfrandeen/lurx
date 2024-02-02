#![allow(unused)]
use crate::compiler::parser;

pub enum RuntimeError {}

pub struct LoxObject {
    type_: LoxType,
}

enum LoxType {
    StrLit(String),
    Number(f32),
    True,
    False,
    Nil,
}

pub trait Evaluate {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError>;
}

impl Evaluate for parser::Primary {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError> {
        let lox_type = match &self {
            parser::Primary::Number(val) => LoxType::Number(*val),
            parser::Primary::StrLit(strlit) => LoxType::StrLit(strlit.clone()),
            parser::Primary::True => LoxType::True,
            parser::Primary::False => LoxType::False,
            parser::Primary::Nil => LoxType::Nil,
            _ => todo!(),
        };
        Ok(LoxObject { type_: lox_type })
    }
}

impl Evaluate for parser::Unary {
    fn evaluate(&self) -> Result<LoxObject, RuntimeError> {
        todo!()
    }
}

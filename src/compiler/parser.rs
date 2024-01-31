#![allow(unused)]
use crate::lexer::{Token, TokenType};

use std::iter::{Iterator, Peekable};

#[derive(Debug, PartialEq)]
pub enum ParseError {}

pub trait Parse {
    fn parse<'a>(
        tokens: &mut Peekable<impl Iterator<Item = &'a Token>>,
    ) -> Result<Self, ParseError>
    where
        Self: Sized;
}

pub fn parse_tokens(tokens: Vec<Token>) {
    let mut token_iter = tokens.iter().peekable();
    let res = Primary::parse(&mut token_iter);
    dbg!(res);
}

#[derive(Debug, PartialEq)]
enum Expression {
    Equality(Equality),
    // TODO!
}

#[derive(Debug, PartialEq)]
struct Equality {
    comparison: Comparison,
    components: Vec<EqualityComponent>,
}

#[derive(Debug, PartialEq)]
enum EqualityComponent {
    Equals(Comparison),
    NotEquals(Comparison),
}

#[derive(Debug, PartialEq)]
struct Comparison {
    term: Term,
    components: Vec<ComparisonComponent>,
}

#[derive(Debug, PartialEq)]
enum ComparisonComponent {
    Greater(Term),
    GreaterEquals(Term),
    Less(Term),
    LessEquals(Term),
}

#[derive(Debug, PartialEq)]
struct Term {
    factor: Factor,
    components: Vec<TermComponent>,
}

#[derive(Debug, PartialEq)]
enum TermComponent {
    Add(Factor),
    Sub(Factor),
}

#[derive(Debug, PartialEq)]
struct Factor {
    unary: Unary,
    components: Vec<FactorComponent>,
}

#[derive(Debug, PartialEq)]
enum FactorComponent {
    Mul(Unary),
    Div(Unary),
}

#[derive(Debug, PartialEq)]
enum Unary {
    Minus(Box<Unary>),
    Not(Box<Unary>),
    Primary(Primary),
}

#[derive(Debug, PartialEq)]
enum Primary {
    Number(f32),
    StrLit(String),
    True,
    False,
    Nil,
    Group(Box<Expression>),
}

impl Parse for Primary {
    fn parse<'a>(
        tokens: &mut Peekable<impl Iterator<Item = &'a Token>>,
    ) -> Result<Self, ParseError> {
        if let Some(next_token) = tokens.next() {
            match next_token.type_ {
                TokenType::NUMLIT => {
                    let value = match next_token
                        .literal
                        .as_ref()
                        .expect("NUMLIT token without an attached value!")
                    {
                        crate::compiler::lexer::Literal::NumLit(num) => num,
                        _ => panic!("NUMLIT token with incorrect literal type!"),
                    };
                    Ok(Primary::Number(*value))
                }
                TokenType::STRINGLIT => {
                    let value = match next_token
                        .literal
                        .as_ref()
                        .expect("STRINGLIT token without an attached value!")
                    {
                        crate::compiler::lexer::Literal::StringLit(strlit) => strlit,
                        _ => panic!("STRINGLIT token with incorrect literal type!"),
                    };
                    Ok(Primary::StrLit(value.clone()))
                }
                TokenType::FALSE => Ok(Primary::False),
                TokenType::TRUE => Ok(Primary::True),
                TokenType::NIL => Ok(Primary::Nil),
                TokenType::LEFT_PAREN => todo!(),
                _ => todo!(),
            }
        } else {
            panic!("unexpected EOF!");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_primary() {
        let primary_source = String::from("true false nil 55 \"hello\"");
        let primary_tokens = crate::lexer::scan_source(&primary_source).unwrap();
        let mut token_iter = primary_tokens.iter().peekable();
        assert_eq!(Primary::parse(&mut token_iter), Ok(Primary::True));
        assert_eq!(Primary::parse(&mut token_iter), Ok(Primary::False));
        assert_eq!(Primary::parse(&mut token_iter), Ok(Primary::Nil));
        let next = Primary::parse(&mut token_iter);
        match next {
            Ok(Primary::Number(n)) => assert_eq!(n, 55.0),
            _ => panic!(),
        }
        let next = Primary::parse(&mut token_iter);
        match next {
            Ok(Primary::StrLit(n)) => assert_eq!(n, "hello".to_string()),
            _ => panic!(),
        }
        assert_eq!(
            token_iter.next().unwrap().type_,
            crate::lexer::TokenType::EOF
        );
    }
}

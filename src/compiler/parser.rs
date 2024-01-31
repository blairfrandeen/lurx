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
    let res = Unary::parse(&mut token_iter);
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

impl Parse for Unary {
    fn parse<'a>(
        tokens: &mut Peekable<impl Iterator<Item = &'a Token>>,
    ) -> Result<Self, ParseError> {
        if let Some(next_token) = tokens.peek() {
            let unary = match next_token.type_ {
                TokenType::MINUS => {
                    tokens.next();
                    Unary::Minus(Box::new(Unary::parse(tokens)?))
                }
                TokenType::BANG => {
                    tokens.next();
                    Unary::Not(Box::new(Unary::parse(tokens)?))
                }
                _ => Unary::Primary(Primary::parse(tokens)?),
            };
            Ok(unary)
        } else {
            panic!("unexpected EOF!");
        }
    }
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
        if let Some(next_token) = tokens.peek() {
            let primary = match next_token.type_ {
                TokenType::NUMLIT => {
                    let value = match next_token
                        .literal
                        .as_ref()
                        .expect("NUMLIT token without an attached value!")
                    {
                        crate::compiler::lexer::Literal::NumLit(num) => num,
                        _ => panic!("NUMLIT token with incorrect literal type!"),
                    };
                    Primary::Number(*value)
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
                    Primary::StrLit(value.clone())
                }
                TokenType::FALSE => Primary::False,
                TokenType::TRUE => Primary::True,
                TokenType::NIL => Primary::Nil,
                TokenType::LEFT_PAREN => todo!(),
                _ => todo!(),
            };
            tokens.next();
            Ok(primary)
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

    #[test]
    fn test_unary_primary() {
        let unary_source = String::from("true false nil 55 \"hello\"");
        let unary_tokens = crate::lexer::scan_source(&unary_source).unwrap();
        let mut token_iter = unary_tokens.iter().peekable();
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Primary(Primary::True))
        );
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Primary(Primary::False))
        );
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Primary(Primary::Nil))
        );
        match Unary::parse(&mut token_iter) {
            Ok(Unary::Primary(Primary::Number(n))) => assert_eq!(n, 55.0),
            _ => panic!(),
        }
        match Unary::parse(&mut token_iter) {
            Ok(Unary::Primary(Primary::StrLit(n))) => assert_eq!(n, "hello".to_string()),
            _ => panic!(),
        }
        assert_eq!(
            token_iter.next().unwrap().type_,
            crate::lexer::TokenType::EOF
        );
    }

    #[test]
    fn test_unary_not() {
        let unary_source = String::from("!!true");
        let unary_tokens = crate::lexer::scan_source(&unary_source).unwrap();
        let mut token_iter = unary_tokens.iter().peekable();
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Not(Box::new(Unary::Not(Box::new(Unary::Primary(
                Primary::True
            ))))))
        );
    }

    #[test]
    fn test_unary_minus() {
        let unary_source = String::from("-5");
        let unary_tokens = crate::lexer::scan_source(&unary_source).unwrap();
        let mut token_iter = unary_tokens.iter().peekable();
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Minus(Box::new(Unary::Primary(Primary::Number(5.0)))))
        );
    }
}

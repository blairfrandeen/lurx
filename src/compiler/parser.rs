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
    let res = Equality::parse(&mut token_iter);
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

impl Parse for Equality {
    fn parse<'a>(
        tokens: &mut Peekable<impl Iterator<Item = &'a Token>>,
    ) -> Result<Self, ParseError> {
        let comparison = Comparison::parse(tokens)?;
        let mut components: Vec<EqualityComponent> = Vec::new();

        while let Some(next_token) = tokens.next_if(|tok| {
            (tok.type_ == TokenType::EQUAL_EQUAL) | (tok.type_ == TokenType::BANG_EQUAL)
        }) {
            match next_token.type_ {
                TokenType::EQUAL_EQUAL => {
                    components.push(EqualityComponent::Equals(Comparison::parse(tokens)?))
                }
                TokenType::BANG_EQUAL => {
                    components.push(EqualityComponent::NotEquals(Comparison::parse(tokens)?))
                }
                _ => panic!("unexpected token!"),
            }
        }
        Ok(Equality {
            comparison,
            components,
        })
    }
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

impl Parse for Comparison {
    fn parse<'a>(
        tokens: &mut Peekable<impl Iterator<Item = &'a Token>>,
    ) -> Result<Self, ParseError> {
        let term = Term::parse(tokens)?;
        let mut components: Vec<ComparisonComponent> = Vec::new();
        while let Some(next_token) = tokens.next_if(|tok| {
            (tok.type_ == TokenType::GREATER)
                | (tok.type_ == TokenType::GREATER_EQUAL)
                | (tok.type_ == TokenType::LESS)
                | (tok.type_ == TokenType::LESS_EQUAL)
        }) {
            match next_token.type_ {
                TokenType::GREATER => {
                    components.push(ComparisonComponent::Greater(Term::parse(tokens)?))
                }
                TokenType::GREATER_EQUAL => {
                    components.push(ComparisonComponent::GreaterEquals(Term::parse(tokens)?))
                }
                TokenType::LESS => components.push(ComparisonComponent::Less(Term::parse(tokens)?)),
                TokenType::LESS_EQUAL => {
                    components.push(ComparisonComponent::LessEquals(Term::parse(tokens)?))
                }
                _ => panic!("unexpected token!"),
            }
        }
        Ok(Comparison { term, components })
    }
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

impl Parse for Term {
    fn parse<'a>(
        tokens: &mut Peekable<impl Iterator<Item = &'a Token>>,
    ) -> Result<Self, ParseError> {
        let factor = Factor::parse(tokens)?;
        let mut components: Vec<TermComponent> = Vec::new();
        while let Some(next_token) =
            tokens.next_if(|tok| (tok.type_ == TokenType::PLUS) | (tok.type_ == TokenType::MINUS))
        {
            match next_token.type_ {
                TokenType::PLUS => components.push(TermComponent::Add(Factor::parse(tokens)?)),
                TokenType::MINUS => components.push(TermComponent::Sub(Factor::parse(tokens)?)),
                _ => panic!("unexpected token!"),
            }
        }
        Ok(Term { factor, components })
    }
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

impl Parse for Factor {
    fn parse<'a>(
        tokens: &mut Peekable<impl Iterator<Item = &'a Token>>,
    ) -> Result<Self, ParseError> {
        let unary = Unary::parse(tokens)?;
        let mut components: Vec<FactorComponent> = Vec::new();
        while let Some(next_token) =
            tokens.next_if(|tok| (tok.type_ == TokenType::STAR) | (tok.type_ == TokenType::SLASH))
        {
            match next_token.type_ {
                TokenType::STAR => components.push(FactorComponent::Mul(Unary::parse(tokens)?)),
                TokenType::SLASH => components.push(FactorComponent::Div(Unary::parse(tokens)?)),
                _ => panic!("unexpected token!"),
            }
        }
        Ok(Factor { unary, components })
    }
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

    #[test]
    fn test_factor() {
        let factor_source = String::from("5*8/-2");
        let factor_tokens = crate::lexer::scan_source(&factor_source).unwrap();
        let mut token_iter = factor_tokens.iter().peekable();
        assert_eq!(
            Factor::parse(&mut token_iter),
            Ok(Factor {
                unary: Unary::Primary(Primary::Number(5.0)),
                components: vec![
                    FactorComponent::Mul(Unary::Primary(Primary::Number(8.0))),
                    FactorComponent::Div(Unary::Minus(Box::new(Unary::Primary(Primary::Number(
                        2.0
                    )))))
                ]
            })
        )
    }

    #[test]
    fn test_term() {
        let term_source = String::from("5*8/-2+3-7");
        let term_tokens = crate::lexer::scan_source(&term_source).unwrap();
        let mut token_iter = term_tokens.iter().peekable();
        assert_eq!(
            Term::parse(&mut token_iter),
            Ok(Term {
                factor: Factor {
                    unary: Unary::Primary(Primary::Number(5.0)),
                    components: vec![
                        FactorComponent::Mul(Unary::Primary(Primary::Number(8.0))),
                        FactorComponent::Div(Unary::Minus(Box::new(Unary::Primary(
                            Primary::Number(2.0)
                        ))))
                    ]
                },
                components: vec![
                    TermComponent::Add(Factor {
                        unary: Unary::Primary(Primary::Number(3.0)),
                        components: vec![]
                    }),
                    TermComponent::Sub(Factor {
                        unary: Unary::Primary(Primary::Number(7.0)),
                        components: vec![]
                    })
                ]
            })
        )
    }

    #[test]
    fn test_comparison() {
        let comp_source = String::from("3>2");
        let comp_tokens = crate::lexer::scan_source(&comp_source).unwrap();
        let mut token_iter = comp_tokens.iter().peekable();
        assert_eq!(
            Comparison::parse(&mut token_iter),
            Ok(Comparison {
                term: Term {
                    factor: Factor {
                        unary: Unary::Primary(Primary::Number(3.0)),
                        components: vec![]
                    },
                    components: vec![],
                },
                components: vec![ComparisonComponent::Greater(Term {
                    factor: Factor {
                        unary: Unary::Primary(Primary::Number(2.0)),
                        components: vec![],
                    },
                    components: vec![],
                })],
            })
        )
    }

    #[test]
    fn test_equality() {
        let comp_source = String::from("true!=false");
        let comp_tokens = crate::lexer::scan_source(&comp_source).unwrap();
        let mut token_iter = comp_tokens.iter().peekable();
        assert_eq!(
            Equality::parse(&mut token_iter),
            Ok(Equality {
                comparison: Comparison {
                    term: Term {
                        factor: Factor {
                            unary: Unary::Primary(Primary::True),
                            components: vec![],
                        },
                        components: vec![],
                    },
                    components: vec![],
                },
                components: vec![EqualityComponent::NotEquals(Comparison {
                    term: Term {
                        factor: Factor {
                            unary: Unary::Primary(Primary::False),
                            components: vec![],
                        },
                        components: vec![],
                    },
                    components: vec![],
                })]
            })
        );
    }
}

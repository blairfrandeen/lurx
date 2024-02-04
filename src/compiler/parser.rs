use crate::lexer::{Token, TokenType};

use std::iter::{Iterator, Peekable};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnclosedParenthesis(Token),
    UnexpectedToken(Token),
}

pub trait Parse {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParseError>
    where
        Self: Sized;

    /// for testing only.
    fn from_str(source: &str) -> Self
    where
        Self: Sized,
    {
        let mut token_iter = crate::lexer::token_iter(&source);
        Self::parse(&mut token_iter).unwrap()
    }
}

pub fn parse_tokens(tokens: Vec<Token>) -> Result<Vec<Expression>, ParseError> {
    let mut token_iter = tokens.into_iter().peekable();
    let mut exprs: Vec<Expression> = Vec::new();
    while let Some(next_tok) = token_iter.peek() {
        match next_tok.type_ {
            TokenType::EOF => break,
            _ => {
                let ex = Expression::parse(&mut token_iter)?;
                exprs.push(ex);
            }
        }
    }
    dbg!(token_iter.peek());
    Ok(exprs)
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Equality(Equality),
    // TODO in future chapters
}

impl Parse for Expression {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParseError> {
        let equality = Equality::parse(tokens)?;
        Ok(Self::Equality(equality))
    }
}

#[derive(Debug, PartialEq)]
pub struct Equality {
    pub comparison: Comparison,
    pub components: Vec<EqualityComponent>,
}

#[derive(Debug, PartialEq)]
pub struct EqualityComponent {
    pub operator: Token,
    pub comparison: Comparison,
}

impl Parse for Equality {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParseError> {
        let comparison = Comparison::parse(tokens)?;
        let mut components: Vec<EqualityComponent> = Vec::new();

        while let Some(operator) = tokens.next_if(|tok| {
            (tok.type_ == TokenType::EQUAL_EQUAL) | (tok.type_ == TokenType::BANG_EQUAL)
        }) {
            components.push(EqualityComponent {
                operator,
                comparison: Comparison::parse(tokens)?,
            });
        }
        Ok(Equality {
            comparison,
            components,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Comparison {
    pub term: Term,
    pub components: Vec<ComparisonComponent>,
}

#[derive(Debug, PartialEq)]
pub struct ComparisonComponent {
    pub operator: Token,
    pub term: Term,
}

impl Parse for Comparison {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParseError> {
        let term = Term::parse(tokens)?;
        let mut components: Vec<ComparisonComponent> = Vec::new();
        while let Some(operator) = tokens.next_if(|tok| {
            (tok.type_ == TokenType::GREATER)
                | (tok.type_ == TokenType::GREATER_EQUAL)
                | (tok.type_ == TokenType::LESS)
                | (tok.type_ == TokenType::LESS_EQUAL)
        }) {
            components.push(ComparisonComponent {
                operator,
                term: Term::parse(tokens)?,
            });
        }
        Ok(Comparison { term, components })
    }
}

#[derive(Debug, PartialEq)]
pub struct Term {
    pub factor: Factor,
    pub components: Vec<TermComponent>,
}

#[derive(Debug, PartialEq)]
pub struct TermComponent {
    pub operator: Token,
    pub factor: Factor,
}

impl Parse for Term {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParseError> {
        let factor = Factor::parse(tokens)?;
        let mut components: Vec<TermComponent> = Vec::new();
        while let Some(operator) =
            tokens.next_if(|tok| (tok.type_ == TokenType::PLUS) | (tok.type_ == TokenType::MINUS))
        {
            components.push(TermComponent {
                operator,
                factor: Factor::parse(tokens)?,
            });
        }
        Ok(Term { factor, components })
    }
}

#[derive(Debug, PartialEq)]
pub struct Factor {
    pub unary: Unary,
    pub components: Vec<FactorComponent>,
}

#[derive(Debug, PartialEq)]
pub struct FactorComponent {
    pub operator: Token,
    pub unary: Unary,
}

impl Parse for Factor {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParseError> {
        let unary = Unary::parse(tokens)?;
        let mut components: Vec<FactorComponent> = Vec::new();
        while let Some(operator) =
            tokens.next_if(|tok| (tok.type_ == TokenType::STAR) | (tok.type_ == TokenType::SLASH))
        {
            components.push(FactorComponent {
                operator,
                unary: Unary::parse(tokens)?,
            });
        }
        Ok(Factor { unary, components })
    }
}

#[derive(Debug, PartialEq)]
pub enum Unary {
    Unary { operator: Token, unary: Box<Unary> },
    Primary(Primary),
}

impl Parse for Unary {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParseError> {
        if let Some(next_token) = tokens.peek() {
            let unary = match next_token.type_ {
                TokenType::MINUS => {
                    let operator = tokens.next().unwrap();
                    Unary::Unary {
                        operator,
                        unary: Box::new(Unary::parse(tokens)?),
                    }
                }
                TokenType::BANG => {
                    let operator = tokens.next().unwrap();
                    Unary::Unary {
                        operator,
                        unary: Box::new(Unary::parse(tokens)?),
                    }
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
pub struct Primary {
    pub token: Token,
    pub group: Option<Box<Expression>>,
}

impl Parse for Primary {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParseError> {
        if let Some(next_token) = tokens.next_if(|tok| {
            (tok.type_ == TokenType::NUMLIT)
                | (tok.type_ == TokenType::STRINGLIT)
                | (tok.type_ == TokenType::FALSE)
                | (tok.type_ == TokenType::TRUE)
                | (tok.type_ == TokenType::NIL)
                | (tok.type_ == TokenType::LEFT_PAREN)
                | (tok.type_ == TokenType::RIGHT_PAREN)
        }) {
            if next_token.type_ == TokenType::LEFT_PAREN {
                let group = Box::new(Expression::parse(tokens)?);
                if let Some(closing_paren) = tokens.next() {
                    match closing_paren.type_ {
                        // return from the function early here, because we've
                        // already consumed the closing parenthesis and want the
                        // token immediately after that
                        TokenType::RIGHT_PAREN => {
                            return Ok(Primary {
                                token: next_token,
                                group: Some(group),
                            })
                        }
                        _ => return Err(ParseError::UnclosedParenthesis(next_token)),
                    }
                } else {
                    panic!("Unexpected EOF!")
                }
            } else if next_token.type_ == TokenType::RIGHT_PAREN {
                if let Some(closing_paren) = tokens.next() {
                    match closing_paren.type_ {
                        TokenType::RIGHT_PAREN => {
                            return Err(ParseError::UnclosedParenthesis(closing_paren.clone()))
                        }
                        _ => panic!("closing parenthesis unmatched and disappeared!"),
                    }
                } else {
                    panic!("Unexpected EOF!")
                }
            } else {
                Ok(Primary {
                    token: next_token.clone(),
                    group: None,
                })
            }
        } else {
            Err(ParseError::UnexpectedToken(
                tokens.next().expect("unexpected EOF!"),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token_iter;

    #[test]
    fn test_primary() {
        let mut token_iter = token_iter("true false nil 55 \"hello\"");
        let mut expected_types = vec![
            TokenType::TRUE,
            TokenType::FALSE,
            TokenType::NIL,
            TokenType::NUMLIT,
            TokenType::STRINGLIT,
        ]
        .into_iter();
        while let Some(exp_type) = expected_types.next() {
            let primary = Primary::parse(&mut token_iter).unwrap();
            assert_eq!(primary.token.type_, exp_type,);
        }
    }

    #[test]
    fn test_unary_primary() {
        let mut token_iter = token_iter("true false nil 55 \"hello\"");
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Primary(Primary {
                token: Token::from_type(TokenType::TRUE),
                group: None
            }))
        );
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Primary(Primary {
                token: Token::from_type(TokenType::FALSE),
                group: None
            }))
        );
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Primary(Primary {
                token: Token::from_type(TokenType::NIL),
                group: None
            }))
        );
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Primary(Primary {
                token: Token::numlit(55.0),
                group: None
            }))
        );
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Primary(Primary {
                token: Token::stringlit("hello".to_string()),
                group: None
            }))
        );
        assert_eq!(
            token_iter.next().unwrap().type_,
            crate::lexer::TokenType::EOF
        );
    }

    #[test]
    fn test_unary_not() {
        let mut token_iter = token_iter("!!true");
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Unary {
                operator: Token::from_type(TokenType::BANG),
                unary: Box::new(Unary::Unary {
                    operator: Token::from_type(TokenType::BANG),
                    unary: Box::new(Unary::Primary(Primary::from_str("true")))
                })
            })
        );
    }

    #[test]
    fn test_unary_minus() {
        let mut token_iter = token_iter("-5");
        assert_eq!(
            Unary::parse(&mut token_iter),
            Ok(Unary::Unary {
                operator: Token::from_type(TokenType::MINUS),
                unary: Box::new(Unary::Primary(Primary::from_str("5")))
            })
        );
    }

    #[test]
    fn test_factor() {
        let mut token_iter = token_iter("5*8/-2");
        assert_eq!(
            Factor::parse(&mut token_iter),
            Ok(Factor {
                unary: Unary::from_str("5"),
                components: vec![
                    FactorComponent {
                        operator: Token::from_type(TokenType::STAR),
                        unary: Unary::from_str("8")
                    },
                    FactorComponent {
                        operator: Token::from_type(TokenType::SLASH),
                        unary: Unary::from_str("-2")
                    }
                ]
            })
        )
    }

    #[test]
    fn test_term() {
        let mut token_iter = token_iter("5*8/-2+3-7");
        assert_eq!(
            Term::parse(&mut token_iter),
            Ok(Term {
                factor: Factor::from_str("5*8/-2"),
                components: vec![
                    TermComponent {
                        operator: Token::from_type(TokenType::PLUS),
                        factor: Factor::from_str("3"),
                    },
                    TermComponent {
                        operator: Token::from_type(TokenType::MINUS),
                        factor: Factor::from_str("7"),
                    },
                ]
            })
        )
    }

    #[test]
    fn test_comparison() {
        let mut token_iter = token_iter("3>2");
        assert_eq!(
            Comparison::parse(&mut token_iter),
            Ok(Comparison {
                term: Term::from_str("3"),
                components: vec![ComparisonComponent {
                    operator: Token::from_type(TokenType::GREATER),
                    term: Term::from_str("2"),
                }],
            })
        )
    }

    #[test]
    fn test_equality() {
        let mut token_iter = token_iter("true!=false");
        assert_eq!(
            Equality::parse(&mut token_iter),
            Ok(Equality {
                comparison: Comparison::from_str("true"),
                components: vec![EqualityComponent {
                    operator: Token::from_type(TokenType::BANG_EQUAL),
                    comparison: Comparison::from_str("false")
                }]
            })
        );
    }

    #[test]
    fn test_expression() {
        let mut token_iter = token_iter("true!=false");
        assert_eq!(
            Expression::parse(&mut token_iter),
            Ok(Expression::Equality(Equality::from_str("true!=false")))
        );
    }

    #[test]
    fn test_unmatched_lparen() {
        let expr_source = String::from("((1+((2))/3)");
        let expr_tokens = crate::lexer::scan_source(&expr_source).unwrap();
        assert_eq!(
            parse_tokens(expr_tokens),
            Err(ParseError::UnclosedParenthesis(Token {
                type_: TokenType::LEFT_PAREN,
                lexeme: "(".to_string(),
                ..Default::default()
            }))
        )
    }
    #[test]
    fn test_unmatched_rparen() {
        let expr_source = String::from("((1+2))/3)))");
        let expr_tokens = crate::lexer::scan_source(&expr_source).unwrap();
        assert_eq!(
            parse_tokens(expr_tokens),
            Err(ParseError::UnclosedParenthesis(Token {
                type_: TokenType::RIGHT_PAREN,
                lexeme: ")".to_string(),
                ..Default::default()
            }))
        )
    }
}

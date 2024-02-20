use crate::lexer::{Token, TokenType};

use std::iter::{Iterator, Peekable};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnclosedParenthesis(Token),
    UnexpectedToken(Token),
    NotImplemented(Token),
    ExpectedToken { expected: TokenType, found: Token },
    MissingEof,
}

#[allow(unused)]
impl Expr {
    /// Function to create expressions from string slices. Intended for testing
    /// only, no error handling is included.
    fn from_str(source: &str) -> Self
    where
        Self: Sized,
    {
        let mut token_iter = crate::lexer::token_iter(&source);
        expression(&mut token_iter).unwrap()
    }
}

#[derive(PartialEq, Debug)]
pub struct Program {
    pub declarations: Vec<Decl>,
}

#[derive(PartialEq, Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Decl {
    VarDecl { name: Token, initializer: Expr },
    Statement(Stmt),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Token),
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
}

pub fn program(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut token_iter = tokens.into_iter().peekable();
    let mut declarations: Vec<Decl> = Vec::new();
    while let Some(next_tok) = token_iter.peek() {
        match next_tok.type_ {
            // TODO: Sanity check for other tokens beyond EOF?
            TokenType::EOF => return Ok(Program { declarations }),
            _ => declarations.push(declaration(&mut token_iter)?),
        }
    }
    Err(ParseError::MissingEof)
}

pub fn declaration(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Decl, ParseError> {
    let next_token = tokens.peek().expect("Unexpected EOF!");
    let decl = match next_token.type_ {
        TokenType::VAR => {
            tokens.next(); // consume var token
            let name = match_token(tokens, TokenType::IDENTIFIER)?;
            match_token(tokens, TokenType::EQUAL)?;
            let initializer = expression(tokens)?;
            Decl::VarDecl { name, initializer }
        }
        _ => Decl::Statement(statement(tokens)?),
    };
    match_token(tokens, TokenType::SEMICOLON)?;
    Ok(decl)
}

/// Ensure the next token in a token iterator is of the expected type
/// Return that token if successful
fn match_token(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    expected: TokenType,
) -> Result<Token, ParseError> {
    let next_token = tokens.next().expect("Unexpected EOF!");
    if next_token.type_ == expected {
        Ok(next_token)
    } else {
        Err(ParseError::ExpectedToken {
            expected,
            found: next_token,
        })
    }
}

pub fn statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt, ParseError> {
    let next_token = tokens.peek().expect("Unexpected EOF!");
    let stmt = match next_token.type_ {
        TokenType::PRINT => {
            tokens.next(); // consume print token
            Stmt::Print(expression(tokens)?)
        }
        _ => Stmt::Expression(expression(tokens)?),
    };
    Ok(stmt)
}

pub fn expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    equality(tokens)
}

fn equality(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = comparison(tokens)?;

    while let Some(operator) = tokens
        .next_if(|tok| (tok.type_ == TokenType::EQUAL_EQUAL) | (tok.type_ == TokenType::BANG_EQUAL))
    {
        let right = comparison(tokens)?;
        expr = Expr::Binary {
            left: Box::new(expr),
            operator,
            right: Box::new(right),
        };
    }
    Ok(expr)
}

fn comparison(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = term(tokens)?;
    while let Some(operator) = tokens.next_if(|tok| {
        (tok.type_ == TokenType::GREATER)
            | (tok.type_ == TokenType::GREATER_EQUAL)
            | (tok.type_ == TokenType::LESS)
            | (tok.type_ == TokenType::LESS_EQUAL)
    }) {
        let right = term(tokens)?;
        expr = Expr::Binary {
            left: Box::new(expr),
            operator,
            right: Box::new(right),
        };
    }
    Ok(expr)
}

fn term(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = factor(tokens)?;
    while let Some(operator) =
        tokens.next_if(|tok| (tok.type_ == TokenType::PLUS) | (tok.type_ == TokenType::MINUS))
    {
        let right = factor(tokens)?;
        expr = Expr::Binary {
            left: Box::new(expr),
            operator,
            right: Box::new(right),
        };
    }
    Ok(expr)
}

fn factor(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let mut expr = unary(tokens)?;
    while let Some(operator) =
        tokens.next_if(|tok| (tok.type_ == TokenType::STAR) | (tok.type_ == TokenType::SLASH))
    {
        let right = unary(tokens)?;
        expr = Expr::Binary {
            left: Box::new(expr),
            operator,
            right: Box::new(right),
        };
    }
    Ok(expr)
}

fn unary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    if let Some(next_token) = tokens.peek() {
        if (next_token.type_ == TokenType::MINUS) | (next_token.type_ == TokenType::BANG) {
            let operator = tokens.next().expect("missing token!");
            let right = Box::new(unary(tokens)?);
            Ok(Expr::Unary { operator, right })
        } else {
            primary(tokens)
        }
    } else {
        panic!("unexpected EOF!");
    }
}

fn primary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    if let Some(next_token) = tokens.next_if(|tok| {
        (tok.type_ == TokenType::NUMLIT)
            | (tok.type_ == TokenType::STRINGLIT)
            | (tok.type_ == TokenType::FALSE)
            | (tok.type_ == TokenType::TRUE)
            | (tok.type_ == TokenType::NIL)
    }) {
        Ok(Expr::Literal(next_token))
    } else if let Some(next_token) = tokens.next_if(|tok| tok.type_ == TokenType::LEFT_PAREN) {
        let expr = expression(tokens)?;
        let closing_paren = tokens.next().expect("Unexpected EOF!");
        match closing_paren.type_ {
            // return from the function early here, because we've
            // already consumed the closing parenthesis and want the
            // token immediately after that
            TokenType::RIGHT_PAREN => return Ok(Expr::Grouping(Box::new(expr))),
            _ => return Err(ParseError::UnclosedParenthesis(next_token)),
        }
    } else if let Some(next_token) = tokens.next_if(|tok| tok.type_ == TokenType::RIGHT_PAREN) {
        Err(ParseError::UnclosedParenthesis(next_token.clone()))
    } else {
        let next_token = tokens.next().expect("Unexpected EOF!");
        Err(ParseError::NotImplemented(next_token.clone()))
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
            let primary = primary(&mut token_iter).unwrap();
            let token = match primary {
                Expr::Literal(tok) => tok,
                _ => panic!(),
            };
            assert_eq!(token.type_, exp_type,);
        }
    }

    #[test]
    fn test_unary_primary() {
        let mut token_iter = token_iter("true false nil 55 \"hello\"");
        assert_eq!(
            unary(&mut token_iter),
            Ok(Expr::Literal(Token::from_type(TokenType::TRUE),))
        );
        assert_eq!(
            unary(&mut token_iter),
            Ok(Expr::Literal(Token::from_type(TokenType::FALSE),))
        );
        assert_eq!(
            unary(&mut token_iter),
            Ok(Expr::Literal(Token::from_type(TokenType::NIL),))
        );
        assert_eq!(
            unary(&mut token_iter),
            Ok(Expr::Literal(Token::numlit(55.0)))
        );
        assert_eq!(
            unary(&mut token_iter),
            Ok(Expr::Literal(Token::stringlit("hello".to_string())))
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
            unary(&mut token_iter),
            Ok(Expr::Unary {
                operator: Token::from_type(TokenType::BANG),
                right: Box::new(Expr::Unary {
                    operator: Token::from_type(TokenType::BANG),
                    right: Box::new(Expr::Literal(Token::from("true")))
                })
            })
        );
    }

    #[test]
    fn test_unary_minus() {
        let mut token_iter = token_iter("-5");
        assert_eq!(
            unary(&mut token_iter),
            Ok(Expr::Unary {
                operator: Token::from_type(TokenType::MINUS),
                right: Box::new(Expr::Literal(Token::from("5")))
            })
        );
    }

    #[test]
    fn test_factor() {
        let mut token_iter = token_iter("5*8/-2");
        assert_eq!(
            factor(&mut token_iter),
            Ok(Expr::Binary {
                left: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(Token::numlit(5.0))),
                    operator: Token::from_type(TokenType::STAR),
                    right: Box::new(Expr::Literal(Token::numlit(8.0))),
                }),
                operator: Token::from_type(TokenType::SLASH),
                right: Box::new(Expr::Unary {
                    operator: Token::from_type(TokenType::MINUS),
                    right: Box::new(Expr::Literal(Token::numlit(2.0)))
                }),
            })
        )
    }

    #[test]
    fn test_term() {
        let mut token_iter = token_iter("5*8/-2+3-7");
        assert_eq!(
            term(&mut token_iter),
            Ok(Expr::Binary {
                left: Box::new(Expr::Binary {
                    left: Box::new(Expr::from_str("5*8/-2")),
                    operator: Token::from_type(TokenType::PLUS),
                    right: Box::new(Expr::from_str("3")),
                }),
                operator: Token::from_type(TokenType::MINUS),
                right: Box::new(Expr::from_str("7")),
            })
        )
    }

    #[test]
    fn test_comparison() {
        let mut token_iter = token_iter("3>2");
        assert_eq!(
            comparison(&mut token_iter),
            Ok(Expr::Binary {
                left: Box::new(Expr::from_str("3")),
                operator: Token::from_type(TokenType::GREATER),
                right: Box::new(Expr::from_str("2")),
            })
        )
    }

    #[test]
    fn test_equality() {
        let mut token_iter = token_iter("true!=false");
        assert_eq!(
            equality(&mut token_iter),
            Ok(Expr::Binary {
                left: Box::new(Expr::from_str("true")),
                operator: Token::from_type(TokenType::BANG_EQUAL),
                right: Box::new(Expr::from_str("false"))
            })
        );
    }

    #[test]
    fn test_expression() {
        let mut token_iter = token_iter("true!=false");
        assert_eq!(
            expression(&mut token_iter),
            Ok(Expr::Binary {
                left: Box::new(Expr::from_str("true")),
                operator: Token::from_type(TokenType::BANG_EQUAL),
                right: Box::new(Expr::from_str("false"))
            })
        );
    }

    #[test]
    fn test_unmatched_lparen() {
        let expr_source = String::from("((1+((2))/3)");
        let expr_tokens = crate::lexer::scan_source(&expr_source).unwrap();
        assert_eq!(
            program(expr_tokens),
            Err(ParseError::UnclosedParenthesis(Token {
                type_: TokenType::LEFT_PAREN,
                ..Default::default()
            }))
        )
    }
    #[test]
    #[ignore] // Failing, see https://github.com/blairfrandeen/lurx/issues/3
    fn test_unmatched_rparen() {
        let expr_source = String::from("((1+2))/3)))");
        let expr_tokens = crate::lexer::scan_source(&expr_source).unwrap();
        assert_eq!(
            program(expr_tokens),
            Err(ParseError::UnclosedParenthesis(Token {
                type_: TokenType::RIGHT_PAREN,
                ..Default::default()
            }))
        )
    }

    #[test]
    #[ignore] // Failing, see https://github.com/blairfrandeen/lurx/issues/3
    fn test_unmatched_closing_paren() {
        let expr_source = String::from("();");
        let expr_tokens = crate::lexer::scan_source(&expr_source).unwrap();
        assert!(program(expr_tokens).is_ok());
    }

    #[test]
    fn test_var_decl() {
        let mut token_iter = token_iter("var a = 7;");
        assert_eq!(
            declaration(&mut token_iter),
            Ok(Decl::VarDecl {
                name: Token::identifier("a".to_string()),
                initializer: Expr::Literal(Token::numlit(7.0))
            })
        );
    }

    #[test]
    fn test_var_decl_missing_eq() {
        let mut token_iter = token_iter("var a 7;");
        assert_eq!(
            declaration(&mut token_iter),
            Err(ParseError::ExpectedToken {
                expected: TokenType::EQUAL,
                found: Token::numlit(7.0),
            })
        )
    }

    #[test]
    fn test_var_decl_missing_ident() {
        let mut token_iter = token_iter("var = 7;");
        assert_eq!(
            declaration(&mut token_iter),
            Err(ParseError::ExpectedToken {
                expected: TokenType::IDENTIFIER,
                found: Token::from_type(TokenType::EQUAL),
            })
        )
    }
}

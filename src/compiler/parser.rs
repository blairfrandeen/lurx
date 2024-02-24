use crate::lexer::{Token, TokenType};

use std::iter::{Iterator, Peekable};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnclosedParenthesis(Token),
    UnexpectedToken(Token),
    NotImplemented(Token),
    ExpectedToken { expected: TokenType, found: Token },
    InvalidAssignmentTarget(Expr),
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
    pub statements: Vec<Stmt>,
    pub errors: Vec<ParseError>,
    pub source: String,
}

#[derive(PartialEq, Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    VarDecl {
        name: Token,
        initializer: Expr,
    },
    Block(Vec<Stmt>),
    Conditional {
        condition: Expr,
        true_branch: Box<Stmt>,
        false_branch: Option<Box<Stmt>>,
    },
    Loop {
        condition: Expr,
        statements: Box<Stmt>,
    },
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
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Variable(Token),
    Assign {
        // NOTE: Official implementation uses a String for `name` below. I've chosen to use a token for
        // better error handling; unsure if this will bite me as we implement fields.
        name: Token,
        value: Box<Expr>,
    },
}

pub fn program(tokens: Vec<Token>, source: String) -> Program {
    let mut token_iter = tokens.into_iter().peekable();
    let mut statements: Vec<Stmt> = Vec::new();
    let mut errors: Vec<ParseError> = Vec::new();
    while let Some(next_tok) = token_iter.peek() {
        match next_tok.type_ {
            // TODO: Sanity check for other tokens beyond EOF?
            TokenType::EOF => break,
            _ => match declaration(&mut token_iter) {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    errors.push(err);
                    synchronize(&mut token_iter);
                }
            },
        }
    }
    Program {
        statements,
        errors,
        source,
    }
}

fn declaration(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt, ParseError> {
    let next_token = tokens.peek().expect("Unexpected EOF!");
    let decl = match next_token.type_ {
        TokenType::VAR => {
            tokens.next(); // consume var token
            let name = consume_token(tokens, TokenType::IDENTIFIER)?;
            // TODO: Allow variables to be declared without initializers
            // See issue #7
            consume_token(tokens, TokenType::EQUAL)?;
            let initializer = expression(tokens)?;
            Stmt::VarDecl { name, initializer }
        }
        _ => statement(tokens)?,
    };

    match decl {
        Stmt::Block(_) => Ok(decl),
        Stmt::Conditional {
            condition: _,
            true_branch: _,
            false_branch: _,
        } => Ok(decl),
        Stmt::Loop {
            condition: _,
            statements: _,
        } => Ok(decl),
        _ => {
            let lookahead = tokens.peek().expect("Unexpected EOF!");
            match lookahead.type_ {
                TokenType::SEMICOLON => {
                    tokens.next();
                    Ok(decl)
                }
                _ => Err(ParseError::ExpectedToken {
                    expected: TokenType::SEMICOLON,
                    found: lookahead.clone(),
                }),
            }
        }
    }
}

fn statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Stmt, ParseError> {
    let next_token = tokens.peek().expect("Unexpected EOF!");
    let stmt = match next_token.type_ {
        TokenType::PRINT => {
            tokens.next(); // consume print token
            Stmt::Print(expression(tokens)?)
        }
        TokenType::LEFT_BRACE => {
            tokens.next();
            let mut stmts = Vec::new();
            while let Some(next_token) = tokens.peek() {
                let stmt = match next_token.type_ {
                    TokenType::RIGHT_BRACE => break,
                    _ => declaration(tokens)?,
                };
                stmts.push(stmt);
            }

            consume_token(tokens, TokenType::RIGHT_BRACE)?;
            Stmt::Block(stmts)
        }
        TokenType::IF => {
            tokens.next();
            let condition = expression(tokens)?;
            let true_branch = Box::new(declaration(tokens)?);
            let next_token = tokens.peek().expect("Unexpected EOF!");
            let false_branch = match next_token.type_ {
                TokenType::ELSE => {
                    tokens.next();
                    Some(Box::new(declaration(tokens)?))
                }
                _ => None,
            };
            Stmt::Conditional {
                condition,
                true_branch,
                false_branch,
            }
        }
        TokenType::WHILE => {
            tokens.next();
            let condition = expression(tokens)?;
            let statements = Box::new(declaration(tokens)?);
            Stmt::Loop {
                condition,
                statements,
            }
        }
        _ => Stmt::Expression(expression(tokens)?),
    };
    Ok(stmt)
}

pub fn expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    assignment(tokens)
}

fn assignment(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let expr = logic_or(tokens)?;

    match tokens.next_if(|tok| tok.type_ == TokenType::EQUAL) {
        Some(_) => {
            let value = assignment(tokens)?;
            match expr {
                Expr::Variable(name) => Ok(Expr::Assign {
                    name,
                    value: Box::new(value),
                }),
                _ => Err(ParseError::InvalidAssignmentTarget(expr)),
            }
        }
        None => Ok(expr),
    }
}

fn logic_or(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let left = logic_and(tokens)?;
    match tokens.next_if(|tok| tok.type_ == TokenType::OR) {
        Some(operator) => {
            let right = logic_and(tokens)?;
            Ok(Expr::Logical {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            })
        }
        None => Ok(left),
    }
}

fn logic_and(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expr, ParseError> {
    let left = equality(tokens)?;
    match tokens.next_if(|tok| tok.type_ == TokenType::AND) {
        Some(operator) => {
            let right = equality(tokens)?;
            Ok(Expr::Logical {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            })
        }
        None => Ok(left),
    }
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
    } else if let Some(next_token) = tokens.next_if(|tok| tok.type_ == TokenType::IDENTIFIER) {
        Ok(Expr::Variable(next_token))
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

//////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
//////////////////////////////////////////////////////////////////

/// Ensure the next token in a token iterator is of the expected type
/// Advance the iterator, and return that token if successful
fn consume_token(
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

/// This function should be called in case of a ParseError. This will advance the tokens until the
/// probable beginning of a new statement is found.
fn synchronize(tokens: &mut Peekable<impl Iterator<Item = Token>>) {
    while let Some(current_token) = tokens.peek() {
        match current_token.type_ {
            // if we get to a semicolon, we suspect we're at a statement boundary, so we consume it
            // and move on
            TokenType::SEMICOLON => {
                tokens.next();
                return;
            }
            // If we get to a token that typically starts a statement, we presume that a semicolon
            // was missing and we return without consuming the token so we can start a new
            // statement.
            TokenType::CLASS => return,
            TokenType::FUN => return,
            TokenType::VAR => return,
            TokenType::FOR => return,
            TokenType::IF => return,
            TokenType::WHILE => return,
            TokenType::PRINT => return,
            TokenType::RETURN => return,
            _ => {
                // anything else, consume the token and keep trying to find a statement boundary.
                tokens.next();
                continue;
            }
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
            program(expr_tokens, expr_source).errors[0],
            ParseError::UnclosedParenthesis(Token {
                type_: TokenType::LEFT_PAREN,
                ..Default::default()
            })
        )
    }
    #[test]
    #[ignore] // Failing, see https://github.com/blairfrandeen/lurx/issues/3
    fn test_unmatched_rparen() {
        let expr_source = String::from("((1+2))/3)))");
        let expr_tokens = crate::lexer::scan_source(&expr_source).unwrap();
        assert_eq!(
            program(expr_tokens, expr_source).errors[0],
            ParseError::UnclosedParenthesis(Token {
                type_: TokenType::RIGHT_PAREN,
                ..Default::default()
            })
        )
    }

    #[test]
    #[ignore] // Failing, see https://github.com/blairfrandeen/lurx/issues/3
    fn test_unmatched_closing_paren() {
        let expr_source = String::from("();");
        let expr_tokens = crate::lexer::scan_source(&expr_source).unwrap();
        assert!(program(expr_tokens, expr_source).errors.is_empty());
    }

    #[test]
    fn test_var_decl() {
        let mut token_iter = token_iter("var a = 7;");
        assert_eq!(
            declaration(&mut token_iter),
            Ok(Stmt::VarDecl {
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

    #[test]
    fn test_multiple_errors() {
        let mult_err =
            std::fs::read_to_string("tests/multiple_error.lox").expect("file should exist");
        let tokens = crate::compiler::lexer::scan_source(&mult_err).unwrap();
        dbg!(&tokens);
        let prgm = program(tokens, mult_err);
        assert_eq!(prgm.errors.len(), 3);
        assert_eq!(prgm.statements.len(), 3);
    }

    #[test]
    fn test_sync() {
        let sync_prgm = "print 1 print 2;".to_string();
        let tokens = crate::compiler::lexer::scan_source(&sync_prgm).unwrap();
        let prgm = program(tokens, sync_prgm);
        assert_eq!(prgm.errors.len(), 1);
        assert_eq!(prgm.statements.len(), 1);
    }

    #[test]
    fn test_logic_or() {
        let mut token_iter = token_iter("true or false;");
        assert_eq!(
            expression(&mut token_iter),
            Ok(Expr::Logical {
                left: Box::new(Expr::Literal(Token::from_type(TokenType::TRUE))),
                operator: Token::from_type(TokenType::OR),
                right: Box::new(Expr::Literal(Token::from_type(TokenType::FALSE))),
            })
        );
    }

    #[test]
    fn test_logic_and() {
        let mut token_iter = token_iter("true and false;");
        assert_eq!(
            expression(&mut token_iter),
            Ok(Expr::Logical {
                left: Box::new(Expr::Literal(Token::from_type(TokenType::TRUE))),
                operator: Token::from_type(TokenType::AND),
                right: Box::new(Expr::Literal(Token::from_type(TokenType::FALSE))),
            })
        );
    }
}

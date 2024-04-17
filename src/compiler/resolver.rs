use std::{cell::RefCell, collections::HashMap};

use crate::compiler::{
    interpreter::Interpreter,
    lexer::Token,
    parser::{Expr, Stmt},
};

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    OwnInitializer(Token),
    Redefinition(Token),
}

pub struct Resolver<'a> {
    pub interpreter: &'a mut Interpreter,
    pub errors: Vec<ResolverError>,
    scopes: Vec<RefCell<HashMap<String, bool>>>,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver {
            interpreter,
            scopes: vec![],
            errors: vec![],
        }
    }

    pub fn resolve(&mut self, statements: &Vec<Stmt>) {
        for stmt in statements.iter() {
            match self.resolve_stmt(stmt) {
                Ok(_) => {}
                Err(err) => self.errors.push(err),
            }
        }
    }

    pub fn resolve_stmt(&mut self, statement: &Stmt) -> Result<(), ResolverError> {
        match statement {
            Stmt::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter() {
                    self.resolve_stmt(stmt)?;
                }
                self.end_scope();
            }
            Stmt::VarDecl { name, initializer } => {
                self.declare(name)?;
                match initializer {
                    Some(expr) => self.resolve_expr(expr)?,
                    None => {}
                };
                self.define(name);
            }
            Stmt::Expression(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::FunDecl {
                name,
                parameters,
                statements,
            } => {
                self.declare(name)?;
                self.define(name);
                // TODO: Separate function form here to next comment when we do classes
                self.begin_scope();
                for param in parameters.iter() {
                    self.declare(param)?;
                    self.define(param);
                }
                self.resolve_stmt(statements)?;
                self.end_scope();
                // kend private function
            }
            Stmt::Conditional {
                condition,
                true_branch,
                false_branch,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(true_branch)?;
                match false_branch {
                    Some(statement) => self.resolve_stmt(statement)?,
                    None => {}
                }
            }
            Stmt::Print(expr) => self.resolve_expr(expr)?,
            Stmt::Return(expr) => self.resolve_expr(expr)?,
            Stmt::WhileLoop {
                condition,
                statements,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(statements)?;
            }
            Stmt::Break => {}
        };
        Ok(())
    }

    pub fn resolve_expr(&mut self, expression: &Expr) -> Result<(), ResolverError> {
        match expression {
            Expr::Variable(var) => {
                if self
                    .scopes
                    .last()
                    .is_some_and(|s| s.borrow().get(var.ident()).is_some_and(|t| !*t))
                {
                    return Err(ResolverError::OwnInitializer(var.clone()));
                }
                self.resolve_local(expression, &var.ident());
            }
            Expr::Assign { name, value } => {
                self.resolve_expr(value)?;
                self.resolve_local(expression, name.ident());
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                self.resolve_expr(callee)?;
                for arg in arguments.iter() {
                    self.resolve_expr(arg)?;
                }
            }
            Expr::Grouping(group) => self.resolve_expr(group)?,
            Expr::Literal(_) => {}
            Expr::Logical { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Unary { right, .. } => self.resolve_expr(right)?,
        };
        Ok(())
    }

    fn resolve_local(&mut self, expression: &Expr, name: &String) {
        let mut i = self.scopes.len() as i32;
        while i >= 0 {
            if self
                .scopes
                .get(i as usize)
                .is_some_and(|s| s.borrow().get(name).is_some())
            {
                self.interpreter
                    .resolve(expression, self.scopes.len() - 1 - i as usize);
                return;
            }
            i = i - 1;
        }
    }

    fn begin_scope(&mut self) {
        let _ = &self.scopes.push(RefCell::new(HashMap::new()));
    }

    fn end_scope(&mut self) {
        let _ = &self.scopes.pop();
    }

    fn declare(&self, name: &Token) -> Result<(), ResolverError> {
        match self.scopes.last() {
            Some(scope) => {
                if scope.borrow().get(name.ident()).is_some() {
                    return Err(ResolverError::Redefinition(name.clone()));
                }
                scope.borrow_mut().insert(name.ident().to_string(), false);
                Ok(())
            }
            None => Ok(()),
        }
    }

    fn define(&self, name: &Token) {
        match self.scopes.last() {
            Some(scope) => scope.borrow_mut().insert(name.ident().to_string(), true),
            None => return,
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::{interpreter, lexer, parser};

    fn prgm_fixt(source: &str) -> parser::Program {
        let tokens = lexer::scan_source(&source.to_string()).unwrap();
        let program = parser::program(tokens, source.to_string());
        program
    }

    #[test]
    fn own_decl_error() {
        let program = prgm_fixt("var a = \"init\"; {var a = a;}");
        let mut interp = interpreter::Interpreter::new();
        let mut res = Resolver::new(&mut interp);
        res.resolve(&program.statements);
        assert_eq!(
            res.errors.first().expect("one error"),
            &ResolverError::OwnInitializer(Token::identifier("a".to_string()))
        )
    }

    #[test]
    fn redefinition_error() {
        let program = prgm_fixt("fun bad() { var a = \"first\"; var a = \"second\"; }");
        let mut interp = interpreter::Interpreter::new();
        let mut res = Resolver::new(&mut interp);
        res.resolve(&program.statements);
        assert!(!res.errors.is_empty());
        assert_eq!(
            res.errors.first().expect("one error"),
            &ResolverError::Redefinition(Token::identifier("a".to_string()))
        )
    }

    #[test]
    fn declare_define() {
        let mut interp = Interpreter::new();
        let mut res = Resolver::new(&mut interp);
        let nametok = Token::identifier("a".to_string());

        // with no scope, declare & define should do nothing
        let _ = res.declare(&nametok);
        res.define(&nametok);
        assert!(res.scopes.is_empty());

        res.begin_scope();
        assert_eq!(res.scopes.len(), 1);
        let _ = res.declare(&nametok);
        assert!(!res
            .scopes
            .first()
            .expect("one scope")
            .borrow()
            .get(nametok.ident())
            .expect("Name should exist"));
        res.define(&nametok);
        assert!(res
            .scopes
            .first()
            .expect("one scope")
            .borrow()
            .get(nametok.ident())
            .expect("Name should exist"));

        // scope should end
        res.end_scope();
        let _ = res.declare(&nametok);
        res.define(&nametok);
        assert!(res.scopes.is_empty());
    }
}

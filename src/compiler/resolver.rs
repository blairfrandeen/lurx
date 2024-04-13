use std::{cell::RefCell, collections::HashMap};

use crate::compiler::{
    interpreter::Interpreter,
    lexer::Token,
    parser::{Expr, Stmt},
};

struct Resolver {
    interpreter: Interpreter,
    scopes: Vec<RefCell<HashMap<String, bool>>>,
}

impl Resolver {
    pub fn resolve(&self, statements: Vec<Stmt>) {
        todo!();
    }

    pub fn resolve_stmt(&mut self, statement: &Stmt) {
        match statement {
            Stmt::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter() {
                    self.resolve_stmt(stmt);
                }
                self.end_scope();
            }
            Stmt::VarDecl { name, initializer } => {
                self.declare(name);
                match initializer {
                    Some(expr) => self.resolve_expr(expr),
                    None => {}
                };
                self.define(name);
            }
            Stmt::Expression(expr) => {
                self.resolve_expr(expr);
            }
            Stmt::FunDecl {
                name,
                parameters,
                statements,
            } => {
                self.declare(name);
                self.define(name);
                self.begin_scope();
                for param in parameters.iter() {
                    self.declare(param);
                    self.define(param);
                }
                self.resolve_stmt(statements);
                self.end_scope();
            }
            _ => todo!(),
        }
    }

    pub fn resolve_expr(&mut self, expression: &Expr) {
        match expression {
            Expr::Variable(var) => {
                match &self.scopes.last() {
                    Some(scope) => match scope.borrow().get(var.ident()) {
                        Some(_) => {},
                        None => todo!("Need proper error hanLIng for reading local variable within its own initializer"),
                    }
                    None => {},
                }
                self.resolve_local(expression, &var.ident());
            }
            Expr::Assign { name, value } => {
                self.resolve_expr(value);
                self.resolve_local(expression, name.ident());
            }
            _ => todo!(),
        }
    }

    fn resolve_local(&mut self, expression: &Expr, name: &String) {
        todo!()
    }

    fn begin_scope(&mut self) {
        let _ = &self.scopes.push(RefCell::new(HashMap::new()));
    }

    fn end_scope(&mut self) {
        let _ = &self.scopes.pop();
    }

    fn declare(&self, name: &Token) {
        match self.scopes.last() {
            Some(scope) => scope.borrow_mut().insert(name.ident().to_string(), false),
            None => return,
        };
    }

    fn define(&self, name: &Token) {
        match self.scopes.last() {
            Some(scope) => scope.borrow_mut().insert(name.ident().to_string(), true),
            None => panic!("Attempt to define undeclared name: {}", name.ident()),
        };
    }
}

use std::collections::HashMap;

use crate::compiler::{
    interpreter::Interpreter,
    parser::{Expr, Stmt},
};

struct Resolver {
    interpreter: Interpreter,
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    pub fn resolve(&self, statements: Vec<Stmt>) {
        todo!();
    }

    fn begin_scope(&mut self) {
        &self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        let _ = &self.scopes.pop();
    }
}

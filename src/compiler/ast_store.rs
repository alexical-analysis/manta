use std::collections::HashMap;

use crate::ast::ast::{Decl, DeclValue, Expr, ExprValue, Stmt, StmtValue};
use crate::ast::lexer::{Pos, Token};
use crate::index_vec::IndexVec;

pub struct AstStore {
    decls: IndexVec<Decl, DeclValue>,
    decl_start: HashMap<Decl, Pos>,

    exprs: IndexVec<Expr, ExprValue>,
    expr_start: HashMap<Expr, Pos>,

    stmts: IndexVec<Stmt, StmtValue>,
    stmt_start: HashMap<Stmt, Pos>,
}

impl AstStore {
    pub fn new() -> Self {
        Self {
            decls: IndexVec::new(),
            decl_start: HashMap::new(),
            exprs: IndexVec::new(),
            expr_start: HashMap::new(),
            stmts: IndexVec::new(),
            stmt_start: HashMap::new(),
        }
    }

    pub fn get_decl(&mut self, start: Token, decl: DeclValue) -> Decl {
        let idx = self.decls.len();
        self.decls.push(decl);

        let decl = Decl::from(idx);
        self.decl_start.insert(decl, start.pos);

        decl
    }

    pub fn get_decl_value(&self, decl: Decl) -> &DeclValue {
        self.decls.get(decl).expect("failed to get decl value")
    }

    pub fn set_decl_value(&mut self, decl: Decl, value: DeclValue) {
        let decl_value = self.decls.get_mut(decl).expect("failed to get decl value");
        *decl_value = value
    }

    pub fn get_decl_start(&self, decl: Decl) -> Pos {
        *self
            .decl_start
            .get(&decl)
            .expect("failed to get decl start")
    }

    pub fn get_stmt(&mut self, start: Token, stmt: StmtValue) -> Stmt {
        let idx = self.stmts.len();
        self.stmts.push(stmt);

        let stmt = Stmt::from(idx);
        self.stmt_start.insert(stmt, start.pos);

        stmt
    }

    pub fn get_stmt_value(&self, stmt: Stmt) -> &StmtValue {
        self.stmts.get(stmt).expect("failed to get stmt value")
    }

    pub fn get_stmt_start(&self, stmt: Stmt) -> Pos {
        *self
            .stmt_start
            .get(&stmt)
            .expect("failed to get stmt start")
    }

    pub fn get_expr(&mut self, start: Token, expr: ExprValue) -> Expr {
        let idx = self.exprs.len();
        self.exprs.push(expr);

        let expr = Expr::from(idx);
        self.expr_start.insert(expr, start.pos);

        expr
    }

    pub fn get_expr_value(&self, expr: Expr) -> &ExprValue {
        self.exprs.get(expr).expect("failed to get expr value")
    }

    pub fn get_expr_start(&self, expr: Expr) -> Pos {
        *self
            .expr_start
            .get(&expr)
            .expect("failed to get expr start")
    }
}

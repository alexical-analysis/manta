mod decl;
mod infix_expr;
mod prefix_expr;
mod stmt;

use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::ast::{BlockStmt, Decl, Expr, Stmt, StmtValue};
use crate::ast::lexer::{Token, Ty};
use crate::ast::parser::decl::parse_decl;
use crate::ast::parser::infix_expr::Precedence;
use crate::ast::parser::prefix_expr::parse_prefix_expr;
use crate::compiler::ast_store::AstStore;
use crate::compiler::context::Context;
use crate::compiler::module::{Module, ModuleValue};
use crate::compiler::str_store::{MStr, StrStore};
use crate::compiler::ty_store::UncheckedTyStore;
use crate::types::unchecked_ty::UncheckedTy;

use super::ast::{DeclValue, ExprValue};
use super::lexer::Lexer;

pub struct Parser<'ctx> {
    module_value: &'ctx mut ModuleValue,
    str_store: &'ctx mut StrStore,
    ty_store: &'ctx mut UncheckedTyStore,
    ast_store: &'ctx mut AstStore,
    infix_parselets: HashMap<Ty, Rc<dyn InfixExprParselet>>,
}

impl<'ctx> Parser<'ctx> {
    pub fn new(ctx: &'ctx mut Context, module: Module) -> Self {
        Self {
            module_value: ctx
                .module_store
                .get_mut(module)
                .expect("failed to get module value"),
            str_store: &mut ctx.str_store,
            ty_store: &mut ctx.unchecked_ty_store,
            ast_store: &mut ctx.ast_store,
            infix_parselets: expr_infix_parselets(),
        }
    }

    pub fn get_fn_type(
        &mut self,
        params: Vec<UncheckedTy>,
        return_type: UncheckedTy,
    ) -> UncheckedTy {
        self.ty_store.get_fn_type(params, return_type)
    }

    pub fn get_decl(&mut self, start: Token, decl: DeclValue) -> Decl {
        self.ast_store.get_decl(start, decl)
    }

    fn get_decl_value(&self, decl: Decl) -> &DeclValue {
        self.ast_store.get_decl_value(decl)
    }

    fn update_decl_value(&mut self, decl: Decl, value: DeclValue) {
        self.ast_store.set_decl_value(decl, value)
    }

    pub fn get_expr(&mut self, start: Token, expr: ExprValue) -> Expr {
        self.ast_store.get_expr(start, expr)
    }

    pub fn get_stmt(&mut self, start: Token, stmt: StmtValue) -> Stmt {
        self.ast_store.get_stmt(start, stmt)
    }

    pub fn parse(&mut self, source: &str) {
        todo!("need to get the actual base");
        let mut lexer = Lexer::new(self.str_store, source, 0);

        let mut decls = vec![];
        loop {
            let token = lexer.peek().clone();
            if token.ty == Ty::Eof {
                break;
            }

            let decl = parse_decl(self, &mut lexer, token);
            decls.push(decl);

            // expect a semicolon after each declaration
            let end = lexer.next(self.str_store);
            if end.ty != Ty::Semicolon {
                lexer.recover_until_decl(self.str_store);
                let decl = self.get_decl(
                    end,
                    DeclValue::Invalid("missing semicolon/new line after declaration"),
                );
                decls.push(decl);
            }
        }

        // make sure the first decl is a module name and set it on the module
        match decls.first() {
            Some(decl) => {
                let decl_value = self.get_decl_value(*decl);
                match decl_value {
                    DeclValue::Mod(module) => {
                        self.module_value.name = module.name;
                    }
                    _ => {
                        self.update_decl_value(
                            *decl,
                            DeclValue::Invalid("missing module name, first declaration in a file must be a module name decl")
                        );
                    }
                }
            }
            None => {
                let token = Token::new_eof(self.str_store, lexer.get_pos());
                self.get_decl(token, DeclValue::Invalid("missing module name"));
            }
        };

        // make sure that after the 2nd decl, use and mod never appear
        for decl in decls.iter().skip(2) {
            let decl_value = self.get_decl_value(*decl);
            match decl_value {
                DeclValue::Mod(_) => self.update_decl_value(
                    *decl,
                    DeclValue::Invalid("module name can only appear as the first decl in a file"),
                ),
                DeclValue::Use(_) => self.update_decl_value(
                    *decl,
                    DeclValue::Invalid(
                        "the use block can only appear directly after the module name in a file",
                    ),
                ),
                _ => continue,
            };
        }
    }

    /// if the 2nd decl is a use block make sure module dependencies get set on the module
    pub fn set_deps(&mut self, module_map: &HashMap<MStr, Module>) {
        let decl = self.module_value.ast.iter().nth(1);
        let decl = match decl {
            Some(decl) => decl,
            None => return,
        };

        let decl_value = self.get_decl_value(*decl);
        let use_decl = match decl_value {
            DeclValue::Use(decl) => decl.clone(),
            _ => return,
        };

        for &dep in &use_decl.deps {
            let &module = module_map
                .get(&dep.name)
                .expect("failed to find dependent module");

            self.module_value.deps.insert(dep.name, module);
        }
    }

    fn parse_expr(&mut self, lexer: &mut Lexer, min_precedence: Precedence) -> Expr {
        let token = lexer.next(self.str_store);

        let mut left = parse_prefix_expr(self, lexer, token);

        loop {
            let token = lexer.peek();
            if token.ty == Ty::Eof || token.ty == Ty::Semicolon {
                break;
            }

            let infix_parselet = match self.infix_parselets.get(&token.ty) {
                Some(parselet) => parselet.clone(),
                None => break,
            };

            if infix_parselet.precedence() <= min_precedence {
                break;
            }

            let token = lexer.next(self.str_store);
            left = infix_parselet.parse(self, lexer, left, token);
        }

        left
    }

    /// parsers a manta block, it's expected that the caller will have already lexed the leading '{'
    /// and when the function returns the trailing '}' will be lexed. Any trailing Semicolon will not
    /// be parsed.
    fn parse_block(&mut self, lexer: &mut Lexer) -> BlockStmt {
        let mut stmts = vec![];

        loop {
            let close = lexer.peek();
            match close.ty {
                Ty::CloseBrace => {}
                Ty::Eof => {
                    let error =
                        self.get_stmt(*close, StmtValue::Invalid("unclosed block expression"));
                    stmts.push(error);
                    break;
                }
                _ => {
                    let stmt = self.parse_stmt(lexer, Precedence::Base);
                    stmts.push(stmt);
                }
            }
        }

        BlockStmt { stmts }
    }

    pub fn parse_ty(&mut self, lexer: &mut Lexer, s: MStr) -> UncheckedTy {
        match self.get_str(s) {
            "i64" => self.ty_store.get_i64_ty(),
            "f64" => self.ty_store.get_f64_ty(),
            "none" => self.ty_store.get_none_ty(),
            "bool" => self.ty_store.get_bool_ty(),
            _ => match lexer.peek().ty {
                Ty::DotDot => {
                    lexer.next(self.str_store);
                    let name = lexer.next(self.str_store);
                    if name.ty != Ty::Identifier {
                        todo!("invalid module identifier, need to make an invalid type")
                    }

                    self.ty_store.get_named_ty(Some(s), name.lexeme)
                }
                _ => self.ty_store.get_named_ty(None, s),
            },
        }
    }

    fn get_str(&mut self, s: MStr) -> &str {
        self.str_store.get_str(s)
    }
}

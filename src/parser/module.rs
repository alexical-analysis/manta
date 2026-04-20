use serde::Serialize;
use std::collections::BTreeMap;

use crate::ast::{BlockStmt, Decl, Expr, LetExcept, Pattern, Payload, Stmt, TypeSpec};
use crate::parser::ParseError;
use crate::parser::lexer::{Token, TokenKind};
use crate::str_store::{self, StrID, StrStore};

use super::lexer::SourceID;

#[derive(Debug, Serialize)]
struct IDTracker {
    id: SymID,
}

impl IDTracker {
    fn new() -> Self {
        IDTracker { id: 0 }
    }

    fn next_id(&mut self) -> SymID {
        self.id += 1;
        self.id - 1
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum BindingType {
    Type(TypeSpec),
    Func(TypeSpec),
    Value,
}

pub type SymID = usize;

#[derive(Debug, Serialize)]
pub struct Binding {
    pub id: SymID,
    pub name: StrID,
    pub binding_type: BindingType,
    pub mutable: bool,
    pub used: bool,
}

#[derive(Debug, Copy, Clone, Serialize)]
pub struct ScopePos {
    scope_id: ScopeID,
    scope_depth: usize,
}

pub type ScopeID = usize;

#[derive(Debug, Serialize)]
struct Scope {
    parent: Option<ScopeID>,
    bindings: Vec<Binding>,
}

impl Scope {
    fn new(parent: ScopeID) -> Scope {
        Scope {
            parent: Some(parent),
            bindings: vec![],
        }
    }

    fn new_root(id_tracker: &mut IDTracker) -> Scope {
        let mut bindings = vec![];
        let builtin_types = vec![
            (str_store::U8, TypeSpec::UInt8),
            (str_store::U16, TypeSpec::UInt16),
            (str_store::U32, TypeSpec::UInt32),
            (str_store::U64, TypeSpec::UInt64),
            (str_store::I8, TypeSpec::Int8),
            (str_store::I16, TypeSpec::Int16),
            (str_store::I32, TypeSpec::Int32),
            (str_store::I64, TypeSpec::Int64),
            (str_store::F32, TypeSpec::Float32),
            (str_store::F64, TypeSpec::Float64),
            (str_store::BOOL, TypeSpec::Bool),
        ];

        for (name, type_spec) in builtin_types {
            bindings.push(Binding {
                id: id_tracker.next_id(),
                name,
                binding_type: BindingType::Type(type_spec),
                mutable: false,
                used: true, // don't warn on unused builtin types
            })
        }

        bindings.push(Binding {
            id: id_tracker.next_id(),
            name: str_store::STR,
            binding_type: BindingType::Type(TypeSpec::String),
            mutable: false,
            used: true, // don't warn if we don't use the str type
        });

        Scope {
            parent: None,
            bindings,
        }
    }
}

#[derive(Debug, Serialize)]
struct SymTable {
    scopes: Vec<Scope>,
    current_scope: ScopeID,
    scope_map: BTreeMap<SourceID, ScopePos>,
    id_tracker: IDTracker,
}

impl SymTable {
    fn new() -> SymTable {
        let mut id_tracker = IDTracker::new();
        SymTable {
            scopes: vec![Scope::new_root(&mut id_tracker)],
            current_scope: 0,
            scope_map: BTreeMap::new(),
            id_tracker,
        }
    }

    fn get_current_scope(&self) -> &Scope {
        match self.scopes.get(self.current_scope) {
            Some(s) => s,
            None => panic!("sym table has no current scope"),
        }
    }

    fn get_current_scope_mut(&mut self) -> &mut Scope {
        match self.scopes.get_mut(self.current_scope) {
            Some(s) => s,
            None => panic!("sym table has no current scope"),
        }
    }

    fn open_scope(&mut self, source_id: SourceID) {
        let new_scope = Scope::new(self.current_scope);
        let new_scope_id = self.scopes.len();

        self.scopes.push(new_scope);
        self.current_scope = new_scope_id;
        self.add_scope_pos(source_id);
    }

    fn add_scope_pos(&mut self, source_id: SourceID) {
        let scope_depth = self.get_current_scope().bindings.len();
        self.scope_map.insert(
            source_id,
            ScopePos {
                scope_id: self.current_scope,
                scope_depth,
            },
        );
    }

    fn close_scope(&mut self) {
        let current_scope = self.get_current_scope();
        match current_scope.parent {
            Some(p) => self.current_scope = p,
            None => panic!("can not close the root scope"),
        }
    }

    fn add_binding(&mut self, name: StrID, binding_type: BindingType, mutable: bool) {
        let b = Binding {
            id: self.id_tracker.next_id(),
            name,
            binding_type,
            mutable,
            used: false,
        };

        let current_scope = self.get_current_scope_mut();
        current_scope.bindings.push(b);
    }

    fn find_binding(&mut self, name: StrID) -> Option<&mut Binding> {
        let mut scope_id = Some(self.current_scope);

        while let Some(id) = scope_id {
            let binding_index = self
                .scopes
                .get(id)
                .expect("invalid scope id")
                .bindings
                .iter()
                .rposition(|b| b.name == name);

            if let Some(idx) = binding_index {
                return Some(&mut self.scopes.get_mut(id).unwrap().bindings[idx]);
            }

            scope_id = self.scopes.get(id).unwrap().parent;
        }

        None
    }

    fn find_binding_in_scope(&self, scope_pos: ScopePos, name: StrID) -> Option<&Binding> {
        self.scopes
            .get(scope_pos.scope_id)
            .expect("invalid scope id lookup in find_binding_in_scope");

        // check the target scope first only looking at bindings declared before the
        // scope_pos.scope_depth
        let mut scope = self
            .scopes
            .get(scope_pos.scope_id)
            .expect("failed to find root scope for binding");
        let binding_index = scope.bindings.as_slice()[..scope_pos.scope_depth]
            .iter()
            .rposition(|b| b.name == name);

        if let Some(idx) = binding_index {
            return Some(&scope.bindings[idx]);
        }

        // for all the remaining parent scopes just look at every binding
        while let Some(id) = scope.parent {
            scope = self
                .scopes
                .get(id)
                .expect("failed to find scope for binding");

            let binding_index = scope.bindings.iter().rposition(|b| b.name == name);

            if let Some(idx) = binding_index {
                return Some(&scope.bindings[idx]);
            }
        }

        None
    }
}

/// A file in a manta module
#[derive(Debug, Serialize)]
pub struct File {
    errors: Vec<ParseError>,
    decls: Vec<Decl>,
}

impl File {
    pub fn new(errors: Vec<ParseError>, decls: Vec<Decl>) -> Self {
        File { errors, decls }
    }
}

/// A module in a manta program
#[derive(Debug, Serialize)]
pub struct Module {
    name: StrID,
    decls: Vec<Decl>,
    errors: Vec<ParseError>,
    using_modules: Vec<StrID>,
    sym_table: SymTable,
}

impl Module {
    pub fn new(mut files: Vec<File>) -> Self {
        let mut decls = vec![];
        let mut errors = vec![];
        let mut using_modules = vec![];

        let mut module = None;
        for file in &mut files {
            // every file needs a module declaration
            let (file_module, mut mod_errors) = Self::get_module(&file.decls);
            errors.append(&mut mod_errors);

            match module {
                Some(m) => {
                    if file_module != m {
                        errors.push(ParseError::Custom(
                            // TODO: need to get the actual tokens here
                            Token {
                                kind: TokenKind::Identifier,
                                source_id: SourceID::from_usize(0),
                                lexeme_id: StrID::from_usize(0),
                            },
                            "multiple modules names in a single dir is not allowed".to_string(),
                        ))
                    }
                }
                None => module = Some(file_module),
            }

            let (mut using, mut use_errors) = Self::get_using(&file.decls);
            using_modules.append(&mut using);
            errors.append(&mut use_errors);

            decls.append(&mut file.decls);
            errors.append(&mut file.errors);
        }

        let name = match module {
            Some(n) => n,
            None => str_store::NIL,
        };

        let sym_table = Self::build_sym_table(&mut errors, &decls);

        Module {
            name,
            decls,
            errors,
            using_modules,
            sym_table,
        }
    }

    pub fn get_scope_pos(&self, source_id: SourceID) -> Option<ScopePos> {
        self.sym_table.scope_map.get(&source_id).copied()
    }

    pub fn get_decls(&self) -> &Vec<Decl> {
        &self.decls
    }

    pub fn find_binding(&self, scope_pos: ScopePos, name: StrID) -> Option<&Binding> {
        self.sym_table.find_binding_in_scope(scope_pos, name)
    }

    pub fn get_errors(&self) -> &Vec<ParseError> {
        &self.errors
    }

    pub fn get_name(&self, str_store: &StrStore) -> String {
        str_store
            .get_string(self.name)
            .expect("failed to get module name")
    }

    fn get_module(decls: &[Decl]) -> (StrID, Vec<ParseError>) {
        let mut module_name = str_store::NIL;
        let mut errors = vec![];
        for (i, decl) in decls.iter().enumerate() {
            if let Decl::Mod(module) = decl {
                if i == 0 {
                    module_name = module.name
                } else {
                    errors.push(ParseError::Custom(
                        // TODO: need to get the actual tokens here
                        Token {
                            kind: TokenKind::Identifier,
                            source_id: SourceID::from_usize(0),
                            lexeme_id: StrID::from_usize(0),
                        },
                        "only a single module name is allowed per file".to_string(),
                    ));
                }
            }
        }

        if module_name == str_store::NIL {
            errors.push(ParseError::Custom(
                // TODO: need to get the actual tokens here
                Token {
                    kind: TokenKind::Identifier,
                    source_id: SourceID::from_usize(0),
                    lexeme_id: StrID::from_usize(0),
                },
                "file is missing module name".to_string(),
            ));
        }

        (module_name, errors)
    }

    fn get_using(decls: &[Decl]) -> (Vec<StrID>, Vec<ParseError>) {
        let mut using_modules = vec![];
        let mut errors = vec![];
        for (i, decl) in decls.iter().enumerate() {
            if let Decl::Use(using) = decl {
                match i {
                    1 => using_modules = using.modules.clone(),
                    0 => {
                        errors.push(ParseError::Custom(
                            // TODO: get the real token for this
                            Token {
                                kind: TokenKind::Identifier,
                                source_id: SourceID::from_usize(0),
                                lexeme_id: StrID::from_usize(0),
                            },
                            "first declaration in a file must be the module name".to_string(),
                        ));
                    }
                    _ => {
                        errors.push(ParseError::Custom(
                        // TODO: get the real token for this
                        Token {
                            kind: TokenKind::Identifier,
                            source_id: SourceID::from_usize(0),
                            lexeme_id: StrID::from_usize(0),
                        },
                        "only a single import section allowed per file, and it must be right below the module name".to_string(),
                    ));
                    }
                }
            };
        }

        (using_modules, errors)
    }

    fn build_sym_table(errors: &mut Vec<ParseError>, decls: &[Decl]) -> SymTable {
        let mut sym_table = SymTable::new();
        for decl in decls {
            match decl {
                Decl::Function(decl) => {
                    // TODO: should add_binding and add_scope_pos always be coupled? Or maybe
                    // just for identifier declarations? In theory though all bindings are
                    // declarations right?
                    sym_table.add_binding(
                        decl.name,
                        BindingType::Func(TypeSpec::Function(decl.function_type.clone())),
                        false,
                    );
                    sym_table.add_scope_pos(decl.id);

                    Self::build_sym_table_type_spec(
                        errors,
                        &mut sym_table,
                        &TypeSpec::Function(decl.function_type.clone()),
                    );

                    sym_table.open_scope(decl.id);

                    for param in &decl.params {
                        // TODO: should add_binding and add_scope_pos always be coupled? Or maybe
                        // just for identifier declarations? In theory though all bindings are
                        // declarations right?
                        sym_table.add_binding(param.name, BindingType::Value, false);
                        sym_table.add_scope_pos(param.id);
                    }

                    Self::build_sym_table_block(errors, &mut sym_table, &decl.body);

                    sym_table.close_scope();
                }
                Decl::Type(decl) => {
                    match &decl.type_spec {
                        TypeSpec::Named(t) => {
                            if let Some(_module) = t.module {
                                // TODO: modules are not yet supported just skip things for now
                                continue;
                            }

                            match sym_table.find_binding(t.name) {
                                Some(b) => b.used = true,
                                None => panic!("unknown type (return this error)"),
                            }

                            sym_table.add_binding(
                                decl.name,
                                BindingType::Type(decl.type_spec.clone()),
                                false,
                            );
                            sym_table.add_scope_pos(t.id);
                        }
                        TypeSpec::Pointer(p) => {
                            sym_table.add_binding(
                                decl.name,
                                BindingType::Type(decl.type_spec.clone()),
                                false,
                            );

                            Self::build_sym_table_type_spec(errors, &mut sym_table, p);
                        }
                        TypeSpec::Slice(s) => {
                            sym_table.add_binding(
                                decl.name,
                                BindingType::Type(decl.type_spec.clone()),
                                false,
                            );

                            Self::build_sym_table_type_spec(errors, &mut sym_table, s);
                        }
                        TypeSpec::Array(a) => {
                            sym_table.add_binding(
                                decl.name,
                                BindingType::Type(decl.type_spec.clone()),
                                false,
                            );

                            Self::build_sym_table_type_spec(errors, &mut sym_table, &a.type_spec);
                        }
                        TypeSpec::Struct(s) => {
                            sym_table.add_binding(
                                decl.name,
                                BindingType::Type(decl.type_spec.clone()),
                                false,
                            );

                            for field in &s.fields {
                                Self::build_sym_table_type_spec(
                                    errors,
                                    &mut sym_table,
                                    &field.type_spec,
                                );
                            }
                        }
                        TypeSpec::Enum(e) => {
                            sym_table.add_binding(
                                decl.name,
                                BindingType::Type(decl.type_spec.clone()),
                                false,
                            );

                            for variant in &e.variants {
                                if let Some(payload) = &variant.payload {
                                    Self::build_sym_table_type_spec(
                                        errors,
                                        &mut sym_table,
                                        payload,
                                    );
                                }
                            }
                        }
                        TypeSpec::String => {
                            sym_table.add_binding(
                                decl.name,
                                BindingType::Type(decl.type_spec.clone()),
                                false,
                            );
                        }
                        _ => {
                            sym_table.add_binding(
                                decl.name,
                                BindingType::Type(decl.type_spec.clone()),
                                false,
                            );
                        }
                    }
                    sym_table.add_scope_pos(decl.id);
                }
                Decl::Const(decl) => {
                    sym_table.add_binding(decl.name, BindingType::Value, false);
                    sym_table.add_scope_pos(decl.id);
                }
                Decl::Var(decl) => {
                    sym_table.add_binding(decl.name, BindingType::Value, true);
                    sym_table.add_scope_pos(decl.id);
                }
                Decl::Use(_) => { /* nothing to do */ }
                Decl::Mod(_) => { /* nothing to do */ }
                Decl::Invalid => { /* nothing to do */ }
            }
        }

        sym_table
    }

    fn build_sym_table_block(
        errors: &mut Vec<ParseError>,
        sym_table: &mut SymTable,
        block: &BlockStmt,
    ) {
        sym_table.open_scope(block.id);

        for stmt in &block.statements {
            Self::build_sym_table_stmt(errors, sym_table, stmt);
        }

        sym_table.close_scope();
    }

    fn build_sym_table_stmt(errors: &mut Vec<ParseError>, sym_table: &mut SymTable, stmt: &Stmt) {
        match stmt {
            Stmt::Let(stmt) => {
                Self::build_sym_table_pattern(errors, sym_table, stmt.mutable, &stmt.pattern);
                Self::build_sym_table_expr(errors, sym_table, &stmt.value);

                match &stmt.except {
                    LetExcept::Or { id, binding, body } => {
                        sym_table.open_scope(*id);

                        if let Some(binding) = binding {
                            sym_table.add_binding(*binding, BindingType::Value, false);
                            sym_table.add_scope_pos(*id);
                        }

                        Self::build_sym_table_block(errors, sym_table, body);

                        sym_table.close_scope();
                    }
                    LetExcept::Wrap(expr) => {
                        Self::build_sym_table_expr(errors, sym_table, expr);
                    }
                    LetExcept::Panic => {}
                    LetExcept::None => {}
                }
            }
            Stmt::Assign(stmt) => {
                Self::build_sym_table_expr(errors, sym_table, &stmt.lvalue);
                Self::build_sym_table_expr(errors, sym_table, &stmt.rvalue);
            }
            Stmt::Expr(stmt) => {
                Self::build_sym_table_expr(errors, sym_table, &stmt.expr);
            }
            Stmt::Return(stmt) => {
                if let Some(value) = &stmt.value {
                    Self::build_sym_table_expr(errors, sym_table, value);
                }
            }
            Stmt::Defer(stmt) => {
                Self::build_sym_table_block(errors, sym_table, &stmt.block);
            }
            Stmt::Match(stmt) => {
                Self::build_sym_table_expr(errors, sym_table, &stmt.target);

                for arm in &stmt.arms {
                    sym_table.open_scope(arm.id);

                    Self::build_sym_table_pattern(errors, sym_table, false, &arm.pattern);
                    Self::build_sym_table_block(errors, sym_table, &arm.body);

                    sym_table.close_scope();
                }
            }
            Stmt::Block(stmt) => {
                Self::build_sym_table_block(errors, sym_table, stmt);
            }
            Stmt::If(stmt) => {
                Self::build_sym_table_expr(errors, sym_table, &stmt.check);
                Self::build_sym_table_block(errors, sym_table, &stmt.success);
                if let Some(fail) = &stmt.fail {
                    Self::build_sym_table_block(errors, sym_table, fail);
                }
            }
            Stmt::Loop(stmt) => {
                Self::build_sym_table_block(errors, sym_table, &stmt.body);
            }
            Stmt::While(stmt) => {
                Self::build_sym_table_expr(errors, sym_table, &stmt.check);
                Self::build_sym_table_block(errors, sym_table, &stmt.body);
            }
            Stmt::For(stmt) => {
                sym_table.add_binding(stmt.binding.name, BindingType::Value, false);
                sym_table.add_scope_pos(stmt.binding.id);

                Self::build_sym_table_block(errors, sym_table, &stmt.body);
            }
            Stmt::Break => { /* nothing to do here */ }
            Stmt::Continue => { /* nothing to do here */ }
        }
    }

    fn build_sym_table_pattern(
        errors: &mut Vec<ParseError>,
        sym_table: &mut SymTable,
        mutable: bool,
        pat: &Pattern,
    ) {
        match pat {
            Pattern::IntLiteral(_) => { /* no symbols to track */ }
            Pattern::UIntLiteral(_) => { /* no symbols to track */ }
            Pattern::StringLiteral(_) => { /* no symbols to track */ }
            Pattern::BoolLiteral(_) => { /* no symbols to track */ }
            Pattern::FloatLiteral(_) => { /* no symbols to track */ }
            Pattern::ModuleIdentifier(_) => { /* no symbols to track */ }
            Pattern::Default => { /* no symbols to track */ }
            Pattern::TypeSpec(pat) => {
                match pat.payload {
                    Payload::Some(payload) => {
                        sym_table.add_binding(payload, BindingType::Value, mutable);
                        sym_table.add_scope_pos(pat.id);
                    }
                    Payload::Default => { /*default patterns can just be ignored*/ }
                    Payload::None => panic!("type specs must have a payload"),
                }

                Self::build_sym_table_type_spec(errors, sym_table, &pat.type_spec);
            }
            Pattern::EnumVariant(pat) => {
                if let Some(enum_name) = &pat.enum_name {
                    sym_table.add_scope_pos(enum_name.id);
                }

                if let Payload::Some(payload) = pat.payload {
                    sym_table.add_binding(payload, BindingType::Value, mutable);
                }

                sym_table.add_scope_pos(pat.id);
            }
            Pattern::Identifier(pat) => {
                sym_table.add_binding(pat.name, BindingType::Value, mutable);
                sym_table.add_scope_pos(pat.id);
            }
        }
    }

    fn build_sym_table_expr(errors: &mut Vec<ParseError>, sym_table: &mut SymTable, expr: &Expr) {
        match expr {
            Expr::IntLiteral(_) => { /* nothing to do */ }
            Expr::UIntLiteral(_) => { /* nothing to do */ }
            Expr::FloatLiteral(_) => { /* nothing to do */ }
            Expr::StringLiteral(_) => { /* nothing to do */ }
            Expr::BoolLiteral(_) => { /* nothing to do */ }
            Expr::Identifier(expr) => match sym_table.find_binding(expr.name) {
                Some(b) => {
                    b.used = true;
                    sym_table.add_scope_pos(expr.id);
                }
                None => errors.push(ParseError::Custom(
                    // TODO: need the acutal token here, not just this placeholder
                    Token {
                        kind: TokenKind::Identifier,
                        source_id: SourceID::from_usize(0),
                        lexeme_id: StrID::from_usize(0),
                    },
                    "use of unknown identifier".to_string(),
                )),
            },
            Expr::Binary(expr) => {
                Self::build_sym_table_expr(errors, sym_table, &expr.left);
                Self::build_sym_table_expr(errors, sym_table, &expr.right);
            }
            Expr::Unary(expr) => {
                Self::build_sym_table_expr(errors, sym_table, &expr.operand);
            }
            Expr::Call(expr) => {
                Self::build_sym_table_expr(errors, sym_table, &expr.func);
                for arg in &expr.args {
                    Self::build_sym_table_expr(errors, sym_table, arg);
                }
            }
            Expr::StructConstructor(expr) => {
                Self::build_sym_table_type_spec(errors, sym_table, &expr.type_spec);
                for field in &expr.fields {
                    Self::build_sym_table_expr(errors, sym_table, &field.value);
                }
            }
            Expr::Index(expr) => {
                Self::build_sym_table_expr(errors, sym_table, &expr.target);
                Self::build_sym_table_expr(errors, sym_table, &expr.index);
            }
            Expr::Range(expr) => {
                Self::build_sym_table_expr(errors, sym_table, &expr.start);
                Self::build_sym_table_expr(errors, sym_table, &expr.end);
            }
            Expr::DotAccess(expr) => {
                if let Some(target) = &expr.target {
                    Self::build_sym_table_expr(errors, sym_table, target);
                }
            }
            Expr::MetaType(expr) => {
                Self::build_sym_table_type_spec(errors, sym_table, &expr.type_spec);
            }
            Expr::Alloc(expr) => {
                Self::build_sym_table_expr(errors, sym_table, &expr.meta_type);
                for opts in &expr.options {
                    Self::build_sym_table_expr(errors, sym_table, opts);
                }
            }
            Expr::Free(expr) => {
                Self::build_sym_table_expr(errors, sym_table, &expr.expr);
            }
        }
    }

    fn build_sym_table_type_spec(
        errors: &mut Vec<ParseError>,
        sym_table: &mut SymTable,
        type_spec: &TypeSpec,
    ) {
        match type_spec {
            TypeSpec::Named(t) => {
                if let Some(_module) = t.module {
                    // TODO: modules are not yet supported just skip things for now
                    errors.push(ParseError::Custom(
                        // TODO: need the actual token here
                        Token {
                            kind: TokenKind::Identifier,
                            source_id: SourceID::from_usize(0),
                            lexeme_id: StrID::from_usize(0),
                        },
                        "modules are not yet supported".to_string(),
                    ));
                    return;
                }

                match sym_table.find_binding(t.name) {
                    Some(b) => b.used = true,
                    None => errors.push(ParseError::Custom(
                        Token {
                            kind: TokenKind::Identifier,
                            source_id: SourceID::from_usize(0),
                            lexeme_id: StrID::from_usize(0),
                        },
                        "use of unknown type".to_string(),
                    )),
                }

                sym_table.add_scope_pos(t.id);
            }
            TypeSpec::Function(ts) => {
                for param in &ts.params {
                    Self::build_sym_table_type_spec(errors, sym_table, param);
                }
                Self::build_sym_table_type_spec(errors, sym_table, &ts.return_type);
            }
            TypeSpec::Pointer(ts) => {
                Self::build_sym_table_type_spec(errors, sym_table, ts);
            }
            TypeSpec::Slice(ts) => {
                Self::build_sym_table_type_spec(errors, sym_table, ts);
            }
            TypeSpec::Array(ts) => {
                Self::build_sym_table_type_spec(errors, sym_table, &ts.type_spec);
            }
            TypeSpec::Struct(ts) => {
                for field in &ts.fields {
                    Self::build_sym_table_type_spec(errors, sym_table, &field.type_spec);
                }
            }
            TypeSpec::Enum(ts) => {
                for variant in &ts.variants {
                    if let Some(payload) = &variant.payload {
                        Self::build_sym_table_type_spec(errors, sym_table, payload);
                    }
                }
            }
            TypeSpec::Int8 => {}
            TypeSpec::Int16 => {}
            TypeSpec::Int32 => {}
            TypeSpec::Int64 => {}
            TypeSpec::UInt8 => {}
            TypeSpec::UInt16 => {}
            TypeSpec::UInt32 => {}
            TypeSpec::UInt64 => {}
            TypeSpec::Float32 => {}
            TypeSpec::Float64 => {}
            TypeSpec::String => {}
            TypeSpec::Bool => {}
            TypeSpec::Unit => {}
        }
    }
}

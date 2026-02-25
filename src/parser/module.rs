use serde::Serialize;
use std::collections::BTreeMap;

use crate::ast::{BlockStmt, Decl, Expr, LetExcept, Pattern, Stmt, TypeSpec};
use crate::parser::ParseError;
use crate::parser::lexer::{Token, TokenKind};
use crate::str_store::{self, StrID};

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

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum BindingType {
    EnumType,
    StructType,
    FuncType,
    UnitType,
    Value,
}

pub type SymID = usize;

#[derive(Debug, Serialize)]
pub struct Binding {
    pub id: SymID,
    pub name: StrID,
    pub binding_type: BindingType,
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
            str_store::U8,
            str_store::U16,
            str_store::U32,
            str_store::U64,
            str_store::I8,
            str_store::I16,
            str_store::I32,
            str_store::I64,
            str_store::F32,
            str_store::F64,
            str_store::BOOL,
        ];

        for t in builtin_types {
            bindings.push(Binding {
                id: id_tracker.next_id(),
                name: t,
                binding_type: BindingType::UnitType,
                used: true, // don't warn on unused builtin types
            })
        }

        bindings.push(Binding {
            id: id_tracker.next_id(),
            name: str_store::STR,
            binding_type: BindingType::StructType,
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
    // TODO: this maps a source_id from the token to it's associated scope. In some ways this is
    // nice because the scope maps to the actual spot in the file where it opens. However it feels
    // a little messy in some ways too. I would rather use something like a NodeID but to do that
    // would require a HUGE refactor of the AST so I'm leaving it at this for now.
    scope_map: BTreeMap<usize, ScopePos>,
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

    fn open_scope(&mut self, source_id: usize) {
        let new_scope = Scope::new(self.current_scope);
        let new_scope_id = self.scopes.len();

        self.scopes.push(new_scope);
        self.current_scope = new_scope_id;
        self.add_scope_pos(source_id);
    }

    fn add_scope_pos(&mut self, source_id: usize) {
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

    fn add_binding(&mut self, name: StrID, binding_type: BindingType) {
        let b = Binding {
            id: self.id_tracker.next_id(),
            name,
            binding_type,
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

/// A module in a manta program
#[derive(Debug, Serialize)]
pub struct Module {
    name: StrID,
    using_modules: Vec<StrID>,
    errors: Vec<ParseError>,
    decls: Vec<Decl>,
    sym_table: SymTable,
}

impl Module {
    pub fn new(mut errors: Vec<ParseError>, decls: Vec<Decl>) -> Self {
        let (name, mut mod_errors) = Self::get_module(&decls);
        errors.append(&mut mod_errors);

        let (using_modules, mut use_errors) = Self::get_using(&decls);
        errors.append(&mut use_errors);

        let sym_table = Self::build_sym_table(&mut errors, &decls);

        Module {
            name,
            using_modules,
            errors,
            decls,
            sym_table,
        }
    }

    pub fn get_scope_pos(&self, source_id: usize) -> Option<ScopePos> {
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
                            source_id: 0,
                            lexeme_id: 0,
                        },
                        "only a single module name is allowed per file".to_string(),
                    ));
                }
            }
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
                                source_id: 0,
                                lexeme_id: 0,
                            },
                            "first declaration in a file must be the module name".to_string(),
                        ));
                    }
                    _ => {
                        errors.push(ParseError::Custom(
                        // TODO: get the real token for this
                        Token {
                            kind: TokenKind::Identifier,
                            source_id: 0,
                            lexeme_id: 0,
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
                    sym_table.add_binding(decl.name, BindingType::FuncType);
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
                        sym_table.add_binding(param.name, BindingType::Value);
                        sym_table.add_scope_pos(param.id);
                    }

                    Self::build_sym_table_block(errors, &mut sym_table, &decl.body);

                    sym_table.close_scope();
                }
                Decl::Type(decl) => {
                    match &decl.type_spec {
                        TypeSpec::Named { module, name } => {
                            if let Some(_module) = module {
                                // TODO: modules are not yet supported just skip things for now
                                continue;
                            }

                            let binding_type = match sym_table.find_binding(*name) {
                                Some(b) => {
                                    b.used = true;
                                    b.binding_type
                                }
                                None => panic!("unknown type (return this error)"),
                            };

                            sym_table.add_binding(decl.name, binding_type);
                        }
                        TypeSpec::Pointer(p) => {
                            sym_table.add_binding(decl.name, BindingType::UnitType);

                            Self::build_sym_table_type_spec(errors, &mut sym_table, p);
                        }
                        TypeSpec::Slice(s) => {
                            sym_table.add_binding(decl.name, BindingType::StructType);

                            Self::build_sym_table_type_spec(errors, &mut sym_table, s);
                        }
                        TypeSpec::Array(a) => {
                            sym_table.add_binding(decl.name, BindingType::UnitType);

                            Self::build_sym_table_type_spec(errors, &mut sym_table, &a.type_spec);
                        }
                        TypeSpec::Struct(s) => {
                            sym_table.add_binding(decl.name, BindingType::StructType);

                            for field in &s.fields {
                                Self::build_sym_table_type_spec(
                                    errors,
                                    &mut sym_table,
                                    &field.type_spec,
                                );
                            }
                        }
                        TypeSpec::Enum(e) => {
                            sym_table.add_binding(decl.name, BindingType::EnumType);

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
                            sym_table.add_binding(decl.name, BindingType::StructType);
                        }
                        _ => {
                            sym_table.add_binding(decl.name, BindingType::UnitType);
                        }
                    }
                    sym_table.add_scope_pos(decl.id);
                }
                Decl::Const(decl) => {
                    sym_table.add_binding(decl.name, BindingType::Value);
                    sym_table.add_scope_pos(decl.id);
                }
                Decl::Var(decl) => {
                    sym_table.add_binding(decl.name, BindingType::Value);
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
                if let Pattern::Payload(pat) = &stmt.pattern {
                    sym_table.add_binding(pat.payload.name, BindingType::Value);
                    sym_table.add_scope_pos(pat.payload.token.source_id);
                }

                if let Pattern::Identifier(ident) = &stmt.pattern {
                    sym_table.add_binding(ident.name, BindingType::Value);
                    sym_table.add_scope_pos(ident.token.source_id);
                }

                Self::build_sym_table_expr(errors, sym_table, &stmt.value);

                match &stmt.except {
                    LetExcept::Or { id, binding, body } => {
                        sym_table.open_scope(*id);

                        if let Some(binding) = binding {
                            sym_table.add_binding(*binding, BindingType::Value);
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

                    if let Pattern::Payload(pat) = &arm.pattern {
                        sym_table.add_binding(pat.payload.name, BindingType::Value);
                        sym_table.add_scope_pos(pat.payload.token.source_id);
                    }

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
        }
    }

    fn build_sym_table_expr(errors: &mut Vec<ParseError>, sym_table: &mut SymTable, expr: &Expr) {
        match expr {
            Expr::IntLiteral(_) => { /* nothing to do */ }
            Expr::FloatLiteral(_) => { /* nothing to do */ }
            Expr::StringLiteral(_) => { /* nothing to do */ }
            Expr::BoolLiteral(_) => { /* nothing to do */ }
            Expr::Identifier(expr) => match sym_table.find_binding(expr.name) {
                Some(b) => {
                    b.used = true;
                    sym_table.add_scope_pos(expr.token.source_id);
                }
                None => errors.push(ParseError::Custom(
                    Token {
                        kind: TokenKind::Identifier,
                        source_id: 0,
                        lexeme_id: 0,
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
            Expr::ModuleAccess(_expr) => {
                errors.push(ParseError::Custom(
                    // TODO: use the actual token here
                    Token {
                        kind: TokenKind::Identifier,
                        source_id: 0,
                        lexeme_id: 0,
                    },
                    "modules are not yet supported".to_string(),
                ));
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
            TypeSpec::Named { module, name } => {
                if let Some(_module) = module {
                    // TODO: modules are not yet supported just skip things for now
                    errors.push(ParseError::Custom(
                        Token {
                            kind: TokenKind::Identifier,
                            source_id: 0,
                            lexeme_id: 0,
                        },
                        "modules are not yet supported".to_string(),
                    ));
                    return;
                }

                match sym_table.find_binding(*name) {
                    Some(b) => b.used = true,
                    None => errors.push(ParseError::Custom(
                        Token {
                            kind: TokenKind::Identifier,
                            source_id: 0,
                            lexeme_id: 0,
                        },
                        "use of unknown type".to_string(),
                    )),
                }
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
            _ => { /* nothing to do */ }
        }
    }
}

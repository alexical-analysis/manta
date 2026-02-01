use std::ops::Deref;

use crate::ast::{
    BlockStmt, Decl, Expr, IdentifierPat, LetExcept, LetStmt, Module, Pattern, Stmt, TypeSpec,
};
use crate::hir::{Node, NodeID, NodeTree};
use crate::str_store::{StrID, StrStore};

struct Binding {
    name: StrID,
    used: bool,
    mutable: bool,
    node: NodeID,
}

struct Type {
    name: StrID,
    kind: TypeKind,
    node: NodeID,
}

#[derive(Copy, Clone, PartialEq)]
enum TypeKind {
    Unit,
    Struct,
    Enum,
    Indexable,
}

type ScopeID = usize;

struct Scope {
    parent: Option<ScopeID>,
    bindings: Vec<Binding>,
    types: Vec<Type>,
}

impl Scope {
    fn new(parent: ScopeID) -> Self {
        Scope {
            parent: Some(parent),
            bindings: vec![],
            types: vec![],
        }
    }

    fn new_root(str_store: &StrStore) -> Self {
        // the root scope always needs to have the builtin types since they're
        // available in every package
        let builtin_types = vec![
            "u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "f32", "f64", "bool",
        ];

        let mut types = vec![];
        for t in builtin_types {
            if let Some(id) = str_store.find_id(t) {
                types.push(Type {
                    name: id,
                    kind: TypeKind::Unit,
                    // this isn't tied to any actual node so this ensures if we try to
                    // access the node we'll get a panic
                    node: usize::MAX,
                });
            }
        }

        if let Some(id) = str_store.find_id("str") {
            types.push(Type {
                name: id,
                kind: TypeKind::Struct,
                // this isn't tied to any actual node so this ensures if we try to
                // access the node we'll get a panic
                node: usize::MAX,
            });
        }

        Scope {
            parent: None,
            bindings: vec![],
            types,
        }
    }
}

struct SymTable {
    scopes: Vec<Scope>,
    current_scope: ScopeID,
}

impl SymTable {
    fn new(str_store: &StrStore) -> Self {
        let root = Scope::new_root(str_store);
        SymTable {
            scopes: vec![root],
            current_scope: 0,
        }
    }

    fn open_scope(&mut self) {
        let parent = self.current_scope;
        self.scopes.push(Scope::new(parent));

        self.current_scope = self.scopes.len() - 1;
    }

    fn close_scope(&mut self) {
        let scope = self.must_get_scope();
        match scope.parent {
            Some(s) => self.current_scope = s,
            None => panic!("can not close the root scope"),
        }
    }

    // TODO: bindings and types should actually be unified so we don't have bidings
    // fighting in a single scope
    // also, I think functions should be included in the bindings as well
    // especially once we start adding closures
    fn add_binding(&mut self, b: Binding) {
        let scope = self.must_get_mut_scope();
        scope.bindings.push(b);
    }

    fn add_type(&mut self, t: Type) {
        let scope = self.must_get_mut_scope();
        scope.types.push(t)
    }

    fn get_binding(&self, name: StrID) -> Option<&Binding> {
        let mut scope = Some(self.must_get_scope());
        while scope.is_some() {
            let s = scope.unwrap();
            match s.bindings.iter().find(|b| b.name == name) {
                Some(b) => return Some(b),
                None => match s.parent {
                    Some(s) => scope = self.scopes.get(s),
                    None => scope = None,
                },
            }
        }

        None
    }

    fn get_type(&self, name: StrID) -> Option<&Type> {
        let mut scope = Some(self.must_get_scope());
        while scope.is_some() {
            let s = scope.unwrap();
            match s.types.iter().find(|t| t.name == name) {
                Some(t) => return Some(t),
                None => match s.parent {
                    Some(s) => scope = self.scopes.get(s),
                    None => scope = None,
                },
            }
        }

        None
    }

    fn must_get_scope(&self) -> &Scope {
        match self.scopes.get(self.current_scope) {
            Some(s) => s,
            None => panic!("invalid ScopeID in SymTable"),
        }
    }

    fn must_get_mut_scope(&mut self) -> &mut Scope {
        match self.scopes.get_mut(self.current_scope) {
            Some(s) => s,
            None => panic!("invalid ScopeID in SymTable"),
        }
    }
}

pub struct Noder {
    panic_id: StrID,
    wrap_id: StrID,
}

impl Noder {
    pub fn new(str_store: &mut StrStore) -> Self {
        // TODO: should we get all the necessary keywords here?
        // maybe keywords like panic etc should be hard coded since we need to use.
        // them in a bunch of places.

        // used in `let .Ok = expr() !` style expressions
        let panic_id = str_store.get_id("panic");

        // used in `let .Ok = expr() wrap .Err` style expressions
        let wrap_id = str_store.get_id("w");
        Noder { panic_id, wrap_id }
    }

    pub fn node(&self, str_store: &StrStore, module: Module) -> NodeTree {
        // Should these types be owned by the Noder type?
        let mut node_tree = NodeTree::new();
        let mut sym_table = SymTable::new(str_store);

        for (i, decl) in module.into_iter().enumerate() {
            if i == 0 && !matches!(decl, Decl::Mod(_)) {
                // TODO: should be a error not a panic like this
                // update it once we have a good error system
                panic!("first line must be a module")
            }

            self.node_decl(&mut sym_table, &mut node_tree, decl);
        }
        todo!()
    }

    fn node_decl(&self, sym_table: &mut SymTable, node_tree: &mut NodeTree, decl: &Decl) {
        match decl {
            Decl::Function(decl) => {
                sym_table.open_scope();

                let mut params = vec![];
                for param in &decl.params {
                    let param_id = node_tree.add_node(Node::VarDecl {
                        name: param.name,
                        type_spec: Some(param.type_spec.clone()),
                    });
                    params.push(param_id);

                    sym_table.add_binding(Binding {
                        name: param.name,
                        used: false,
                        mutable: false, // params are immutable
                        node: param_id,
                    });
                }

                let body_id = self.node_block(sym_table, node_tree, &decl.body);

                let decl_id = node_tree.add_root_node(Node::FunctionDecl {
                    name: decl.name,
                    params,
                    return_type: decl.return_type.clone(),
                    body: body_id,
                });

                sym_table.add_binding(Binding {
                    name: decl.name,
                    used: false,
                    mutable: false,
                    node: decl_id,
                });

                sym_table.close_scope();
            }
            Decl::Type(decl) => {
                // create the node
                let decl_id = node_tree.add_root_node(Node::TypeDecl {
                    name: decl.name,
                    type_spec: decl.type_spec.clone(),
                });

                let kind = match decl.type_spec {
                    TypeSpec::Array(_) => TypeKind::Indexable,
                    TypeSpec::Slice(_) => TypeKind::Indexable,
                    TypeSpec::Enum(_) => TypeKind::Enum,
                    TypeSpec::Struct(_) => TypeKind::Struct,
                    TypeSpec::Named { module, name } => {
                        if module.is_some() {
                            todo!("need to handle modules");
                        }

                        match sym_table.get_type(name) {
                            Some(t) => t.kind,
                            // TODO: use the error reporter instead of just panicing
                            None => panic!("unknown type!"),
                        }
                    }
                    _ => TypeKind::Unit, // all other types are unit types
                };

                sym_table.add_type(Type {
                    name: decl.name,
                    kind,
                    node: decl_id,
                });
            }
            Decl::Const(decl) => {
                // create the nodes
                let decl_id = node_tree.add_root_node(Node::VarDecl {
                    name: decl.name,
                    type_spec: None,
                });
                let value_node = self.node_expr(sym_table, node_tree, &decl.value);
                node_tree.add_node(Node::Assign {
                    target: decl_id,
                    value: value_node,
                });

                // update the symbol table
                sym_table.add_binding(Binding {
                    name: decl.name,
                    used: false,
                    mutable: false,
                    node: decl_id,
                });
            }
            Decl::Var(decl) => {
                // create the nodes
                let decl_id = node_tree.add_root_node(Node::VarDecl {
                    name: decl.name,
                    type_spec: None,
                });
                let value_node = self.node_expr(sym_table, node_tree, &decl.value);
                node_tree.add_node(Node::Assign {
                    target: decl_id,
                    value: value_node,
                });

                // update the symbol table
                sym_table.add_binding(Binding {
                    name: decl.name,
                    used: false,
                    mutable: true,
                    node: decl_id,
                });
            }
            Decl::Use(_) => todo!(
                "need to resolve the import graph and then import modules, this is unsupported for now"
            ),
            Decl::Mod(decl) => {
                let result = node_tree.set_module_name(decl.name);
                if result.is_err() {
                    panic!("error setting node name")
                }
            }
        }
    }
    fn node_block(
        &self,
        sym_table: &mut SymTable,
        node_tree: &mut NodeTree,
        block: &BlockStmt,
    ) -> NodeID {
        sym_table.open_scope();

        let mut stmt_ids = vec![];
        for stmt in &block.statements {
            let stmt_id = self.node_stmt(sym_table, node_tree, stmt);
            stmt_ids.push(stmt_id);
        }

        sym_table.close_scope();

        node_tree.add_node(Node::Block {
            statements: stmt_ids,
        })
    }
    fn node_stmt(&self, sym_table: &mut SymTable, node_tree: &mut NodeTree, stmt: &Stmt) -> NodeID {
        match stmt {
            Stmt::Let(stmt) => self.node_let(sym_table, node_tree, stmt),
            Stmt::Assign(stmt) => {
                let l_id = self.node_expr(sym_table, node_tree, &stmt.lvalue);
                let r_id = self.node_expr(sym_table, node_tree, &stmt.rvalue);

                // TODO: should i check that l_id is an assignable node here or should that
                // happen later when I do full type checking? (defaulting to later for now)
                node_tree.add_node(Node::Assign {
                    target: l_id,
                    value: r_id,
                })
            }
            Stmt::Expr(stmt) => self.node_expr(sym_table, node_tree, &stmt.expr),
            Stmt::Return(stmt) => {
                let value = if let Some(v) = &stmt.value {
                    let value_id = self.node_expr(sym_table, node_tree, v);
                    Some(value_id)
                } else {
                    None
                };

                node_tree.add_node(Node::Return { value })
            }
            Stmt::Defer(stmt) => {
                let block_id = self.node_block(sym_table, node_tree, &stmt.block);
                node_tree.add_node(Node::Defer { block: block_id })
            }
            Stmt::Match(stmt) => {
                let target_id = self.node_expr(sym_table, node_tree, &stmt.target);
                let mut arms = vec![];
                for arm in &stmt.arms {
                    let block_id = self.node_block(sym_table, node_tree, &arm.body);

                    let arm_id = node_tree.add_node(Node::MatchArm {
                        pattern: arm.pattern.clone(),
                        body: block_id,
                    });

                    arms.push(arm_id);
                }

                node_tree.add_node(Node::Match {
                    target: target_id,
                    arms,
                })
            }
            Stmt::Block(stmt) => self.node_block(sym_table, node_tree, stmt),
            Stmt::If(stmt) => {
                let check_id = self.node_expr(sym_table, node_tree, &stmt.check);
                let success_id = self.node_block(sym_table, node_tree, &stmt.success);
                let fail_id = stmt
                    .fail
                    .as_ref()
                    .map(|fail| self.node_block(sym_table, node_tree, fail));

                node_tree.add_node(Node::If {
                    condition: check_id,
                    then_block: success_id,
                    else_block: fail_id,
                })
            }
        }
    }

    fn node_let(
        &self,
        sym_table: &mut SymTable,
        node_tree: &mut NodeTree,
        stmt: &LetStmt,
    ) -> NodeID {
        let mut arms = vec![];
        let empty_body = node_tree.add_node(Node::Block { statements: vec![] });
        if let Pattern::Payload(pat) = &stmt.pattern {
            let type_spec = match pat.pat.deref() {
                Pattern::TypeSpec(ts) => Some(ts.clone()),
                _ => None,
            };

            node_tree.add_node(Node::VarDecl {
                name: pat.payload,
                type_spec,
            });

            let pat_id = node_tree.add_node(Node::MatchArm {
                pattern: stmt.pattern.clone(),
                body: empty_body,
            });
            arms.push(pat_id);

            todo!("update the symbol table with the new binding");
        } else {
            let pat_id = node_tree.add_node(Node::MatchArm {
                pattern: stmt.pattern.clone(),
                body: empty_body,
            });
            arms.push(pat_id)
        };

        let default_id = match &stmt.except {
            LetExcept::Or { binding, body } => {
                let body_id = self.node_block(sym_table, node_tree, body);

                // TODO: need to update the sym_table for the match body here.
                match *binding {
                    Some(b) => node_tree.add_node(Node::MatchArm {
                        // TODO: sould we simplify this pattern to a Default and then
                        // a var decl. That's what we do for function params...
                        pattern: Pattern::Identifier(IdentifierPat { name: b }),
                        body: body_id,
                    }),
                    None => node_tree.add_node(Node::MatchArm {
                        pattern: Pattern::Default,
                        body: body_id,
                    }),
                }
            }
            LetExcept::Wrap(expr) => {
                let enum_id = self.node_wrap_expr(sym_table, node_tree, expr);
                if enum_id.is_none() {
                    panic!("not a valid target for a let wrap statement")
                }

                // TODO: need to wrap this in a block
                let body_id = node_tree.add_node(Node::Return { value: enum_id });

                // TODO: need to update the sym_table for the match body.
                // also body should be a block instead of just a regular expression so that
                // the sym_table works correctly. We don't want to bind values to outer scopes
                node_tree.add_node(Node::MatchArm {
                    pattern: Pattern::Identifier(IdentifierPat { name: self.wrap_id }),
                    body: body_id,
                })
            }
            LetExcept::Panic => {
                // TODO: can we reuse this node mabye?
                let panic_id = node_tree.add_node(Node::Identifier(self.panic_id));
                // TODO: need to actually pass the results of the call to the panic
                let body_id = node_tree.add_node(Node::Call {
                    func: panic_id,
                    args: vec![],
                });

                node_tree.add_node(Node::MatchArm {
                    pattern: Pattern::Default,
                    body: body_id,
                })
            }
            LetExcept::None => {
                // TODO: this is only leagl if the pattern can not
                // fail to match we need to be able to varify this
                // at compile time maybe just treat this as `unreachable`?
                // right now we'll just panic...

                // TODO: can we reuse this node mabye?
                let panic_id = node_tree.add_node(Node::Identifier(self.panic_id));
                // TODO: need to actually pass the results of the call to the panic
                let body_id = node_tree.add_node(Node::Call {
                    func: panic_id,
                    args: vec![],
                });

                node_tree.add_node(Node::MatchArm {
                    pattern: Pattern::Default,
                    body: body_id,
                })
            }
        };

        arms.push(default_id);

        let value_id = self.node_expr(sym_table, node_tree, &stmt.value);
        node_tree.add_node(Node::Match {
            target: value_id,
            arms,
        });

        // TODO: we're generating two new nodes here, how do we return both?
        // maybe a seq node? It can't be a block because the var delc needs to be in the parent scope
        // and sticking everything in a block will keep that from escpaing
        // seq nodes would be pretty easy to flatten in an checking pass...
        todo!()
    }

    /// convert an abitrary expression into a wrap node for the `let .Ok = expr wrap .Err` syntax
    /// if the given expression is not a valid enum expression None is returned instead.
    fn node_wrap_expr(
        &self,
        sym_table: &SymTable,
        node_tree: &mut NodeTree,
        expr: &Expr,
    ) -> Option<NodeID> {
        let dot_expr = match expr {
            Expr::DotAccess(expr) => expr,
            _ => return None,
        };

        let variant = dot_expr.field.name;
        let target = match &dot_expr.target {
            Some(target) => target,
            // TODO: how do we check this more carfully, we need to fill in the type hole first
            None => {
                let wrap_id = node_tree.add_node(Node::Identifier(self.wrap_id));
                let enum_id = node_tree.add_node(Node::EnumConstructor {
                    target: None,
                    variant,
                    payload: Some(wrap_id),
                });
                return Some(enum_id);
            }
        };

        let target = match target.deref() {
            Expr::Identifier(expr) => Some(expr.name),
            // if we have a non-identifier target this isn't an enum variant
            _ => return None,
        };

        if let Some(target) = target {
            match sym_table.get_type(target) {
                Some(t) => {
                    // if the identifier is a declared enum type we know this is a valid enum expression
                    if t.kind != TypeKind::Enum {
                        return None;
                    }
                }
                // this is not a declared type, it could be a struct value though
                None => return None,
            };
        }

        let wrap_id = node_tree.add_node(Node::Identifier(self.wrap_id));
        let enum_id = node_tree.add_node(Node::EnumConstructor {
            target,
            variant,
            payload: Some(wrap_id),
        });

        Some(enum_id)
    }

    fn node_expr(&self, sym_table: &mut SymTable, node_tree: &mut NodeTree, expr: &Expr) -> NodeID {
        match expr {
            Expr::IntLiteral(expr) => node_tree.add_node(Node::IntLiteral(*expr)),
            Expr::FloatLiteral(expr) => node_tree.add_node(Node::FloatLiteral(*expr)),
            Expr::StringLiteral(expr) => node_tree.add_node(Node::StringLiteral(*expr)),
            Expr::BoolLiteral(expr) => node_tree.add_node(Node::BoolLiteral(*expr)),
            Expr::Identifier(expr) => match sym_table.get_binding(expr.name) {
                // make sure this binding exists before we dereference it
                // TODO: should I check type information here?
                Some(_) => node_tree.add_node(Node::Identifier(expr.name)),
                None => panic!("unknown identifier"),
            },
            Expr::Binary(expr) => {
                let left_id = self.node_expr(sym_table, node_tree, &expr.left);
                let right_id = self.node_expr(sym_table, node_tree, &expr.right);
                node_tree.add_node(Node::Binary {
                    left: left_id,
                    operator: expr.operator,
                    right: right_id,
                })
            }
            Expr::Unary(expr) => {
                let expr_id = self.node_expr(sym_table, node_tree, &expr.operand);
                node_tree.add_node(Node::Unary {
                    operator: expr.operator,
                    operand: expr_id,
                })
            }
            // TODO: this could be a payload on an enum expression. We need to check for that
            Expr::Call(expr) => {
                // TODO: check the func type, if the node is an EnumConstructor then we need to
                // update the payload instead of treating it like a call
                let func_id = self.node_expr(sym_table, node_tree, &expr.func);

                let mut args = vec![];
                for arg in &expr.args {
                    let param_id = self.node_expr(sym_table, node_tree, arg);
                    args.push(param_id);
                }

                node_tree.add_node(Node::Call {
                    func: func_id,
                    args,
                })
            }
            Expr::Index(expr) => {
                let target_id = self.node_expr(sym_table, node_tree, &expr.target);
                let idx_id = self.node_expr(sym_table, node_tree, &expr.index);

                node_tree.add_node(Node::Index {
                    target: target_id,
                    index: idx_id,
                })
            }
            Expr::Range(expr) => {
                let start_id = self.node_expr(sym_table, node_tree, &expr.start);
                let end_id = self.node_expr(sym_table, node_tree, &expr.end);
                node_tree.add_node(Node::Range {
                    start: start_id,
                    end: end_id,
                })
            }
            // TODO: this is a rats nest of conditionals, we really should clean this up
            // maybe it can use self.node_expr() and then check the node type after the fact
            Expr::DotAccess(expr) => match &expr.target {
                Some(target) => match target.deref() {
                    Expr::Identifier(ident) => {
                        let binding = sym_table.get_binding(ident.name);
                        if binding.is_some() {
                            // if this is a known binding then it's not an enum type
                            let target_id = self.node_expr(sym_table, node_tree, target);
                            return node_tree.add_node(Node::FieldAccess {
                                target: Some(target_id),
                                field: expr.field.name,
                            });
                        }

                        let type_binding = sym_table.get_type(ident.name);
                        if let Some(t) = type_binding {
                            if t.kind == TypeKind::Enum {
                                let target_id = self.node_expr(sym_table, node_tree, target);
                                return node_tree.add_node(Node::EnumConstructor {
                                    target: Some(target_id),
                                    variant: expr.field.name,
                                    payload: None,
                                });
                            };
                        }

                        panic!("unknown identifier")
                    }
                    _ => {
                        let target_id = self.node_expr(sym_table, node_tree, target);
                        node_tree.add_node(Node::FieldAccess {
                            target: Some(target_id),
                            field: expr.field.name,
                        })
                    }
                },
                // if there's no target it must be and enum
                None => node_tree.add_node(Node::EnumConstructor {
                    target: None,
                    variant: expr.field.name,
                    payload: None,
                }),
            },
            Expr::ModuleAccess(_expr) => todo!("modules are not yet supported"),
            Expr::MetaType(expr) => node_tree.add_node(Node::MetaType {
                type_spec: expr.type_spec.clone(),
            }),
            Expr::Alloc(expr) => {
                let meta_id = self.node_expr(sym_table, node_tree, &expr.meta_type);
                let mut options = vec![];
                for opt in &expr.options {
                    let opt_id = self.node_expr(sym_table, node_tree, opt);
                    options.push(opt_id);
                }

                node_tree.add_node(Node::Alloc {
                    meta_type: meta_id,
                    options,
                })
            }
            Expr::Free(expr) => {
                let ptr_id = self.node_expr(sym_table, node_tree, &expr.expr);
                node_tree.add_node(Node::Free { expr: ptr_id })
            }
        }
    }
}

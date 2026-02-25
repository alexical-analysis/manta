use serde::Serialize;
use std::cmp::Ord;
use std::collections::BTreeMap;
use std::ops::Deref;

use crate::ast::{
    BlockStmt, Decl, Expr, FunctionType, LetExcept, LetStmt, Pattern, Stmt, StructField,
    StructType, TypeSpec,
};
use crate::hir::{Node, NodeID, PatternNode};
use crate::parser::module::{BindingType, Module, SymID};
use crate::str_store;

#[derive(Serialize)]
struct SideTable<K, V> {
    keys: BTreeMap<K, usize>,
    values: Vec<V>,
}

impl<K: Ord, V> SideTable<K, V> {
    fn new() -> Self {
        SideTable {
            keys: BTreeMap::new(),
            values: vec![],
        }
    }

    fn add(&mut self, key: K, value: V) {
        if self.keys.contains_key(&key) {
            panic!("can not add the same node twice")
        }

        let id = self.values.len();
        self.values.push(value);
        self.keys.insert(key, id);
    }

    fn get(&self, key: K) -> Option<&V> {
        match self.keys.get(&key) {
            Some(i) => self.values.get(*i),
            _ => None,
        }
    }
}

/// NodeTree contains all the nodes for a given tree as well as tracking the tree roots
#[derive(Serialize)]
pub struct NodeTree {
    nodes: Vec<Node>,
    roots: Vec<NodeID>,
    type_map: SideTable<NodeID, TypeSpec>,
    symbol_map: SideTable<SymID, NodeID>,
}

impl NodeTree {
    /// Create a new NodeTree
    pub fn new() -> Self {
        NodeTree {
            nodes: vec![],
            roots: vec![],
            type_map: SideTable::new(),
            symbol_map: SideTable::new(),
        }
    }

    /// Add node adds a new node ot the store and returns its unique NodeID
    pub fn add_node(&mut self, node: Node) -> NodeID {
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    /// Adds a root node to the store and returns its unique NodeID
    pub fn add_root_node(&mut self, node: Node) -> NodeID {
        self.nodes.push(node);
        let id = self.nodes.len() - 1;
        self.roots.push(id);
        id
    }

    pub fn get_node(&self, node_id: NodeID) -> Option<&Node> {
        self.nodes.get(node_id)
    }

    pub fn get_mut_node(&mut self, node_id: NodeID) -> Option<&mut Node> {
        self.nodes.get_mut(node_id)
    }
}

pub fn node_module(module: Module) -> NodeTree {
    // Should these types be owned by the Noder type?
    let mut node_tree = NodeTree::new();

    for decl in module.get_decls() {
        node_decl(&mut node_tree, &module, decl);
    }

    node_tree
}

fn node_decl(node_tree: &mut NodeTree, module: &Module, decl: &Decl) {
    match decl {
        Decl::Function(decl) => {
            let mut params = vec![];
            for i in 0..decl.params.len() {
                let param = &decl.params[i];
                let param_type = &decl.function_type.params[i];

                let ident_id = node_tree.add_node(Node::Identifier(param.name));
                let param_id = node_tree.add_node(Node::VarDecl { ident: ident_id });
                params.push(param_id);

                node_tree.type_map.add(ident_id, param_type.clone());

                let scope_pos = module
                    .get_scope_pos(param.id)
                    .expect("missing scope position for function paramater");
                let binding = module
                    .find_binding(scope_pos, param.name)
                    .expect("missing binding for function parameter");

                node_tree.symbol_map.add(binding.id, ident_id);
            }

            let body_id = node_block(node_tree, module, &decl.body);

            let ident_id = node_tree.add_node(Node::Identifier(decl.name));
            let func_id = node_tree.add_root_node(Node::FunctionDecl {
                ident: ident_id,
                params,
                body: body_id,
            });

            node_tree
                .type_map
                .add(func_id, TypeSpec::Function(decl.function_type.clone()));

            let scope_pos = module
                .get_scope_pos(decl.id)
                .expect("missing scope_pos for function");
            let binding = module
                .find_binding(scope_pos, decl.name)
                .expect("missing binding for function");

            node_tree.symbol_map.add(binding.id, ident_id);
        }
        Decl::Type(decl) => {
            let ident_id = node_tree.add_node(Node::Identifier(decl.name));
            let decl_id = node_tree.add_root_node(Node::TypeDecl { ident: ident_id });

            node_tree.type_map.add(decl_id, decl.type_spec.clone());

            let scope_pos = module
                .get_scope_pos(decl.id)
                .expect("missing scope_posfor type decl");
            let binding = module
                .find_binding(scope_pos, decl.name)
                .expect("missing binding for type decl");

            node_tree.symbol_map.add(binding.id, ident_id);
        }
        Decl::Const(decl) => {
            let ident_id = node_tree.add_node(Node::Identifier(decl.name));
            node_tree.add_root_node(Node::VarDecl { ident: ident_id });

            let scope_pos = module
                .get_scope_pos(decl.id)
                .expect("missing scope_posfor const decl");
            let binding = module
                .find_binding(scope_pos, decl.name)
                .expect("missing binding for const decl");

            node_tree.symbol_map.add(binding.id, ident_id);

            let value_node = node_expr(node_tree, module, &decl.value);
            node_tree.add_root_node(Node::Assign {
                target: ident_id,
                value: value_node,
            });
        }
        Decl::Var(decl) => {
            let ident_id = node_tree.add_node(Node::Identifier(decl.name));
            node_tree.add_root_node(Node::VarDecl { ident: ident_id });

            let scope_pos = module
                .get_scope_pos(decl.token.source_id)
                .expect("missing scope_posfor var decl");
            let binding = module
                .find_binding(scope_pos, decl.name)
                .expect("missing binding for var decl");

            node_tree.symbol_map.add(binding.id, ident_id);

            let value_node = node_expr(node_tree, module, &decl.value);
            node_tree.add_root_node(Node::Assign {
                target: ident_id,
                value: value_node,
            });
        }
        Decl::Use(_) => { /* ignore these since they're handled by the parser */ }
        Decl::Mod(_) => { /* ignore these since they're handled by the parser */ }
        Decl::Invalid => {
            node_tree.add_root_node(Node::Invalid);
        }
    }
}
fn node_block(node_tree: &mut NodeTree, module: &Module, block: &BlockStmt) -> NodeID {
    let mut stmt_ids = vec![];
    for stmt in &block.statements {
        let ids = node_stmt(node_tree, module, stmt);
        stmt_ids.extend(ids);
    }

    node_tree.add_node(Node::Block {
        statements: stmt_ids,
    })
}

fn node_stmt(node_tree: &mut NodeTree, module: &Module, stmt: &Stmt) -> Vec<NodeID> {
    match stmt {
        Stmt::Let(stmt) => node_let(node_tree, module, stmt),
        Stmt::Assign(stmt) => {
            let l_id = node_expr(node_tree, module, &stmt.lvalue);
            let r_id = node_expr(node_tree, module, &stmt.rvalue);

            // TODO: should i check that l_id is an assignable node here or should that
            // happen later when I do full type checking? (defaulting to later for now)
            vec![node_tree.add_node(Node::Assign {
                target: l_id,
                value: r_id,
            })]
        }
        Stmt::Expr(stmt) => vec![node_expr(node_tree, module, &stmt.expr)],
        Stmt::Return(stmt) => {
            let value = if let Some(v) = &stmt.value {
                let value_id = node_expr(node_tree, module, v);
                Some(value_id)
            } else {
                None
            };

            vec![node_tree.add_node(Node::Return { value })]
        }
        Stmt::Defer(stmt) => {
            let block_id = node_block(node_tree, module, &stmt.block);
            vec![node_tree.add_node(Node::Defer { block: block_id })]
        }
        Stmt::Match(stmt) => {
            let target_id = node_expr(node_tree, module, &stmt.target);
            let mut arms = vec![];
            for arm in &stmt.arms {
                let pat_id = node_pattern(node_tree, module, &arm.pattern);
                let block_id = node_block(node_tree, module, &arm.body);

                let arm_id = node_tree.add_node(Node::MatchArm {
                    pattern: pat_id,
                    body: block_id,
                });

                arms.push(arm_id);
            }

            vec![node_tree.add_node(Node::Match {
                target: target_id,
                arms,
            })]
        }
        Stmt::Block(stmt) => vec![node_block(node_tree, module, stmt)],
        Stmt::If(stmt) => {
            let check_id = node_expr(node_tree, module, &stmt.check);
            let success_id = node_block(node_tree, module, &stmt.success);
            let fail_id = stmt
                .fail
                .as_ref()
                .map(|fail| node_block(node_tree, module, fail));

            vec![node_tree.add_node(Node::If {
                condition: check_id,
                then_block: success_id,
                else_block: fail_id,
            })]
        }
    }
}

fn node_pattern(node_tree: &mut NodeTree, module: &Module, pattern: &Pattern) -> NodeID {
    match pattern {
        Pattern::IntLiteral(pat) => {
            node_tree.add_node(Node::Pattern(PatternNode::IntLiteral(*pat)))
        }
        Pattern::StringLiteral(pat) => {
            node_tree.add_node(Node::Pattern(PatternNode::StringLiteral(*pat)))
        }
        Pattern::BoolLiteral(pat) => {
            node_tree.add_node(Node::Pattern(PatternNode::BoolLiteral(*pat)))
        }
        Pattern::FloatLiteral(pat) => {
            node_tree.add_node(Node::Pattern(PatternNode::FloatLiteral(*pat)))
        }
        Pattern::TypeSpec(_) => node_tree.add_node(Node::Pattern(PatternNode::TypeSpec)),
        Pattern::Payload(pat) => {
            let payload = &pat.payload;
            let ident_id = node_tree.add_node(Node::Identifier(payload.name));

            let scope_pos = module
                .get_scope_pos(payload.token.source_id)
                .expect("missing source position for the payload");
            let binding = module
                .find_binding(scope_pos, payload.name)
                .expect("missing binding for payload");

            node_tree.symbol_map.add(binding.id, ident_id);

            let pat_id = node_pattern(node_tree, module, &pat.pat);
            node_tree.add_node(Node::Pattern(PatternNode::Payload {
                pat: pat_id,
                payload_ident: ident_id,
            }))
        }
        Pattern::ModuleAccess(pat) => {
            let pat_id = node_pattern(node_tree, module, &pat.pat);
            node_tree.add_node(Node::Pattern(PatternNode::ModuleAccess {
                module: pat.module.name,
                pat: pat_id,
            }))
        }
        Pattern::DotAccess(pat) => {
            let target_id = pat
                .target
                .clone()
                .map(|t| node_pattern(node_tree, module, &t));

            node_tree.add_node(Node::Pattern(PatternNode::DotAccess {
                target: target_id,
                field: pat.field.name,
            }))
        }
        Pattern::Identifier(pat) => {
            node_tree.add_node(Node::Pattern(PatternNode::Identifier(pat.name)))
        }
        Pattern::Default => node_tree.add_node(Node::Pattern(PatternNode::Default)),
    }
}

fn node_let(node_tree: &mut NodeTree, module: &Module, stmt: &LetStmt) -> Vec<NodeID> {
    let mut nodes = vec![];
    let mut arms = vec![];
    let value_id = node_expr(node_tree, module, &stmt.value);

    match &stmt.pattern {
        Pattern::Identifier(ident) => {
            //
            // let ident = value
            //
            // becomes:
            //
            // decl ident
            // ident = value

            if stmt.except != LetExcept::None {
                panic!(
                    "identifier expressions can never faile and should not have an except handle"
                )
            }

            let ident_id = node_tree.add_node(Node::Identifier(ident.name));
            nodes.push(node_tree.add_node(Node::VarDecl { ident: ident_id }));

            let scope_pos = module
                .get_scope_pos(ident.token.source_id)
                .expect("missing scope_posfor function");
            let binding = module
                .find_binding(scope_pos, ident.name)
                .expect("missing binding for function");

            node_tree.symbol_map.add(binding.id, ident_id);

            let assign_id = node_tree.add_node(Node::Assign {
                target: ident_id,
                value: value_id,
            });
            nodes.push(assign_id);

            return nodes;
        }
        Pattern::Payload(pat) => {
            //
            // let Pat(ident) = value or(e) { ... }
            //
            // becomes:
            //
            // decl ident
            // match {
            //     Pat(<inner let>) { ident = <inner let> }
            //     e                { ... }
            // }

            let _type_spec = match pat.pat.deref() {
                Pattern::TypeSpec(ts) => Some(ts.clone()),
                _ => {
                    // TODO: we actually need to handle all the different cases here. For example
                    // if the pattern is a dot expression we need to look type information and the
                    // variant to figure out what the identifer type should actually be
                    // also, a lot of these should actually be errors like
                    // let 10(v) = 10 is not a valid pattern that we want to support
                    None
                }
            };

            // ouside variable declaration
            let name = pat.payload.name;
            let ident_id = node_tree.add_node(Node::Identifier(name));
            nodes.push(node_tree.add_node(Node::VarDecl { ident: ident_id }));

            let scope_pos = module
                .get_scope_pos(pat.payload.token.source_id)
                .expect("missing scope_posfor function");
            let binding = module
                .find_binding(scope_pos, name)
                .expect("missing binding for function");

            node_tree.symbol_map.add(binding.id, ident_id);

            // rebuild the pattern to use the same inner pattern and a new <inner let> payload
            let inner_ident_id = node_tree.add_node(Node::Identifier(str_store::INNERLET));
            let inner_pat_id = node_pattern(node_tree, module, &pat.pat);
            let pat_id = node_tree.add_node(Node::Pattern(PatternNode::Payload {
                pat: inner_pat_id,
                payload_ident: inner_ident_id,
            }));

            // set up the inner assignment using the identifer node created above
            let assign_id = node_tree.add_node(Node::Assign {
                target: ident_id,
                value: inner_ident_id,
            });

            // create the match arm for the successful let match
            let arm_id = node_tree.add_node(Node::MatchArm {
                pattern: pat_id,
                body: assign_id,
            });

            arms.push(arm_id);
        }
        _ => {
            //
            // let Pat = value or(e) { ... }
            //
            // becomes:
            //
            // match {
            //     Pat { }
            //     e   { ... }
            // }

            if stmt.except == LetExcept::None {
                panic!("let statement needs an exception handler")
            }

            let pat_id = node_pattern(node_tree, module, &stmt.pattern);
            let empty_body = node_tree.add_node(Node::Block { statements: vec![] });
            let arm_id = node_tree.add_node(Node::MatchArm {
                pattern: pat_id,
                body: empty_body,
            });
            arms.push(arm_id)
        }
    }

    let default_id = match &stmt.except {
        // TODO: binding should be an identifier so we have a symbol ID so we can map to an
        // identifer and pass it through successfully
        LetExcept::Or {
            token,
            binding,
            body,
        } => {
            match *binding {
                Some(b) => {
                    // or(e) { ... }
                    //
                    // becomes:
                    //
                    // e { ... }
                    let ident_id = node_tree.add_node(Node::Identifier(b));
                    let pat_id =
                        node_tree.add_node(Node::Pattern(PatternNode::Identifier(ident_id)));

                    let scope_pos = module
                        .get_scope_pos(token.source_id)
                        .expect("missing scope position for or binding");
                    let binding = module
                        .find_binding(scope_pos, b)
                        .expect("missing binding for or identifier");

                    node_tree.symbol_map.add(binding.id, ident_id);

                    let body_id = node_block(node_tree, module, body);
                    node_tree.add_node(Node::MatchArm {
                        pattern: pat_id,
                        body: body_id,
                    })
                }
                None => {
                    // or { ... }
                    //
                    // becomes:
                    //
                    // _ { ... }
                    let pat_id = node_tree.add_node(Node::Pattern(PatternNode::Default));

                    let body_id = node_block(node_tree, module, body);
                    node_tree.add_node(Node::MatchArm {
                        pattern: pat_id,
                        body: body_id,
                    })
                }
            }
        }
        LetExcept::Wrap(expr) => {
            // wrap .Variant
            //
            // becomes:
            //
            // <wrap> { return .Variant(<wrap>) }
            let ident_id = node_tree.add_node(Node::Identifier(str_store::WRAP));
            let enum_id = node_wrap_expr(node_tree, module, expr, ident_id);

            let ret_id = node_tree.add_node(Node::Return {
                value: Some(enum_id),
            });
            let body_id = node_tree.add_node(Node::Block {
                statements: vec![ret_id],
            });

            let pat_id =
                node_tree.add_node(Node::Pattern(PatternNode::Identifier(str_store::WRAP)));
            node_tree.add_node(Node::MatchArm {
                pattern: pat_id,
                body: body_id,
            })
        }
        LetExcept::Panic => {
            // !
            //
            // becomes:
            //
            // <panic> { panic(<panic>) }

            // the identifier for the function call
            let panic_id = node_tree.add_node(Node::Identifier(str_store::PANIC));

            node_tree.type_map.add(
                panic_id,
                TypeSpec::Function(FunctionType {
                    // TODO: this needs to be the type of the expression that's being matched or an
                    // any type like in Go, just pick a random type for now
                    params: vec![TypeSpec::String],
                    return_type: Some(Box::new(TypeSpec::Panic)),
                }),
            );

            // the identifier for the pattern, this needs to be seperate from the function call
            // ideentifier for type checking to work correctly since they need to have different
            // underlying types
            let ident_id = node_tree.add_node(Node::Identifier(str_store::PANIC));

            let call_id = node_tree.add_node(Node::Call {
                func: panic_id,
                args: vec![ident_id],
            });
            let body_id = node_tree.add_node(Node::Block {
                statements: vec![call_id],
            });

            let pat_id = node_tree.add_node(Node::Pattern(PatternNode::Identifier(ident_id)));
            node_tree.add_node(Node::MatchArm {
                pattern: pat_id,
                body: body_id,
            })
        }
        LetExcept::None => {
            // TODO: we need to check that this expression can not fail to match
            // for now just panic if we hit this arm somehow

            // the identifier for the function call
            let panic_id = node_tree.add_node(Node::Identifier(str_store::PANIC));

            node_tree.type_map.add(
                panic_id,
                TypeSpec::Function(FunctionType {
                    // TODO: this needs to be the type of the expression that's being matched or an
                    // any type like in Go, just pick a random type for now
                    params: vec![TypeSpec::String],
                    return_type: Some(Box::new(TypeSpec::Panic)),
                }),
            );

            // the identifier for the pattern, this needs to be seperate from the function call
            // ideentifier for type checking to work correctly since they need to have different
            // underlying types
            let ident_id = node_tree.add_node(Node::Identifier(str_store::PANIC));

            let call_id = node_tree.add_node(Node::Call {
                func: panic_id,
                args: vec![ident_id],
            });
            let body_id = node_tree.add_node(Node::Block {
                statements: vec![call_id],
            });

            let pat_id = node_tree.add_node(Node::Pattern(PatternNode::Identifier(ident_id)));
            node_tree.add_node(Node::MatchArm {
                pattern: pat_id,
                body: body_id,
            })
        }
    };

    arms.push(default_id);

    let match_id = node_tree.add_node(Node::Match {
        target: value_id,
        arms,
    });
    nodes.push(match_id);

    nodes
}

/// convert an abitrary expression into a wrap node for the `let .Ok = expr wrap .Err` syntax
fn node_wrap_expr(
    node_tree: &mut NodeTree,
    module: &Module,
    expr: &Expr,
    wrap_id: NodeID,
) -> NodeID {
    let dot_expr = match expr {
        Expr::DotAccess(expr) => expr,
        _ => panic!("only dot expressions are allowed!"),
    };

    let target = match &dot_expr.target {
        Some(target) => target,
        // TODO: how do we check this more carfully, we need to fill in the type hole first by
        // checking what the return value of the function is
        None => {
            let enum_id = node_tree.add_node(Node::EnumConstructor {
                target: None,
                variant: dot_expr.field,
                payload: Some(wrap_id),
            });
            return enum_id;
        }
    };

    let target = match target.deref() {
        Expr::Identifier(expr) => expr,
        // if we have a non-identifier target this isn't an enum variant
        _ => panic!("dot expression target must be an identifier"),
    };

    let scope_pos = module
        .get_scope_pos(target.token.source_id)
        .expect("could not find scope for identifier");

    let binding = module
        .find_binding(scope_pos, target.name)
        .expect("unknown identifier used in dot expression target");

    // if the identifier is a declared enum type we know this is a valid enum expression
    if binding.binding_type != BindingType::EnumType {
        panic!("can only call wrap with enum types");
    }

    node_tree.add_node(Node::EnumConstructor {
        target: Some(target.name),
        variant: dot_expr.field,
        payload: Some(wrap_id),
    })
}

fn node_expr(node_tree: &mut NodeTree, module: &Module, expr: &Expr) -> NodeID {
    match expr {
        Expr::IntLiteral(expr) => {
            let node_id = node_tree.add_node(Node::IntLiteral(*expr));

            // need to infer this type instead of just assuming it's an i64
            node_tree.type_map.add(node_id, TypeSpec::Int64);

            node_id
        }
        Expr::FloatLiteral(expr) => {
            let node_id = node_tree.add_node(Node::FloatLiteral(*expr));

            // need to infer this type instead of just assuming it's an f64
            node_tree.type_map.add(node_id, TypeSpec::Float64);

            node_id
        }
        Expr::StringLiteral(expr) => {
            let node_id = node_tree.add_node(Node::StringLiteral(*expr));
            node_tree.type_map.add(node_id, TypeSpec::String);
            node_id
        }
        Expr::BoolLiteral(expr) => {
            let node_id = node_tree.add_node(Node::BoolLiteral(*expr));
            node_tree.type_map.add(node_id, TypeSpec::Bool);
            node_id
        }
        Expr::Identifier(expr) => {
            let scope_pos = module
                .get_scope_pos(expr.token.source_id)
                .expect("could not get scope for identifier");

            let binding = module
                .find_binding(scope_pos, expr.name)
                .expect("failed to find binding for identifier expression");

            let ident_id = node_tree
                .symbol_map
                .get(binding.id)
                .expect("unknown identifier in expression check");

            *ident_id
        }
        Expr::Binary(expr) => {
            let left_id = node_expr(node_tree, module, &expr.left);
            let right_id = node_expr(node_tree, module, &expr.right);
            node_tree.add_node(Node::Binary {
                left: left_id,
                operator: expr.operator,
                right: right_id,
            })
        }
        Expr::Unary(expr) => {
            let expr_id = node_expr(node_tree, module, &expr.operand);
            node_tree.add_node(Node::Unary {
                operator: expr.operator,
                operand: expr_id,
            })
        }
        Expr::Call(expr) => {
            let func_id = node_expr(node_tree, module, &expr.func);

            let mut args = vec![];
            for arg in &expr.args {
                let param_id = node_expr(node_tree, module, arg);
                args.push(param_id);
            }

            // if the function was an enum constructor when we actually need to update the enum
            // to contain a payload and return the EnumConstructor itself rather than creating
            // and returning the ID for the function call.
            let mut func_node = node_tree.get_mut_node(func_id).unwrap();
            if let Node::EnumConstructor { payload: p, .. } = &mut func_node {
                if args.len() != 1 {
                    panic!("enum constructors can only contain a single paramater")
                }

                *p = Some(*args.first().unwrap());
                func_id
            } else {
                node_tree.add_node(Node::Call {
                    func: func_id,
                    args,
                })
            }
        }
        Expr::Index(expr) => {
            let target_id = node_expr(node_tree, module, &expr.target);
            let idx_id = node_expr(node_tree, module, &expr.index);

            node_tree.add_node(Node::Index {
                target: target_id,
                index: idx_id,
            })
        }
        Expr::Range(expr) => {
            let start_id = node_expr(node_tree, module, &expr.start);
            let end_id = node_expr(node_tree, module, &expr.end);
            node_tree.add_node(Node::Range {
                start: start_id,
                end: end_id,
            })
        }
        Expr::DotAccess(expr) => {
            let target = match &expr.target {
                Some(t) => t,
                None => {
                    // if there's no target it must be an enum (and if it's not, we'll fail type
                    // checking later)
                    return node_tree.add_node(Node::EnumConstructor {
                        target: None,
                        variant: expr.field,
                        payload: None,
                    });
                }
            };

            let target_id = node_expr(node_tree, module, target);

            let binding = match target.deref() {
                Expr::Identifier(ident) => {
                    let scope_pos = module
                        .get_scope_pos(ident.token.source_id)
                        .expect("could not find scope_posfor identifier");
                    module
                        .find_binding(scope_pos, ident.name)
                        .expect("missing binding for already noded target")
                }
                _ => {
                    // non-identifier targets can only be field access expressions
                    return node_tree.add_node(Node::FieldAccess {
                        target: Some(target_id),
                        field: expr.field,
                    });
                }
            };

            match binding.binding_type {
                BindingType::EnumType => node_tree.add_node(Node::EnumConstructor {
                    target: Some(target_id),
                    variant: expr.field,
                    payload: None,
                }),
                _ => {
                    // identifiers can still be field access expressions of the identifier is
                    // not an enum type
                    node_tree.add_node(Node::FieldAccess {
                        target: Some(target_id),
                        field: expr.field,
                    })
                }
            }
        }
        Expr::ModuleAccess(_expr) => todo!("modules are not yet supported"),
        Expr::MetaType(_expr) => {
            let node_id = node_tree.add_node(Node::MetaType);
            node_tree.type_map.add(
                node_id,
                // TODO: the meta type should probably live somewhere central since I don't
                // think this is the only place this will pop up.
                TypeSpec::Struct(StructType {
                    fields: vec![
                        // TODO: These should probably be USize types if I add that...
                        StructField {
                            name: str_store::SIZEOF,
                            type_spec: TypeSpec::UInt64,
                        },
                        StructField {
                            name: str_store::ALIGNOF,
                            type_spec: TypeSpec::UInt64,
                        },
                        // this stores boolean information about the type like if it's a slice
                        // hash-map or string etc.
                        StructField {
                            name: str_store::METAFLAGS,
                            type_spec: TypeSpec::UInt64,
                        },
                    ],
                }),
            );

            node_id
        }
        Expr::Alloc(expr) => {
            let meta_id = node_expr(node_tree, module, &expr.meta_type);
            let mut options = vec![];
            for opt in &expr.options {
                let opt_id = node_expr(node_tree, module, opt);
                options.push(opt_id);
            }

            let node_id = node_tree.add_node(Node::Alloc {
                meta_type: meta_id,
                options,
            });

            node_tree.type_map.add(node_id, TypeSpec::UnsafePtr);

            node_id
        }
        Expr::Free(expr) => {
            let ptr_id = node_expr(node_tree, module, &expr.expr);
            node_tree.add_node(Node::Free { expr: ptr_id })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, ConstDecl, UnaryOp, VarDecl};
    use crate::parser::lexer::{Token, TokenKind};
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::path::Path;

    #[test]
    fn typemap_new_get_none() {
        let tm: SideTable<NodeID, TypeSpec> = SideTable::new();
        assert_eq!(tm.get(0), None);
        assert_eq!(tm.get(10), None);
    }

    #[test]
    fn typemap_add_and_get() {
        let mut tm: SideTable<NodeID, TypeSpec> = SideTable::new();
        tm.add(0, TypeSpec::Int32);
        assert_eq!(tm.get(0), Some(&TypeSpec::Int32));

        tm.add(1, TypeSpec::Bool);
        assert_eq!(tm.get(1), Some(&TypeSpec::Bool));
        assert_eq!(tm.get(0), Some(&TypeSpec::Int32));
    }

    #[test]
    fn typemap_sparse_indices() {
        let mut tm: SideTable<NodeID, TypeSpec> = SideTable::new();
        tm.add(3, TypeSpec::String);
        assert_eq!(tm.get(3), Some(&TypeSpec::String));
        assert_eq!(tm.get(0), None);
    }

    fn assert_file_path_eq(path: &std::path::Path, noder_dir: &Path) {
        let ext = path.extension().expect("Failed to get file extension");
        if ext != "manta" {
            return;
        }

        let file_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => panic!("Failed to read {}", path.display()),
        };

        let mut str_store = crate::str_store::StrStore::new();
        let parser = crate::parser::Parser::new(source);
        let module = parser.parse_module(&mut str_store);

        let node_tree = node_module(module);

        let json_output =
            serde_json::to_string_pretty(&node_tree).expect("Failed to serialize NodeTree to JSON");

        let noder_file = noder_dir.join(format!("{}.json", file_name));

        if noder_file.exists() {
            let expected_json = match fs::read_to_string(&noder_file) {
                Ok(s) => s,
                Err(_) => panic!("Failed to read {}", noder_file.display()),
            };

            assert_eq!(
                json_output, expected_json,
                "Noder output mismatch for {}",
                file_name
            );
        } else {
            fs::create_dir_all(noder_dir).expect("Failed to create noder test directory");

            match fs::write(&noder_file, &json_output) {
                Ok(_) => (),
                Err(_) => panic!("Failed to write noder output to {:?}", noder_file),
            };

            panic!(
                "Generated new noder output file: {:?}. Please verify its correctness.",
                noder_file
            );
        }
    }

    include!(concat!(env!("OUT_DIR"), "/generated_noder_tests.rs"));

    macro_rules! test_noder {
        ( $( $case:ident { decl: $decl:expr, expected: $expected:expr } ),* $(,)? ) => {
            $(
                #[test]
                fn $case() {
                    // Build module from provided declaration
                    let decl = $decl;
                    let module = Module::new(vec![], vec![decl]);

                    let node_tree = node_module(module);

                    // Evaluate the provided expression to obtain the expected NodeTree
                    let expected = $expected;

                    let actual_json = serde_json::to_string_pretty(&node_tree).unwrap();
                    let expected_json = serde_json::to_string_pretty(&expected).unwrap();
                    assert_eq!(actual_json, expected_json);
                }
            )*
        }
    }

    // Table-driven tests for noder: each case provides an AST snippet and an expression
    // that returns the expected NodeTree.
    test_noder!(
        node_const_decl_int_literal {
            decl: Decl::Const(ConstDecl {
                id: 0,
                name: 1,
                value: Expr::IntLiteral(42)
            }),
            expected: {
                let mut e = NodeTree::new();

                let ident_id = e.add_node(Node::Identifier(1));
                e.add_root_node(Node::VarDecl { ident: ident_id });

                let value_id = e.add_node(Node::IntLiteral(42));
                e.type_map.add(value_id, TypeSpec::Int64);
                e.add_root_node(Node::Assign {
                    target: ident_id,
                    value: value_id,
                });
                e.symbol_map.add(12, ident_id);

                e
            }
        },
        node_var_decl_string_literal {
            decl: Decl::Var(VarDecl {
                token: Token {
                    kind: TokenKind::Identifier,
                    source_id: 0,
                    lexeme_id: 0,
                },
                name: 2,
                value: Expr::StringLiteral(3)
            }),
            expected: {
                let mut e = NodeTree::new();

                let ident_id = e.add_node(Node::Identifier(2));
                e.add_root_node(Node::VarDecl { ident: ident_id });

                let value_id = e.add_node(Node::StringLiteral(3));
                e.type_map.add(value_id, TypeSpec::String);
                e.add_root_node(Node::Assign {
                    target: ident_id,
                    value: value_id,
                });
                e.symbol_map.add(12, ident_id);

                e
            }
        },
    );

    #[test]
    fn test_new_store_is_empty() {
        let store = NodeTree::new();
        assert_eq!(store.nodes.len(), 0);
        assert_eq!(store.roots.len(), 0);
    }

    #[test]
    fn test_add_node_returns_correct_id() {
        let mut store = NodeTree::new();
        let node = Node::Invalid;
        let id = store.add_node(node);
        assert_eq!(id, 0);
    }

    #[test]
    fn test_add_multiple_nodes() {
        let mut store = NodeTree::new();
        let id1 = store.add_node(Node::Invalid);
        let id2 = store.add_node(Node::BoolLiteral(true));
        let id3 = store.add_node(Node::IntLiteral(42));

        assert_eq!(id1, 0);
        assert_eq!(id2, 1);
        assert_eq!(id3, 2);
        assert_eq!(store.nodes.len(), 3);
    }

    #[test]
    fn test_add_node_does_not_add_to_roots() {
        let mut store = NodeTree::new();
        store.add_node(Node::Invalid);
        assert_eq!(store.roots.len(), 0);
    }

    #[test]
    fn test_add_root_node_returns_correct_id() {
        let mut store = NodeTree::new();
        let node = Node::Invalid;
        let id = store.add_root_node(node);
        assert_eq!(id, 0);
    }

    #[test]
    fn test_add_root_node_adds_to_roots() {
        let mut store = NodeTree::new();
        let id = store.add_root_node(Node::BoolLiteral(true));
        assert_eq!(store.roots.len(), 1);
        assert_eq!(store.roots[0], id);
    }

    #[test]
    fn test_add_multiple_root_nodes() {
        let mut store = NodeTree::new();
        let id1 = store.add_root_node(Node::IntLiteral(1));
        let id2 = store.add_root_node(Node::IntLiteral(2));
        let id3 = store.add_root_node(Node::IntLiteral(3));

        assert_eq!(store.roots.len(), 3);
        assert_eq!(store.roots[0], id1);
        assert_eq!(store.roots[1], id2);
        assert_eq!(store.roots[2], id3);
    }

    #[test]
    fn test_mix_nodes_and_root_nodes() {
        let mut store = NodeTree::new();
        let regular_id = store.add_node(Node::Invalid);
        let root_id = store.add_root_node(Node::BoolLiteral(true));
        let another_regular = store.add_node(Node::IntLiteral(42));

        assert_eq!(store.nodes.len(), 3);
        assert_eq!(store.roots.len(), 1);
        assert_eq!(store.roots[0], root_id);
        assert_ne!(regular_id, root_id);
        assert_ne!(another_regular, root_id);
    }

    #[test]
    fn test_add_int_literal() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::IntLiteral(100));
        assert_eq!(store.nodes[id], Node::IntLiteral(100));
    }

    #[test]
    fn test_add_float_literal() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::FloatLiteral(3.45));
        assert_eq!(store.nodes[id], Node::FloatLiteral(3.45));
    }

    #[test]
    fn test_add_string_literal() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::StringLiteral(0));
        assert_eq!(store.nodes[id], Node::StringLiteral(0));
    }

    #[test]
    fn test_add_bool_literal() {
        let mut store = NodeTree::new();
        let id_true = store.add_node(Node::BoolLiteral(true));
        let id_false = store.add_node(Node::BoolLiteral(false));

        assert_eq!(store.nodes[id_true], Node::BoolLiteral(true));
        assert_eq!(store.nodes[id_false], Node::BoolLiteral(false));
    }

    #[test]
    fn test_add_identifier() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Identifier(10));
        assert_eq!(store.nodes[id], Node::Identifier(10));
    }

    #[test]
    fn test_add_block_node() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Block {
            statements: vec![0, 1, 2],
        });
        assert_eq!(
            store.nodes[id],
            Node::Block {
                statements: vec![0, 1, 2],
            }
        );
    }

    #[test]
    fn test_add_binary_operation() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Binary {
            left: 0,
            operator: BinaryOp::Add,
            right: 1,
        });
        assert_eq!(
            store.nodes[id],
            Node::Binary {
                left: 0,
                operator: BinaryOp::Add,
                right: 1,
            }
        );
    }

    #[test]
    fn test_add_unary_operation() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Unary {
            operator: UnaryOp::Negate,
            operand: 0,
        });
        assert_eq!(
            store.nodes[id],
            Node::Unary {
                operator: UnaryOp::Negate,
                operand: 0,
            }
        );
    }

    #[test]
    fn test_add_function_declaration() {
        let mut store = NodeTree::new();
        let ident_id = store.add_node(Node::Identifier(20));
        let id = store.add_node(Node::FunctionDecl {
            ident: ident_id,
            params: vec![],
            body: 0,
        });

        if let Node::FunctionDecl { ident, .. } = &store.nodes[id] {
            assert_eq!(*ident, 0);
        } else {
            panic!("Expected FunctionDecl");
        }
    }

    #[test]
    fn test_sequential_ids_are_unique() {
        let mut store = NodeTree::new();
        let mut ids = Vec::new();
        for i in 0..10 {
            let id = store.add_node(Node::IntLiteral(i as i64));
            ids.push(id);
        }

        // All IDs should be unique
        for i in 0..ids.len() {
            for j in i + 1..ids.len() {
                assert_ne!(ids[i], ids[j]);
            }
        }
    }

    #[test]
    fn test_add_many_root_nodes() {
        let mut store = NodeTree::new();
        for i in 0..20 {
            store.add_root_node(Node::IntLiteral(i as i64));
        }
        assert_eq!(store.roots.len(), 20);
        assert_eq!(store.nodes.len(), 20);
    }
}

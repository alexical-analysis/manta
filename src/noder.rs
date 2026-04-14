pub mod typer;

use serde::Serialize;
use std::cmp::Ord;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::ops::Deref;

use crate::ast::{
    self, BlockStmt, Decl, Expr, LetExcept, LetStmt, Pattern, Payload, ReturnStmt, Stmt,
};
use crate::hir::{
    ArrayType, DefaultPat, EnumType, EnumVariant, EnumVariantPat, FunctionType, NamedType, Node,
    NodeID, PatternNode, StructField, StructType, TypeSpec, TypeSpecPat,
};
use crate::noder::typer::Typer;
use crate::parser::module::{BindingType, Module, SymID};
use crate::str_store;

#[derive(Serialize)]
pub(crate) struct SideTable<K, V> {
    pub(crate) keys: BTreeMap<K, usize>,
    pub(crate) values: Vec<V>,
}

impl<K: Ord + Debug, V: Debug> SideTable<K, V> {
    fn new() -> Self {
        SideTable {
            keys: BTreeMap::new(),
            values: vec![],
        }
    }

    fn add(&mut self, key: K, value: V) {
        if self.keys.contains_key(&key) {
            panic!("can not add the same key twice")
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

    fn set(&mut self, key: K, value: V) {
        match self.keys.get(&key) {
            Some(k) => match self.values.get_mut(*k) {
                Some(v) => *v = value,
                None => panic!("missing value for key {:?}", key),
            },
            None => panic!("setting value {:?} for unknown key {:?}", value, key),
        }
    }
}

/// NodeTree contains all the nodes for a given tree as well as tracking the tree roots
#[derive(Serialize)]
pub struct NodeTree {
    pub(crate) nodes: Vec<Node>,
    pub roots: Vec<NodeID>,
    // the type_map maps each node in the node tree to it's type (if it has one)
    // TODO: we're going to have a type for every node so maybe we can use a slot map or something
    // to pack these more tightly withouth a lookup/ performance cost. right now the side
    // table uses an internal b tree map.
    pub(crate) type_map: SideTable<NodeID, TypeSpec>,
    // the symbol_map maps a bindings symbol id, which is related to it's declaratoin site, to the
    // node_id where it was declared, using this you can look up a symbols declaration site in the
    // node tree through the symbol table
    pub(crate) symbol_map: SideTable<SymID, NodeID>,
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
        let id = self.nodes.len() - 1;
        NodeID::from_usize(id)
    }

    /// Adds a root node to the store and returns its unique NodeID
    pub fn add_root_node(&mut self, node: Node) -> NodeID {
        self.nodes.push(node);
        let id = self.nodes.len() - 1;

        let node_id = NodeID::from_usize(id);
        self.roots.push(node_id);

        node_id
    }

    pub fn get_node(&self, node_id: NodeID) -> Option<&Node> {
        self.nodes.get(node_id.to_usize())
    }

    pub fn get_mut_node(&mut self, node_id: NodeID) -> Option<&mut Node> {
        self.nodes.get_mut(node_id.to_usize())
    }

    pub fn get_type(&self, node_id: NodeID) -> Option<&TypeSpec> {
        self.type_map.get(node_id)
    }
}

pub fn node_module(module: Module) -> NodeTree {
    // Should these types be owned by the Noder type?
    let mut node_tree = NodeTree::new();

    for decl in module.get_decls() {
        node_decl(&mut node_tree, &module, decl);
    }

    let mut typer = Typer::new();
    // TODO: gather errors here instead of just panicing
    typer.type_node_tree(&mut node_tree);

    node_tree
}

fn node_type_spec(node_tree: &NodeTree, module: &Module, type_spec: &ast::TypeSpec) -> TypeSpec {
    match type_spec {
        ast::TypeSpec::Int8 => TypeSpec::Int8,
        ast::TypeSpec::Int16 => TypeSpec::Int16,
        ast::TypeSpec::Int32 => TypeSpec::Int32,
        ast::TypeSpec::Int64 => TypeSpec::Int64,
        ast::TypeSpec::UInt8 => TypeSpec::UInt8,
        ast::TypeSpec::UInt16 => TypeSpec::UInt16,
        ast::TypeSpec::UInt32 => TypeSpec::UInt32,
        ast::TypeSpec::UInt64 => TypeSpec::UInt64,
        ast::TypeSpec::Float32 => TypeSpec::Float32,
        ast::TypeSpec::Float64 => TypeSpec::Float64,
        ast::TypeSpec::String => TypeSpec::String,
        ast::TypeSpec::Bool => TypeSpec::Bool,
        ast::TypeSpec::Unit => TypeSpec::Unit,
        ast::TypeSpec::Named(t) => {
            if t.module.is_some() {
                panic!("modules are not yet supported")
            }

            // look up the type using it's scope_position and name and extract the underlying type
            let scope_pos = module
                .get_scope_pos(t.id)
                .expect("failed to find type identifier scope position");
            let binding = module
                .find_binding(scope_pos, t.name)
                .expect("missing binding for type identifier");
            let type_spec = match &binding.binding_type {
                BindingType::TypeDecl(t) => t,
                _ => panic!("this binding was not for a type"),
            };
            let name_id = node_tree
                .symbol_map
                .get(binding.id)
                .expect("failed to find declaration name for named type spec");

            let type_spec = node_type_spec(node_tree, module, type_spec);
            let type_spec = Box::new(type_spec);
            TypeSpec::Named(NamedType {
                name: *name_id,
                type_spec,
            })
        }
        ast::TypeSpec::Pointer(t) => {
            let inner = node_type_spec(node_tree, module, t);
            TypeSpec::Pointer(Box::new(inner))
        }
        ast::TypeSpec::Slice(t) => {
            let inner = node_type_spec(node_tree, module, t);
            TypeSpec::Slice(Box::new(inner))
        }
        ast::TypeSpec::Array(t) => {
            let inner = node_type_spec(node_tree, module, &t.type_spec);
            TypeSpec::Array(ArrayType {
                size: t.size,
                type_spec: Box::new(inner),
            })
        }
        ast::TypeSpec::Struct(t) => {
            let mut fields = vec![];
            for field in &t.fields {
                let inner = node_type_spec(node_tree, module, &field.type_spec);
                fields.push(StructField {
                    name: field.name,
                    type_spec: inner,
                })
            }
            TypeSpec::Struct(StructType { fields })
        }
        ast::TypeSpec::Enum(t) => {
            let mut variants: Vec<EnumVariant> = vec![];
            for variant in &t.variants {
                let inner = variant
                    .payload
                    .as_ref()
                    .map(|t| node_type_spec(node_tree, module, t));
                variants.push(EnumVariant {
                    name: variant.name,
                    payload: inner,
                })
            }
            TypeSpec::Enum(EnumType { variants })
        }
        ast::TypeSpec::Function(t) => {
            let return_type = node_type_spec(node_tree, module, &t.return_type);
            let return_type = Box::new(return_type);

            let mut params = vec![];
            for param in &t.params {
                let inner = node_type_spec(node_tree, module, param);
                params.push(inner);
            }

            TypeSpec::Function(FunctionType {
                params,
                return_type,
            })
        }
    }
}

fn node_decl(node_tree: &mut NodeTree, module: &Module, decl: &Decl) {
    match decl {
        Decl::Function(decl) => {
            let mut params = vec![];
            for i in 0..decl.params.len() {
                let param = &decl.params[i];
                let param_type = &decl.function_type.params[i];

                let ident_id = node_tree.add_node(Node::Identifier {
                    module: None,
                    name: param.name,
                });
                let param_id = node_tree.add_node(Node::VarDecl { ident: ident_id });
                params.push(param_id);

                let type_spec = node_type_spec(node_tree, module, param_type);
                node_tree.type_map.add(ident_id, type_spec);

                let scope_pos = module
                    .get_scope_pos(param.id)
                    .expect("missing scope position for function paramater");
                let binding = module
                    .find_binding(scope_pos, param.name)
                    .expect("missing binding for function parameter");

                node_tree.symbol_map.add(binding.id, ident_id);
            }

            let return_type = node_type_spec(node_tree, module, &decl.function_type.return_type);

            // need to make sure the identifier is declared before we node the fn body so recursive
            // functions can find themselves
            let ident_id = node_tree.add_node(Node::Identifier {
                name: decl.name,
                module: None,
            });
            let scope_pos = module
                .get_scope_pos(decl.id)
                .expect("missing scope_pos for function");
            let binding = module
                .find_binding(scope_pos, decl.name)
                .expect("missing binding for function");
            node_tree.symbol_map.add(binding.id, ident_id);

            let body_id = node_fn_body(node_tree, module, &decl.body, return_type);
            let func_id = node_tree.add_root_node(Node::FunctionDecl {
                ident: ident_id,
                params,
                body: body_id,
            });

            let func_type = ast::TypeSpec::Function(decl.function_type.clone());
            let func_type = node_type_spec(node_tree, module, &func_type);

            node_tree.type_map.add(ident_id, func_type.clone());
            node_tree.type_map.add(func_id, func_type);
        }
        Decl::Type(decl) => {
            let ident_id = node_tree.add_node(Node::Identifier {
                name: decl.name,
                module: None,
            });
            let decl_id = node_tree.add_root_node(Node::TypeDecl { ident: ident_id });
            let type_spec = node_type_spec(node_tree, module, &decl.type_spec);
            node_tree.type_map.add(decl_id, type_spec);

            let scope_pos = module
                .get_scope_pos(decl.id)
                .expect("missing scope_posfor type decl");
            let binding = module
                .find_binding(scope_pos, decl.name)
                .expect("missing binding for type decl");

            node_tree.symbol_map.add(binding.id, ident_id);
        }
        Decl::Const(decl) => {
            let ident_id = node_tree.add_node(Node::Identifier {
                module: None,
                name: decl.name,
            });
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
            let ident_id = node_tree.add_node(Node::Identifier {
                name: decl.name,
                module: None,
            });
            node_tree.add_root_node(Node::VarDecl { ident: ident_id });

            let scope_pos = module
                .get_scope_pos(decl.id)
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

fn node_fn_body(
    node_tree: &mut NodeTree,
    module: &Module,
    block: &BlockStmt,
    return_type: TypeSpec,
) -> NodeID {
    let mut stmt_ids = vec![];
    for stmt in &block.statements {
        let ids = node_stmt(node_tree, module, stmt);
        stmt_ids.extend(ids);
    }

    // we allow the final return in a function to be omitted if the return type is the
    // unit type we need to ensure we add that return in here though so that we're ready
    // for the MIR construction
    if return_type == TypeSpec::Unit {
        match block.statements.last() {
            Some(Stmt::Return(_)) => { /* we're good, the final statement is a return */ }
            Some(_) | None => {
                let ret_stmt = &Stmt::Return(ReturnStmt { value: None });
                let return_id = node_stmt(node_tree, module, ret_stmt);
                stmt_ids.push(*return_id.first().expect("failed to node statement"))
            }
        }
    }

    node_tree.add_node(Node::Block {
        statements: stmt_ids,
    })
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

            // TODO: should I check that l_id is an assignable node here or should that
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
        Pattern::TypeSpec(pat) => {
            let mut payload = None;
            if let Payload::Some(pay) = pat.payload {
                let payload_id = node_tree.add_node(Node::Identifier {
                    name: pay,
                    module: None,
                });

                let scope_pos = module
                    .get_scope_pos(pat.id)
                    .expect("missing scope_posfor var decl");
                let binding = module
                    .find_binding(scope_pos, pay)
                    .expect("missing binding for var decl");

                node_tree.symbol_map.add(binding.id, payload_id);
                payload = Some(payload_id)
            }

            let node_id = node_tree.add_node(Node::Pattern(PatternNode::TypeSpec(TypeSpecPat {
                payload,
            })));

            // we add the type spec during the noding phase for type specs because otherwise we would
            // loose the type information. The hir tree only tracks type information in
            // the type_map, not on the individual nodes like the ast.
            let type_spec = node_type_spec(node_tree, module, &pat.type_spec);
            node_tree.type_map.add(node_id, type_spec);

            node_id
        }
        Pattern::EnumVariant(pat) => {
            let mut enum_name = None;
            if let Some(ident) = &pat.enum_name {
                let scope_pos = module
                    .get_scope_pos(pat.id)
                    .expect("missing scope position for enum variant");
                let binding = module
                    .find_binding(scope_pos, ident.name)
                    .expect("can not find binding for enum variant");
                let ident_node = node_tree
                    .symbol_map
                    .get(binding.id)
                    .expect("failed to find declaration node for the enum variant");

                enum_name = Some(*ident_node);
            }

            let mut payload = None;
            if let Payload::Some(pay) = pat.payload {
                let payload_ident = node_tree.add_node(Node::Identifier {
                    name: pay,
                    module: None,
                });

                let scope_pos = module
                    .get_scope_pos(pat.id)
                    .expect("missing scope_posfor var enum variant pattern payload");
                let binding = module
                    .find_binding(scope_pos, pay)
                    .expect("missing binding for enum variant pattern payload");

                node_tree.symbol_map.add(binding.id, payload_ident);
                payload = Some(payload_ident)
            }

            node_tree.add_node(Node::Pattern(PatternNode::EnumVariant(EnumVariantPat {
                enum_name,
                variant: pat.variant,
                payload,
            })))
        }
        Pattern::Identifier(pat) => {
            // Identifiers are turned into Default patterns with the identifier as the payload
            let payload_ident = node_tree.add_node(Node::Identifier {
                name: pat.name,
                module: None,
            });

            let scope_pos = module
                .get_scope_pos(pat.id)
                .expect("missing scope_posfor var enum variant pattern payload");
            let binding = module
                .find_binding(scope_pos, pat.name)
                .expect("missing binding for enum variant pattern payload");

            node_tree.symbol_map.add(binding.id, payload_ident);

            let payload = Some(payload_ident);
            node_tree.add_node(Node::Pattern(PatternNode::Default(DefaultPat { payload })))
        }
        Pattern::ModuleIdentifier(_) => {
            panic!("no module identifier patterns should exist at this point in the hir")
        }
        Pattern::Default => node_tree.add_node(Node::Pattern(PatternNode::Default(DefaultPat {
            payload: None,
        }))),
    }
}

fn node_let(node_tree: &mut NodeTree, module: &Module, stmt: &LetStmt) -> Vec<NodeID> {
    let mut nodes = vec![];
    let mut arms = vec![];
    let value_id = node_expr(node_tree, module, &stmt.value);

    match &stmt.pattern {
        Pattern::EnumVariant(pat) => {
            if stmt.except == LetExcept::None {
                panic!("missing exception handler for this pattern")
            }

            if let Payload::Some(pay) = pat.payload {
                // let .ident(p) = value
                //
                // decl outer_p
                // match value {
                //   .ident(inner_p) { outer_p = inner_p }
                //   ...
                // }

                let pattern = node_pattern(node_tree, module, &stmt.pattern);

                // lookup the payload node id
                let scope_pos = module
                    .get_scope_pos(pat.id)
                    .expect("missing scope_posfor var enum variant pattern payload");
                let binding = module
                    .find_binding(scope_pos, pay)
                    .expect("missing binding for enum variant pattern payload");
                let payload_id = *node_tree
                    .symbol_map
                    .get(binding.id)
                    .expect("missing payload id for enum pattern");

                let outer_ident = node_tree.add_node(Node::Identifier {
                    name: pay,
                    module: None,
                });
                nodes.push(node_tree.add_node(Node::VarDecl { ident: outer_ident }));

                let assign_id = node_tree.add_node(Node::Assign {
                    target: outer_ident,
                    value: payload_id,
                });
                let match_body = node_tree.add_node(Node::Block {
                    statements: vec![assign_id],
                });

                let scope_pos = module
                    .get_scope_pos(pat.id)
                    .expect("missing scope_posfor var enum variant pattern payload");
                let binding = module
                    .find_binding(scope_pos, pay)
                    .expect("missing binding for enum variant pattern payload");

                // Now that we've wired up the assignment we need to update the symbol_map to point
                // to our outer_ident node instead of the inner_ident node
                node_tree.symbol_map.set(binding.id, outer_ident);

                let arm = node_tree.add_node(Node::MatchArm {
                    pattern,
                    body: match_body,
                });
                arms.push(arm);
            } else {
                // let .ident = value
                //
                // match value {
                //   .ident { }
                //   ...
                // }

                let pattern = node_pattern(node_tree, module, &stmt.pattern);
                let empty_body = node_tree.add_node(Node::Block { statements: vec![] });
                let arm = node_tree.add_node(Node::MatchArm {
                    pattern,
                    body: empty_body,
                });
                arms.push(arm);
            }
        }
        Pattern::Identifier(ident) => {
            // let ident = value
            //
            // decl ident
            // ident = value

            if stmt.except != LetExcept::None {
                panic!(
                    "identifier expressions can never faile and should not have an except handle"
                )
            }

            let ident_id = node_tree.add_node(Node::Identifier {
                name: ident.name,
                module: None,
            });
            nodes.push(node_tree.add_node(Node::VarDecl { ident: ident_id }));

            let scope_pos = module
                .get_scope_pos(ident.id)
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
        Pattern::TypeSpec(pat) => {
            if stmt.except == LetExcept::None {
                panic!("missing exception handler for this pattern")
            }

            match pat.payload {
                Payload::Some(pay) => {
                    // let type(ident) = value
                    //
                    // decl outer_ident
                    // match value {
                    //   type(inner_ident) => { outer_ident = inner_ident }
                    //   ...
                    // }

                    let pattern = node_pattern(node_tree, module, &stmt.pattern);

                    // lookup the payload node id
                    let scope_pos = module
                        .get_scope_pos(pat.id)
                        .expect("missing scope_posfor var enum variant pattern payload");
                    let binding = module
                        .find_binding(scope_pos, pay)
                        .expect("missing binding for enum variant pattern payload");
                    let payload_id = *node_tree
                        .symbol_map
                        .get(binding.id)
                        .expect("missing payload id for enum pattern");

                    let outer_ident = node_tree.add_node(Node::Identifier {
                        name: pay,
                        module: None,
                    });
                    nodes.push(node_tree.add_node(Node::VarDecl { ident: outer_ident }));

                    let assign_id = node_tree.add_node(Node::Assign {
                        target: outer_ident,
                        value: payload_id,
                    });
                    let match_body = node_tree.add_node(Node::Block {
                        statements: vec![assign_id],
                    });

                    let scope_pos = module
                        .get_scope_pos(pat.id)
                        .expect("missing scope_posfor var enum variant pattern payload");
                    let binding = module
                        .find_binding(scope_pos, pay)
                        .expect("missing binding for enum variant pattern payload");

                    // Now that we've wired up the assignment we need to update the symbol_map to point
                    // to our outer_ident node instead of the inner_ident node
                    node_tree.symbol_map.set(binding.id, outer_ident);

                    let arm = node_tree.add_node(Node::MatchArm {
                        pattern,
                        body: match_body,
                    });
                    arms.push(arm);
                }
                Payload::Default => {
                    // let type(_) = value
                    //
                    // match value {
                    //   type(_) => {}
                    //   ...
                    // }

                    let pattern = node_pattern(node_tree, module, &stmt.pattern);
                    let empty_body = node_tree.add_node(Node::Block { statements: vec![] });
                    let arm = node_tree.add_node(Node::MatchArm {
                        pattern,
                        body: empty_body,
                    });
                    arms.push(arm);
                }
                Payload::None => panic!("type spec patterns must have a payload to be valid"),
            }
        }
        _ => {
            //
            // let Pat = value
            //
            // becomes:
            //
            // match {
            //     Pat { }
            //     ...
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
        LetExcept::Or { id, binding, body } => {
            match *binding {
                Some(e) => {
                    // or(e) { ... }
                    //
                    // becomes:
                    //
                    // e { ... }
                    let ident_id = node_tree.add_node(Node::Identifier {
                        name: e,
                        module: None,
                    });
                    let pat_id =
                        node_tree.add_node(Node::Pattern(PatternNode::Default(DefaultPat {
                            payload: Some(ident_id),
                        })));

                    let scope_pos = module
                        .get_scope_pos(*id)
                        .expect("missing scope position for or binding");
                    let binding = module
                        .find_binding(scope_pos, e)
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
                    let pat_id =
                        node_tree.add_node(Node::Pattern(PatternNode::Default(DefaultPat {
                            payload: None,
                        })));

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
            let wrap_id = node_tree.add_node(Node::Identifier {
                name: str_store::WRAP,
                module: None,
            });
            let enum_id = node_wrap_expr(node_tree, module, expr, wrap_id);

            let ret_id = node_tree.add_node(Node::Return {
                value: Some(enum_id),
            });
            let body_id = node_tree.add_node(Node::Block {
                statements: vec![ret_id],
            });

            let pat_id = node_tree.add_node(Node::Pattern(PatternNode::Default(DefaultPat {
                payload: Some(wrap_id),
            })));

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
            let panic_fn_id = node_tree.add_node(Node::Identifier {
                name: str_store::PANIC,
                module: None,
            });

            node_tree.type_map.add(
                panic_fn_id,
                TypeSpec::Function(FunctionType {
                    params: vec![TypeSpec::String],
                    return_type: Box::new(TypeSpec::Panic),
                }),
            );

            // the identifier for the pattern, this needs to be seperate from the function call
            // identifier for type checking to work correctly since they need to have different
            // underlying types
            let panic_ident_id = node_tree.add_node(Node::Identifier {
                name: str_store::PANIC,
                module: None,
            });

            let panic_str_id = node_tree.add_node(Node::StringLiteral(str_store::UNDERSCORE));

            let call_id = node_tree.add_node(Node::Call {
                func: panic_fn_id,
                // TODO: eventually we need to support more flexible panics but for now just use a
                // constant string
                args: vec![panic_str_id],
            });
            let body_id = node_tree.add_node(Node::Block {
                statements: vec![call_id],
            });

            let pat_id = node_tree.add_node(Node::Pattern(PatternNode::Default(DefaultPat {
                payload: Some(panic_ident_id),
            })));
            node_tree.add_node(Node::MatchArm {
                pattern: pat_id,
                body: body_id,
            })
        }
        LetExcept::None => {
            // TODO: we need to check that this expression can not fail to match
            // for now just panic if we hit this arm somehow

            // the identifier for the function call
            let panic_fn_id = node_tree.add_node(Node::Identifier {
                name: str_store::PANIC,
                module: None,
            });

            node_tree.type_map.add(
                panic_fn_id,
                TypeSpec::Function(FunctionType {
                    // TODO: this needs to be the type of the expression that's being matched or an
                    // any type like in Go, just pick a random type for now
                    params: vec![TypeSpec::String],
                    return_type: Box::new(TypeSpec::Panic),
                }),
            );

            // the identifier for the pattern, this needs to be seperate from the function call
            // identifier for type checking to work correctly since they need to have different
            // underlying types
            let panic_ident_id = node_tree.add_node(Node::Identifier {
                name: str_store::PANIC,
                module: None,
            });

            let call_id = node_tree.add_node(Node::Call {
                func: panic_fn_id,
                args: vec![panic_ident_id],
            });
            let body_id = node_tree.add_node(Node::Block {
                statements: vec![call_id],
            });

            let pat_id = node_tree.add_node(Node::Pattern(PatternNode::Default(DefaultPat {
                payload: Some(panic_ident_id),
            })));
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
            return node_tree.add_node(Node::EnumConstructor {
                target: None,
                variant: dot_expr.field,
                payload: Some(wrap_id),
            });
        }
    };

    let target = match target.deref() {
        Expr::Identifier(expr) => expr,
        // if we have a non-identifier target this isn't an enum variant
        _ => panic!("dot expression target must be an identifier"),
    };

    let scope_pos = module
        .get_scope_pos(target.id)
        .expect("could not find scope for identifier");

    let binding = module
        .find_binding(scope_pos, target.name)
        .expect("unknown identifier used in dot expression target");

    // if the identifier is a declared enum type we know this is a valid enum expression
    if !matches!(
        binding.binding_type,
        BindingType::TypeDecl(ast::TypeSpec::Enum(_))
    ) {
        panic!("can only call wrap with enum types")
    }

    let target_node = node_tree.symbol_map.get(binding.id).cloned();

    let enum_constructor_id = node_tree.add_node(Node::EnumConstructor {
        target: target_node,
        variant: dot_expr.field,
        payload: Some(wrap_id),
    });

    // Look up the enum type from the symbol map
    if let Some(type_decl_id) = node_tree.symbol_map.get(binding.id)
        && let Some(enum_type) = node_tree.type_map.get(*type_decl_id)
    {
        node_tree
            .type_map
            .add(enum_constructor_id, enum_type.clone());
    }

    enum_constructor_id
}

fn node_expr(node_tree: &mut NodeTree, module: &Module, expr: &Expr) -> NodeID {
    match expr {
        Expr::IntLiteral(expr) => node_tree.add_node(Node::IntLiteral(*expr)),
        Expr::FloatLiteral(expr) => node_tree.add_node(Node::FloatLiteral(*expr)),
        Expr::StringLiteral(expr) => node_tree.add_node(Node::StringLiteral(*expr)),
        Expr::BoolLiteral(expr) => node_tree.add_node(Node::BoolLiteral(*expr)),
        Expr::Identifier(expr) => {
            let scope_pos = module
                .get_scope_pos(expr.id)
                .expect("could not get scope for identifier");

            let binding = module
                .find_binding(scope_pos, expr.name)
                .expect("failed to find binding for identifier expression");

            let ident_id = node_tree
                .symbol_map
                .get(binding.id)
                .expect("identifier declaration missing from symbol map");

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

                let first_arg = args.first().unwrap();
                *p = Some(*first_arg);
                func_id
            } else {
                node_tree.add_node(Node::Call {
                    func: func_id,
                    args,
                })
            }
        }
        Expr::StructConstructor(_) => todo!("struct constructors are not supported yet"),
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
                        .get_scope_pos(ident.id)
                        .expect("could not find scope_posfor identifier");
                    module
                        .find_binding(scope_pos, ident.name)
                        .expect("missing binding for already noded target")
                }
                _ => {
                    // non-identifier targets can only be field access expressions
                    return node_tree.add_node(Node::FieldAccess {
                        target: target_id,
                        field: expr.field,
                    });
                }
            };

            match binding.binding_type {
                BindingType::TypeDecl(ast::TypeSpec::Enum(_)) => {
                    let enum_constructor_id = node_tree.add_node(Node::EnumConstructor {
                        target: Some(target_id),
                        variant: expr.field,
                        payload: None,
                    });
                    // Look up the enum type from the symbol map
                    if let Some(type_decl_id) = node_tree.symbol_map.get(binding.id)
                        && let Some(enum_type) = node_tree.type_map.get(*type_decl_id)
                    {
                        node_tree
                            .type_map
                            .add(enum_constructor_id, enum_type.clone());
                    }
                    enum_constructor_id
                }
                _ => {
                    // identifiers can still be field access expressions of the identifier is
                    // not an enum type
                    node_tree.add_node(Node::FieldAccess {
                        target: target_id,
                        field: expr.field,
                    })
                }
            }
        }
        Expr::MetaType(expr) => {
            let node_id = node_tree.add_node(Node::MetaType);
            let ts = node_type_spec(node_tree, module, &expr.type_spec);
            node_tree.type_map.add(node_id, ts);

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
    use std::fs;
    use std::path::Path;

    use pretty_assertions::assert_eq;

    use crate::ast::{BinaryOp, ConstDecl, TypeDecl, UnaryOp, VarDecl};
    use crate::parser::Parser;
    use crate::parser::lexer::SourceID;
    use crate::str_store::{StrID, StrStore};

    #[test]
    fn typemap_new_get_none() {
        let tm: SideTable<NodeID, TypeSpec> = SideTable::new();
        assert_eq!(tm.get(NodeID::from_usize(0)), None);
        assert_eq!(tm.get(NodeID::from_usize(10)), None);
    }

    #[test]
    fn typemap_add_and_get() {
        let mut tm: SideTable<NodeID, TypeSpec> = SideTable::new();
        tm.add(NodeID::from_usize(0), TypeSpec::Int32);
        assert_eq!(tm.get(NodeID::from_usize(0)), Some(&TypeSpec::Int32));

        tm.add(NodeID::from_usize(1), TypeSpec::Bool);
        assert_eq!(tm.get(NodeID::from_usize(1)), Some(&TypeSpec::Bool));
        assert_eq!(tm.get(NodeID::from_usize(0)), Some(&TypeSpec::Int32));
    }

    #[test]
    fn typemap_sparse_indices() {
        let mut tm: SideTable<NodeID, TypeSpec> = SideTable::new();
        tm.add(NodeID::from_usize(3), TypeSpec::String);
        assert_eq!(tm.get(NodeID::from_usize(3)), Some(&TypeSpec::String));
        assert_eq!(tm.get(NodeID::from_usize(0)), None);
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

        let mut str_store = StrStore::new();
        let parser = Parser::new(source);
        let module = parser.parse_module(&mut str_store);

        let node_tree = node_module(module);

        let total = node_tree.nodes.len();
        let untyped: Vec<usize> = (0..total)
            .filter(|&i| node_tree.type_map.get(NodeID::from_usize(i)).is_none())
            .collect();
        if !untyped.is_empty() {
            let pct = (untyped.len() as f64 / total as f64) * 100.0;
            panic!(
                "{}: {}/{} nodes ({:.1}%) are missing types. Untyped node IDs: {:?}",
                file_name,
                untyped.len(),
                total,
                pct,
                untyped,
            );
        }

        let any_typed: Vec<usize> = (0..total)
            .filter(|&i| {
                matches!(
                    node_tree.type_map.get(NodeID::from_usize(i)),
                    Some(TypeSpec::Any)
                        | Some(TypeSpec::IntLiteral(_))
                        | Some(TypeSpec::FloatLiteral(_))
                        | Some(TypeSpec::InferredEnumExpr(_))
                        | Some(TypeSpec::InferredEnumPat(_))
                )
            })
            .collect();
        if !any_typed.is_empty() {
            panic!(
                "{}: {}/{} nodes have an unresolved type that still needs to be inferred. Node IDs: {:?}",
                file_name,
                any_typed.len(),
                total,
                any_typed,
            );
        }

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
        ( $( $case:ident { decl: $decl:expr, expected: $expected:expr } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    // Build module from provided declaration
                    let decl = $decl;
                    let module = Module::new(vec![], vec![decl]);

                    let node_tree = node_module(module);

                    let expected = $expected;

                    let actual_json = serde_json::to_string_pretty(&node_tree).unwrap();
                    let expected_json = serde_json::to_string_pretty(&expected).unwrap();
                    assert_eq!(actual_json, expected_json);
                }
            )*
        }
    }

    // Table-driven tests for noder: each case provides an AST declaration and a literal
    // NodeTree as the expected output.
    test_noder!(
        node_const_decl_int_literal {
            decl: Decl::Const(ConstDecl {
                id: SourceID::from_usize(0),
                name: StrID::from_usize(1),
                value: Expr::IntLiteral(42)
            }),
            expected: NodeTree {
                // NodeID(0): Identifier, NodeID(1): VarDecl (root), NodeID(2): IntLiteral, NodeID(3): Assign (root)
                nodes: vec![
                    Node::Identifier {
                        name: StrID::from_usize(1),
                        module: None
                    },
                    Node::VarDecl {
                        ident: NodeID::from_usize(0)
                    },
                    Node::IntLiteral(42),
                    Node::Assign {
                        target: NodeID::from_usize(0),
                        value: NodeID::from_usize(2)
                    },
                ],
                roots: vec![NodeID::from_usize(1), NodeID::from_usize(3)],
                type_map: SideTable {
                    // inserted: (NodeID(1), Unit), (NodeID(3), Unit), (NodeID(2), Int64), (NodeID(0), Int64)
                    keys: BTreeMap::from([
                        (NodeID::from_usize(0), 3),
                        (NodeID::from_usize(1), 0),
                        (NodeID::from_usize(2), 2),
                        (NodeID::from_usize(3), 1),
                    ]),
                    values: vec![
                        TypeSpec::Unit,
                        TypeSpec::Unit,
                        TypeSpec::Int64,
                        TypeSpec::Int64
                    ],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::from([(12_usize, 0)]),
                    values: vec![NodeID::from_usize(0)],
                },
            }
        },
        node_invalid_decl {
            decl: Decl::Invalid,
            expected: NodeTree {
                nodes: vec![Node::Invalid],
                roots: vec![NodeID::from_usize(0)],
                type_map: SideTable {
                    // typer assigns Panic to the Invalid node
                    keys: BTreeMap::from([(NodeID::from_usize(0), 0)]),
                    values: vec![TypeSpec::Panic],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::new(),
                    values: vec![]
                },
            }
        },
        node_const_decl_bool_literal {
            decl: Decl::Const(ConstDecl {
                id: SourceID::from_usize(0),
                name: StrID::from_usize(1),
                value: Expr::BoolLiteral(true)
            }),
            expected: NodeTree {
                // NodeID(0): Identifier, NodeID(1): VarDecl (root), NodeID(2): BoolLiteral, NodeID(3): Assign (root)
                nodes: vec![
                    Node::Identifier {
                        name: StrID::from_usize(1),
                        module: None
                    },
                    Node::VarDecl {
                        ident: NodeID::from_usize(0)
                    },
                    Node::BoolLiteral(true),
                    Node::Assign {
                        target: NodeID::from_usize(0),
                        value: NodeID::from_usize(2)
                    },
                ],
                roots: vec![NodeID::from_usize(1), NodeID::from_usize(3)],
                type_map: SideTable {
                    // inserted: (NodeID(1), Unit), (NodeID(3), Unit), (NodeID(2), Bool), (NodeID(0), Bool)
                    keys: BTreeMap::from([
                        (NodeID::from_usize(0), 3),
                        (NodeID::from_usize(1), 0),
                        (NodeID::from_usize(2), 2),
                        (NodeID::from_usize(3), 1),
                    ]),
                    values: vec![
                        TypeSpec::Unit,
                        TypeSpec::Unit,
                        TypeSpec::Bool,
                        TypeSpec::Bool
                    ],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::from([(12_usize, 0)]),
                    values: vec![NodeID::from_usize(0)],
                },
            }
        },
        node_const_decl_float_literal {
            decl: Decl::Const(ConstDecl {
                id: SourceID::from_usize(0),
                name: StrID::from_usize(1),
                value: Expr::FloatLiteral(3.45)
            }),
            expected: NodeTree {
                // NodeID(0): Identifier, NodeID(1): VarDecl (root), NodeID(2): FloatLiteral, NodeID(3): Assign (root)
                nodes: vec![
                    Node::Identifier {
                        name: StrID::from_usize(1),
                        module: None
                    },
                    Node::VarDecl {
                        ident: NodeID::from_usize(0)
                    },
                    Node::FloatLiteral(3.45),
                    Node::Assign {
                        target: NodeID::from_usize(0),
                        value: NodeID::from_usize(2)
                    },
                ],
                roots: vec![NodeID::from_usize(1), NodeID::from_usize(3)],
                type_map: SideTable {
                    // inserted: (NodeID(1), Unit), (NodeID(3), Unit), (NodeID(2), Float64), (NodeID(0), Float64)
                    keys: BTreeMap::from([
                        (NodeID::from_usize(0), 3),
                        (NodeID::from_usize(1), 0),
                        (NodeID::from_usize(2), 2),
                        (NodeID::from_usize(3), 1),
                    ]),
                    values: vec![
                        TypeSpec::Unit,
                        TypeSpec::Unit,
                        TypeSpec::Float64,
                        TypeSpec::Float64
                    ],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::from([(12_usize, 0)]),
                    values: vec![NodeID::from_usize(0)],
                },
            }
        },
        node_type_decl_int64 {
            decl: Decl::Type(TypeDecl {
                id: SourceID::from_usize(0),
                name: StrID::from_usize(1),
                type_spec: ast::TypeSpec::Int64,
            }),
            expected: NodeTree {
                // NodeID(0): Identifier, NodeID(1): TypeDecl (root)
                nodes: vec![
                    Node::Identifier {
                        name: StrID::from_usize(1),
                        module: None
                    },
                    Node::TypeDecl {
                        ident: NodeID::from_usize(0)
                    },
                ],
                roots: vec![NodeID::from_usize(1)],
                type_map: SideTable {
                    // inserted by noder: (NodeID(1), Int64)
                    // inserted by typer: (NodeID(0), Named { name: NodeID(0), type_spec: Int64 })
                    keys: BTreeMap::from([(NodeID::from_usize(0), 1), (NodeID::from_usize(1), 0),]),
                    values: vec![
                        TypeSpec::Int64,
                        TypeSpec::Named(NamedType {
                            name: NodeID::from_usize(0),
                            type_spec: Box::new(TypeSpec::Int64),
                        }),
                    ],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::from([(12_usize, 0)]),
                    values: vec![NodeID::from_usize(0)],
                },
            }
        },
        node_var_decl_string_literal {
            decl: Decl::Var(VarDecl {
                id: SourceID::from_usize(0),
                name: StrID::from_usize(2),
                value: Expr::StringLiteral(StrID::from_usize(3))
            }),
            expected: NodeTree {
                // NodeID(0): Identifier, NodeID(1): VarDecl (root), NodeID(2): StringLiteral, NodeID(3): Assign (root)
                nodes: vec![
                    Node::Identifier {
                        name: StrID::from_usize(2),
                        module: None
                    },
                    Node::VarDecl {
                        ident: NodeID::from_usize(0)
                    },
                    Node::StringLiteral(StrID::from_usize(3)),
                    Node::Assign {
                        target: NodeID::from_usize(0),
                        value: NodeID::from_usize(2)
                    },
                ],
                roots: vec![NodeID::from_usize(1), NodeID::from_usize(3)],
                type_map: SideTable {
                    // inserted: (NodeID(1), Unit), (NodeID(3), Unit), (NodeID(2), String), (NodeID(0), String)
                    keys: BTreeMap::from([
                        (NodeID::from_usize(0), 3),
                        (NodeID::from_usize(1), 0),
                        (NodeID::from_usize(2), 2),
                        (NodeID::from_usize(3), 1),
                    ]),
                    values: vec![
                        TypeSpec::Unit,
                        TypeSpec::Unit,
                        TypeSpec::String,
                        TypeSpec::String
                    ],
                },
                symbol_map: SideTable {
                    keys: BTreeMap::from([(12_usize, 0)]),
                    values: vec![NodeID::from_usize(0)],
                },
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
        assert_eq!(id, NodeID::from_usize(0));
    }

    #[test]
    fn test_add_multiple_nodes() {
        let mut store = NodeTree::new();
        let id1 = store.add_node(Node::Invalid);
        let id2 = store.add_node(Node::BoolLiteral(true));
        let id3 = store.add_node(Node::IntLiteral(42));

        assert_eq!(id1, NodeID::from_usize(0));
        assert_eq!(id2, NodeID::from_usize(1));
        assert_eq!(id3, NodeID::from_usize(2));
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
        assert_eq!(id, NodeID::from_usize(0));
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
        assert_eq!(store.nodes[id.to_usize()], Node::IntLiteral(100));
    }

    #[test]
    fn test_add_float_literal() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::FloatLiteral(3.45));
        assert_eq!(store.nodes[id.to_usize()], Node::FloatLiteral(3.45));
    }

    #[test]
    fn test_add_string_literal() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::StringLiteral(StrID::from_usize(0)));
        assert_eq!(
            store.nodes[id.to_usize()],
            Node::StringLiteral(StrID::from_usize(0))
        );
    }

    #[test]
    fn test_add_bool_literal() {
        let mut store = NodeTree::new();
        let id_true = store.add_node(Node::BoolLiteral(true));
        let id_false = store.add_node(Node::BoolLiteral(false));

        assert_eq!(store.nodes[id_true.to_usize()], Node::BoolLiteral(true));
        assert_eq!(store.nodes[id_false.to_usize()], Node::BoolLiteral(false));
    }

    #[test]
    fn test_add_identifier() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Identifier {
            name: StrID::from_usize(10),
            module: None,
        });
        assert_eq!(
            store.nodes[id.to_usize()],
            Node::Identifier {
                name: StrID::from_usize(10),
                module: None
            },
        );
    }

    #[test]
    fn test_add_block_node() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Block {
            statements: vec![
                NodeID::from_usize(0),
                NodeID::from_usize(1),
                NodeID::from_usize(2),
            ],
        });
        assert_eq!(
            store.nodes[id.to_usize()],
            Node::Block {
                statements: vec![
                    NodeID::from_usize(0),
                    NodeID::from_usize(1),
                    NodeID::from_usize(2),
                ],
            }
        );
    }

    #[test]
    fn test_add_binary_operation() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Binary {
            left: NodeID::from_usize(0),
            operator: BinaryOp::Add,
            right: NodeID::from_usize(1),
        });
        assert_eq!(
            store.nodes[id.to_usize()],
            Node::Binary {
                left: NodeID::from_usize(0),
                operator: BinaryOp::Add,
                right: NodeID::from_usize(1),
            }
        );
    }

    #[test]
    fn test_add_unary_operation() {
        let mut store = NodeTree::new();
        let id = store.add_node(Node::Unary {
            operator: UnaryOp::Negate,
            operand: NodeID::from_usize(0),
        });
        assert_eq!(
            store.nodes[id.to_usize()],
            Node::Unary {
                operator: UnaryOp::Negate,
                operand: NodeID::from_usize(0),
            }
        );
    }

    #[test]
    fn test_add_function_declaration() {
        let mut store = NodeTree::new();
        let ident_id = store.add_node(Node::Identifier {
            name: StrID::from_usize(20),
            module: None,
        });
        let id = store.add_node(Node::FunctionDecl {
            ident: ident_id,
            params: vec![],
            body: NodeID::from_usize(0),
        });

        if let Node::FunctionDecl { ident, .. } = &store.nodes[id.to_usize()] {
            assert_eq!(*ident, NodeID::from_usize(0));
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

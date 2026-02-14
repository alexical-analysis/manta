use std::ops::Deref;

use crate::ast::{BlockStmt, Decl, Expr, LetExcept, LetStmt, Pattern, Stmt};
use crate::hir::{Node, NodeID, NodeTree, PatternNode};
use crate::parser::module::{BindingType, Module};
use crate::str_store;

pub struct Noder {}

impl Noder {
    pub fn new() -> Self {
        Noder {}
    }

    pub fn node(&mut self, module: Module) -> NodeTree {
        // Should these types be owned by the Noder type?
        let mut node_tree = NodeTree::new();

        for decl in module.get_decls() {
            self.node_decl(&mut node_tree, &module, decl);
        }

        node_tree
    }

    fn node_decl(&mut self, node_tree: &mut NodeTree, module: &Module, decl: &Decl) {
        match decl {
            Decl::Function(decl) => {
                let mut params = vec![];
                for param in &decl.params {
                    let param_id = node_tree.add_node(Node::VarDecl {
                        name: param.name,
                        type_spec: Some(param.type_spec.clone()),
                    });
                    params.push(param_id);
                }

                let body_id = self.node_block(node_tree, module, &decl.body);

                node_tree.add_root_node(Node::FunctionDecl {
                    name: decl.name,
                    params,
                    return_type: decl.return_type.clone(),
                    body: body_id,
                });
            }
            Decl::Type(decl) => {
                // create the node
                node_tree.add_root_node(Node::TypeDecl {
                    name: decl.name,
                    type_spec: decl.type_spec.clone(),
                });
            }
            Decl::Const(decl) => {
                // create the nodes
                let decl_id = node_tree.add_root_node(Node::VarDecl {
                    name: decl.name,
                    type_spec: None,
                });
                let value_node = Self::node_expr(node_tree, module, &decl.value);
                node_tree.add_node(Node::Assign {
                    target: decl_id,
                    value: value_node,
                });
            }
            Decl::Var(decl) => {
                // create the nodes
                let decl_id = node_tree.add_root_node(Node::VarDecl {
                    name: decl.name,
                    type_spec: None,
                });
                let value_node = Self::node_expr(node_tree, module, &decl.value);
                node_tree.add_node(Node::Assign {
                    target: decl_id,
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
    fn node_block(
        &mut self,
        node_tree: &mut NodeTree,
        module: &Module,
        block: &BlockStmt,
    ) -> NodeID {
        let mut stmt_ids = vec![];
        for stmt in &block.statements {
            let stmt_id = self.node_stmt(node_tree, module, stmt);
            stmt_ids.push(stmt_id);
        }

        node_tree.add_node(Node::Block {
            statements: stmt_ids,
        })
    }

    fn node_stmt(&mut self, node_tree: &mut NodeTree, module: &Module, stmt: &Stmt) -> NodeID {
        match stmt {
            Stmt::Let(stmt) => self.node_let(node_tree, module, stmt),
            Stmt::Assign(stmt) => {
                let l_id = Self::node_expr(node_tree, module, &stmt.lvalue);
                let r_id = Self::node_expr(node_tree, module, &stmt.rvalue);

                // TODO: should i check that l_id is an assignable node here or should that
                // happen later when I do full type checking? (defaulting to later for now)
                node_tree.add_node(Node::Assign {
                    target: l_id,
                    value: r_id,
                })
            }
            Stmt::Expr(stmt) => Self::node_expr(node_tree, module, &stmt.expr),
            Stmt::Return(stmt) => {
                let value = if let Some(v) = &stmt.value {
                    let value_id = Self::node_expr(node_tree, module, v);
                    Some(value_id)
                } else {
                    None
                };

                node_tree.add_node(Node::Return { value })
            }
            Stmt::Defer(stmt) => {
                let block_id = self.node_block(node_tree, module, &stmt.block);
                node_tree.add_node(Node::Defer { block: block_id })
            }
            Stmt::Match(stmt) => {
                let target_id = Self::node_expr(node_tree, module, &stmt.target);
                let mut arms = vec![];
                for arm in &stmt.arms {
                    let pat_id = Self::node_pattern(node_tree, &arm.pattern);
                    let block_id = self.node_block(node_tree, module, &arm.body);

                    let arm_id = node_tree.add_node(Node::MatchArm {
                        pattern: pat_id,
                        body: block_id,
                    });

                    arms.push(arm_id);
                }

                node_tree.add_node(Node::Match {
                    target: target_id,
                    arms,
                })
            }
            Stmt::Block(stmt) => self.node_block(node_tree, module, stmt),
            Stmt::If(stmt) => {
                let check_id = Self::node_expr(node_tree, module, &stmt.check);
                let success_id = self.node_block(node_tree, module, &stmt.success);
                let fail_id = stmt
                    .fail
                    .as_ref()
                    .map(|fail| self.node_block(node_tree, module, fail));

                node_tree.add_node(Node::If {
                    condition: check_id,
                    then_block: success_id,
                    else_block: fail_id,
                })
            }
        }
    }

    fn node_pattern(node_tree: &mut NodeTree, pattern: &Pattern) -> NodeID {
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
                node_tree.add_node(Node::Pattern(PatternNode::TypeSpec(pat.clone())))
            }
            Pattern::Payload(pat) => {
                let pat_id = Self::node_pattern(node_tree, &pat.pat);
                node_tree.add_node(Node::Pattern(PatternNode::Payload {
                    pat: pat_id,
                    payload: pat.payload,
                }))
            }
            Pattern::ModuleAccess(pat) => {
                let pat_id = Self::node_pattern(node_tree, &pat.pat);
                node_tree.add_node(Node::Pattern(PatternNode::ModuleAccess {
                    module: pat.module.name,
                    pat: pat_id,
                }))
            }
            Pattern::DotAccess(pat) => {
                let target_id = pat
                    .target
                    .clone()
                    .map(|t| Self::node_pattern(node_tree, &t));

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

    fn node_let(&mut self, node_tree: &mut NodeTree, module: &Module, stmt: &LetStmt) -> NodeID {
        let mut arms = vec![];
        let value_id = Self::node_expr(node_tree, module, &stmt.value);

        if let Pattern::Payload(pat) = &stmt.pattern {
            let type_spec = match pat.pat.deref() {
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

            let var_id = node_tree.add_node(Node::VarDecl {
                name: pat.payload,
                type_spec,
            });

            let ident_id = node_tree.add_node(Node::Identifier(pat.payload));
            let assign_id = node_tree.add_node(Node::Assign {
                target: var_id,
                value: ident_id,
            });

            let pat_id = Self::node_pattern(node_tree, &stmt.pattern);
            let arm_id = node_tree.add_node(Node::MatchArm {
                pattern: pat_id,
                body: assign_id,
            });
            arms.push(arm_id);
        } else {
            let pat_id = Self::node_pattern(node_tree, &stmt.pattern);
            let empty_body = node_tree.add_node(Node::Block { statements: vec![] });
            let arm_id = node_tree.add_node(Node::MatchArm {
                pattern: pat_id,
                body: empty_body,
            });
            arms.push(arm_id)
        };

        let default_id = match &stmt.except {
            LetExcept::Or { binding, body, .. } => {
                let body_id = self.node_block(node_tree, module, body);

                match *binding {
                    Some(b) => {
                        let pat_id = node_tree.add_node(Node::Pattern(PatternNode::Identifier(b)));
                        node_tree.add_node(Node::MatchArm {
                            pattern: pat_id,
                            body: body_id,
                        })
                    }
                    None => {
                        let pat_id = node_tree.add_node(Node::Pattern(PatternNode::Default));
                        node_tree.add_node(Node::MatchArm {
                            pattern: pat_id,
                            body: body_id,
                        })
                    }
                }
            }
            LetExcept::Wrap(expr) => {
                let enum_id = self.node_wrap_expr(node_tree, module, expr);
                if enum_id.is_none() {
                    panic!("not a valid target for a let wrap statement")
                }

                let body_id = node_tree.add_node(Node::Return { value: enum_id });
                let block_id = node_tree.add_node(Node::Block {
                    statements: vec![body_id],
                });

                let pat_id =
                    node_tree.add_node(Node::Pattern(PatternNode::Identifier(str_store::WRAP)));
                node_tree.add_node(Node::MatchArm {
                    pattern: pat_id,
                    body: block_id,
                })
            }
            LetExcept::Panic => {
                // TODO: can we reuse this node mabye?
                let panic_id = node_tree.add_node(Node::Identifier(str_store::PANIC));
                // TODO: need to actually pass the results of the call to the panic
                let body_id = node_tree.add_node(Node::Call {
                    func: panic_id,
                    args: vec![],
                });

                let pat_id = node_tree.add_node(Node::Pattern(PatternNode::Default));
                node_tree.add_node(Node::MatchArm {
                    pattern: pat_id,
                    body: body_id,
                })
            }
            LetExcept::None => {
                // TODO: this is only leagl if the pattern can not
                // fail to match we need to be able to varify this
                // at compile time maybe just treat this as `unreachable`?
                // right now we'll just panic...

                // TODO: can we reuse this node mabye?
                let panic_id = node_tree.add_node(Node::Identifier(str_store::PANIC));
                // TODO: need to actually pass the results of the call to the panic
                let body_id = node_tree.add_node(Node::Call {
                    func: panic_id,
                    args: vec![],
                });

                let pattern_id = node_tree.add_node(Node::Pattern(PatternNode::Default));
                node_tree.add_node(Node::MatchArm {
                    pattern: pattern_id,
                    body: body_id,
                })
            }
        };

        arms.push(default_id);

        node_tree.add_node(Node::Match {
            target: value_id,
            arms,
        })
    }

    /// convert an abitrary expression into a wrap node for the `let .Ok = expr wrap .Err` syntax
    /// if the given expression is not a valid enum expression None is returned instead.
    fn node_wrap_expr(
        &mut self,
        node_tree: &mut NodeTree,
        module: &Module,
        expr: &Expr,
    ) -> Option<NodeID> {
        let dot_expr = match expr {
            Expr::DotAccess(expr) => expr,
            _ => return None,
        };

        let target = match &dot_expr.target {
            Some(target) => target,
            // TODO: how do we check this more carfully, we need to fill in the type hole first by
            // checking what the return value of the function is
            None => {
                let wrap_id = node_tree.add_node(Node::Identifier(str_store::WRAP));
                let enum_id = node_tree.add_node(Node::EnumConstructor {
                    target: None,
                    variant: dot_expr.field,
                    payload: Some(wrap_id),
                });
                return Some(enum_id);
            }
        };

        let target = match target.deref() {
            Expr::Identifier(expr) => expr,
            // if we have a non-identifier target this isn't an enum variant
            _ => return None,
        };

        let scope_id = module
            .get_scope_id(target.token.source_id)
            .expect("could not find scope for identifier");
        let binding = module.find_binding(scope_id, target.name);
        if let Some(b) = binding {
            // if the identifier is a declared enum type we know this is a valid enum expression
            if b.binding_type != BindingType::EnumType {
                return None;
            }

            let wrap_id = node_tree.add_node(Node::Identifier(str_store::WRAP));
            let enum_id = node_tree.add_node(Node::EnumConstructor {
                target: Some(target.name),
                variant: dot_expr.field,
                payload: Some(wrap_id),
            });

            Some(enum_id)
        } else {
            // this is not a declared type, it could be a struct value though
            None
        }
    }

    fn node_expr(node_tree: &mut NodeTree, module: &Module, expr: &Expr) -> NodeID {
        match expr {
            Expr::IntLiteral(expr) => node_tree.add_node(Node::IntLiteral(*expr)),
            Expr::FloatLiteral(expr) => node_tree.add_node(Node::FloatLiteral(*expr)),
            Expr::StringLiteral(expr) => node_tree.add_node(Node::StringLiteral(*expr)),
            Expr::BoolLiteral(expr) => node_tree.add_node(Node::BoolLiteral(*expr)),
            Expr::Identifier(expr) => {
                let scope_id = module
                    .get_scope_id(expr.token.source_id)
                    .expect("could not get scope for identifier");
                match module.find_binding(scope_id, expr.name) {
                    // make sure this binding exists before we dereference it
                    // TODO: should I check type information here?
                    Some(_) => node_tree.add_node(Node::Identifier(expr.name)),
                    None => panic!("unknown identifier"),
                }
            }
            Expr::Binary(expr) => {
                let left_id = Self::node_expr(node_tree, module, &expr.left);
                let right_id = Self::node_expr(node_tree, module, &expr.right);
                node_tree.add_node(Node::Binary {
                    left: left_id,
                    operator: expr.operator,
                    right: right_id,
                })
            }
            Expr::Unary(expr) => {
                let expr_id = Self::node_expr(node_tree, module, &expr.operand);
                node_tree.add_node(Node::Unary {
                    operator: expr.operator,
                    operand: expr_id,
                })
            }
            Expr::Call(expr) => {
                let func_id = Self::node_expr(node_tree, module, &expr.func);

                let mut args = vec![];
                for arg in &expr.args {
                    let param_id = Self::node_expr(node_tree, module, arg);
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
                let target_id = Self::node_expr(node_tree, module, &expr.target);
                let idx_id = Self::node_expr(node_tree, module, &expr.index);

                node_tree.add_node(Node::Index {
                    target: target_id,
                    index: idx_id,
                })
            }
            Expr::Range(expr) => {
                let start_id = Self::node_expr(node_tree, module, &expr.start);
                let end_id = Self::node_expr(node_tree, module, &expr.end);
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
                        let scope_id = module
                            .get_scope_id(ident.token.source_id)
                            .expect("could not find scope_id for identifier");
                        let binding = module.find_binding(scope_id, ident.name);
                        if let Some(b) = binding {
                            if b.binding_type == BindingType::EnumType {
                                let target_id = Self::node_expr(node_tree, module, target);
                                return node_tree.add_node(Node::EnumConstructor {
                                    target: Some(target_id),
                                    variant: expr.field,
                                    payload: None,
                                });
                            } else {
                                // dot expressions that are not enum constructures must be field
                                // access expressions instead
                                let target_id = Self::node_expr(node_tree, module, target);
                                return node_tree.add_node(Node::FieldAccess {
                                    target: Some(target_id),
                                    field: expr.field,
                                });
                            }
                        }

                        panic!("unknown identifier")
                    }
                    _ => {
                        let target_id = Self::node_expr(node_tree, module, target);
                        node_tree.add_node(Node::FieldAccess {
                            target: Some(target_id),
                            field: expr.field,
                        })
                    }
                },
                // if there's no target it must be and enum
                None => node_tree.add_node(Node::EnumConstructor {
                    target: None,
                    variant: expr.field,
                    payload: None,
                }),
            },
            Expr::ModuleAccess(_expr) => todo!("modules are not yet supported"),
            Expr::MetaType(expr) => node_tree.add_node(Node::MetaType {
                type_spec: expr.type_spec.clone(),
            }),
            Expr::Alloc(expr) => {
                let meta_id = Self::node_expr(node_tree, module, &expr.meta_type);
                let mut options = vec![];
                for opt in &expr.options {
                    let opt_id = Self::node_expr(node_tree, module, opt);
                    options.push(opt_id);
                }

                node_tree.add_node(Node::Alloc {
                    meta_type: meta_id,
                    options,
                })
            }
            Expr::Free(expr) => {
                let ptr_id = Self::node_expr(node_tree, module, &expr.expr);
                node_tree.add_node(Node::Free { expr: ptr_id })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::path::Path;

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

        let mut noder = Noder::new();
        let node_tree = noder.node(module);

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

                    let mut noder = Noder::new();
                    let node_tree = noder.node(module);

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
            decl: Decl::Const(crate::ast::ConstDecl {
                name: 1,
                value: Expr::IntLiteral(42)
            }),
            expected: {
                let mut e = NodeTree::new();
                let decl_id = e.add_root_node(Node::VarDecl {
                    name: 1,
                    type_spec: None,
                });
                let value_id = e.add_node(Node::IntLiteral(42));
                e.add_node(Node::Assign {
                    target: decl_id,
                    value: value_id,
                });
                e
            }
        },
        node_var_decl_string_literal {
            decl: Decl::Var(crate::ast::VarDecl {
                name: 2,
                value: Expr::StringLiteral(3)
            }),
            expected: {
                let mut e = NodeTree::new();
                let decl_id = e.add_root_node(Node::VarDecl {
                    name: 2,
                    type_spec: None,
                });
                let value_id = e.add_node(Node::StringLiteral(3));
                e.add_node(Node::Assign {
                    target: decl_id,
                    value: value_id,
                });
                e
            }
        },
    );
}

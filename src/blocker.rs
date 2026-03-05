use std::collections::HashSet;

use crate::ast::{FunctionType, TypeSpec, UnaryOp};
use crate::hir::{Node, NodeID};
use crate::mir::{
    BasicBlock, BlockId, ConstValue, Instruction, Local, LocalId, MirFunction, MirModule,
    Terminator, ValueId,
};
use crate::noder::NodeTree;
use crate::str_store::{self, StrID};

pub struct BlockBuilder {
    instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
}

impl BlockBuilder {
    fn new() -> Self {
        BlockBuilder {
            instructions: vec![],
            terminator: None,
        }
    }

    fn add_instruction(&mut self, inst: Instruction) {
        if self.is_closed() {
            panic!("can not add instructions to a block that's aready closed");
        }
        self.instructions.push(inst);
    }

    fn set_terminator(&mut self, term: Terminator) {
        self.terminator = Some(term);
    }

    fn is_closed(&self) -> bool {
        self.terminator.is_some()
    }

    fn to_basic_block(&self) -> BasicBlock {
        let term = match &self.terminator {
            Some(term) => term,
            None => panic!("basic block must have a terminator"),
        };

        let mut created_values = HashSet::new();
        let mut block_args = vec![];
        for inst in self.instructions.clone() {
            match inst {
                Instruction::Const { result, .. } => {
                    created_values.insert(result);
                }
                Instruction::UnaryOp {
                    result, operand, ..
                } => {
                    created_values.insert(result);

                    if !created_values.contains(&operand) {
                        block_args.push(operand)
                    }
                }
                Instruction::BinaryOp {
                    result, lhs, rhs, ..
                } => {
                    created_values.insert(result);

                    if !created_values.contains(&lhs) {
                        block_args.push(lhs)
                    }
                    if !created_values.contains(&rhs) {
                        block_args.push(rhs);
                    }
                }
                Instruction::LoadLocal { result, .. } => {
                    created_values.insert(result);
                }
                Instruction::StoreLocal { value, .. } => {
                    if !created_values.contains(&value) {
                        block_args.push(value)
                    }
                }
                Instruction::Call { result, args, .. } => {
                    created_values.insert(result);

                    for arg in args {
                        if !created_values.contains(&arg) {
                            block_args.push(arg)
                        }
                    }
                }
                Instruction::CallTry { result, args, .. } => {
                    created_values.insert(result);

                    for arg in args {
                        if !created_values.contains(&arg) {
                            block_args.push(arg)
                        }
                    }
                }
                Instruction::VariantGetPayload { result, src, .. } => {
                    created_values.insert(result);

                    if !created_values.contains(&src) {
                        block_args.push(src)
                    }
                }
                Instruction::Move { src, .. } => {
                    if !created_values.contains(&src) {
                        block_args.push(src)
                    }
                }
                Instruction::Copy { src, .. } => {
                    if !created_values.contains(&src) {
                        block_args.push(src)
                    }
                }
                Instruction::DropLocal { .. } => { /* no value IDs here */ }
                Instruction::DeclareLocal { .. } => { /* no value IDs here */ }
                Instruction::SetInitialized { .. } => { /* no value IDs here */ }
            }
        }

        BasicBlock {
            block_args,
            instructions: self.instructions.clone(),
            terminator: term.clone(),
        }
    }
}

// TODO: do I need a function builder actually? The MirFunction might be enough actually
pub struct FunctionBuilder {
    name: StrID,
    params: Vec<StrID>,
    // TODO: the MIR probably needs it's own TypeSpec type that's more focused on the low level
    // details of the type (e.g. byte-size, layout, etc.)
    type_spec: TypeSpec,
    locals: Vec<Local>,         // Indexed by LocalId
    blocks: Vec<BlockBuilder>,  // Indexed by BlockId
    value_types: Vec<TypeSpec>, // Indexed by ValueId
}

impl FunctionBuilder {
    fn new(name: StrID, params: Vec<StrID>, type_spec: TypeSpec) -> Self {
        FunctionBuilder {
            name,
            type_spec,
            params,
            locals: vec![],
            blocks: vec![],
            value_types: vec![],
        }
    }

    fn add_instruction(&mut self, block_id: BlockId, inst: Instruction) {
        let block = self.get_block_mut(block_id);
        block.add_instruction(inst);
    }

    fn set_terminator(&mut self, block_id: BlockId, term: Terminator) {
        let block = self.get_block_mut(block_id);
        block.set_terminator(term);
    }

    fn add_block(&mut self) -> BlockId {
        let block_builder = BlockBuilder::new();

        self.blocks.push(block_builder);
        BlockId::from_u32(self.blocks.len() as u32)
    }

    fn add_value(&mut self, type_spec: TypeSpec) -> ValueId {
        self.value_types.push(type_spec);
        ValueId::from_u32(self.value_types.len() as u32)
    }

    fn get_block(&self, block_id: BlockId) -> &BlockBuilder {
        match self.blocks.get(block_id.as_idx()) {
            Some(block) => block,
            None => panic!("Unknown block {:?}", block_id),
        }
    }

    fn get_block_mut(&mut self, block_id: BlockId) -> &mut BlockBuilder {
        match self.blocks.get_mut(block_id.as_idx()) {
            Some(block) => block,
            None => panic!("Unknown block {:?}", block_id),
        }
    }

    fn block_is_closed(&self, block_id: BlockId) -> bool {
        self.get_block(block_id).is_closed()
    }

    fn to_mir_function(&self) -> MirFunction {
        if self.blocks.is_empty() {
            panic!("function must have an entry block")
        };

        let blocks = self.blocks.iter().map(|b| b.to_basic_block()).collect();

        MirFunction {
            name: self.name,
            params: self.params.clone(), // Parameter names and types
            type_spec: self.type_spec.clone(),
            blocks,
            entry_block: BlockId::from_u32(1),
            locals: self.locals.clone(),
            value_types: self.value_types.clone(),
        }
    }
}

pub fn block_hir(node_tree: &NodeTree) -> MirModule {
    // init block constructed to contain global initializations
    let mut fn_builder = FunctionBuilder::new(
        str_store::INIT,
        vec![],
        TypeSpec::Function(FunctionType {
            params: vec![],
            return_type: None,
        }),
    );

    let mut block_id = fn_builder.add_block();

    for node_id in &node_tree.roots {
        block_id = block_statement(node_tree, *node_id, &mut fn_builder, block_id);
    }

    // TODO: need to actually close block correctly
    if !fn_builder.block_is_closed(block_id) {
        fn_builder.set_terminator(block_id, Terminator::Return { value: None });
    }

    let init = fn_builder.to_mir_function();

    // block the rest of the functions
    let mut functions = vec![];
    for node_id in &node_tree.roots {
        if let Some(f) = block_root_node(node_tree, *node_id) {
            functions.push(f);
        }
    }

    MirModule::new(init, functions)
}

pub fn block_root_node(node_tree: &NodeTree, node_id: NodeID) -> Option<MirFunction> {
    let node = match node_tree.get_node(node_id) {
        Some(n) => n,
        None => panic!("type checking unknown node"),
    };
    let node = node.clone();

    match node {
        Node::FunctionDecl {
            ident,
            params,
            body,
        } => {
            let name = match node_tree.get_node(ident) {
                Some(n) => n,
                None => panic!("missing function name (can not find node)"),
            };

            let name = match name {
                Node::Identifier(ident) => ident,
                _ => panic!("function name wasn't an identifier"),
            };

            let params: Vec<StrID> = params
                .iter()
                .map(|node_id| {
                    let node = node_tree
                        .get_node(*node_id)
                        .expect("failed to find param node");

                    let node_id = if let Node::VarDecl { ident } = node {
                        ident
                    } else {
                        panic!("param must be a var decl but was a {:?}", node)
                    };

                    let node = node_tree
                        .get_node(*node_id)
                        .expect("failed to find param identifier");

                    if let Node::Identifier(ident) = node {
                        *ident
                    } else {
                        panic!("param mut be an identifier but was a {:?}", node)
                    }
                })
                .collect();

            let func_type = match node_tree.get_type(node_id) {
                Some(ts) => ts.clone(),
                None => panic!("missing type for function decl"),
            };

            let mut fn_builder = FunctionBuilder::new(*name, params, func_type.clone());

            let mut block_id = fn_builder.add_block();
            let body = get_block_stmts(node_tree, body);
            for stmt in body {
                block_id = block_statement(node_tree, *stmt, &mut fn_builder, block_id);
            }

            // TODO: this is a hack and we need to do things correctly
            if !fn_builder.block_is_closed(block_id) {
                fn_builder.set_terminator(block_id, Terminator::Return { value: None });
            }

            Some(fn_builder.to_mir_function())
        }
        _ => None,
    }
}

pub fn block_statement(
    node_tree: &NodeTree,
    node_id: NodeID,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
) -> BlockId {
    if fn_builder.block_is_closed(block_id) {
        // if the block is closed just skip all the remaining instructions as they are no longer
        // reachable, trying to adding them would cause a panic
        return block_id;
    }

    let node = match node_tree.get_node(node_id) {
        Some(n) => n,
        None => panic!("trying to block unknown node"),
    };
    let node = node.clone();

    match node {
        Node::Invalid => {
            let result = fn_builder.add_value(TypeSpec::Panic);
            fn_builder.add_instruction(
                block_id,
                Instruction::Call {
                    result,
                    func: str_store::PANIC,
                    // TODO: what are the args here? Should the 'Node::Invalid' have associated error
                    // info so that we panic with a syntax error or something?
                    args: vec![],
                },
            );
            fn_builder.set_terminator(block_id, Terminator::Unreachable);

            block_id
        }
        Node::If {
            condition,
            then_block,
            else_block,
        } => {
            let expr_value = block_expression(node_tree, condition, fn_builder, block_id);

            let mut true_block_id = fn_builder.add_block();
            let mut false_block_id = fn_builder.add_block();
            let merge_block_id = fn_builder.add_block();

            let stmts = get_block_stmts(node_tree, then_block);
            for stmt in stmts {
                // this block might be terminated by a seperate statement in which case we'll get a
                // new block to work on so handle that here
                true_block_id = block_statement(node_tree, *stmt, fn_builder, true_block_id);
            }

            fn_builder.set_terminator(
                block_id,
                Terminator::Branch {
                    cond: expr_value,
                    true_target: true_block_id,
                    false_target: false_block_id,
                },
            );

            if let Some(n) = else_block {
                let stmts = get_block_stmts(node_tree, n);
                for stmt in stmts {
                    // this block might be terminated by a seperate statement in which case we'll get a
                    // new block to work on so handle that here
                    false_block_id = block_statement(node_tree, *stmt, fn_builder, false_block_id);
                }
            }

            // TODO: this block might have been terminated with a Terminator::Unreachable (for
            // example if there's a panic inside the if statment). If that's the case we don't want
            // to overwrite that terminator. That's the only valid case though, all other cases
            // should result in us getting an unterminated brach. There are 2 ways I can see to
            // solve this
            // 1) is to NEVER hand back a terminated block from 'block_statement' which means we
            //    need to figure out what to do with panics
            // 2) is to explicitly check for that case either here in or set_terminator.
            // I'm not yet sure which one I prefer so I'll have to come back to it.
            fn_builder.set_terminator(
                true_block_id,
                Terminator::Jump {
                    target: merge_block_id,
                },
            );

            // TODO: same here as above
            fn_builder.set_terminator(
                false_block_id,
                Terminator::Jump {
                    target: merge_block_id,
                },
            );

            merge_block_id
        }
        Node::VarDecl { ident } => {
            let ident_name = match node_tree.get_node(ident) {
                Some(Node::Identifier(s)) => *s,
                _ => panic!("var decl ident must be an identifier"),
            };

            let type_spec = match node_tree.get_type(node_id) {
                Some(ts) => ts.clone(),
                None => TypeSpec::Int64,
            };

            let local_id = LocalId::from_u32((fn_builder.locals.len() + 1) as u32);
            fn_builder.locals.push(Local {
                id: local_id,
                type_spec,
                name: ident_name,
            });

            fn_builder.add_instruction(block_id, Instruction::DeclareLocal { local: local_id });
            block_id
        }
        Node::Assign { target, value } => {
            let value_result = block_expression(node_tree, value, fn_builder, block_id);

            // Determine if the target is assignable
            let target_node = match node_tree.get_node(target) {
                Some(n) => n.clone(),
                None => panic!("assignment target node not found"),
            };

            match target_node {
                Node::Identifier(s) => {
                    // Simple variable assignment
                    if let Some(local) = fn_builder.locals.iter().find(|l| l.name == s) {
                        fn_builder.add_instruction(
                            block_id,
                            Instruction::StoreLocal {
                                local: local.id,
                                value: value_result,
                            },
                        );
                    } else {
                        panic!("Unknown identifier in assignment: {:?}", s)
                    }
                }
                Node::Unary {
                    operator: UnaryOp::Dereference,
                    operand,
                } => {
                    // Pointer dereference assignment: *p = value
                    let _ptr_value = block_expression(node_tree, operand, fn_builder, block_id);
                    // For now, treat dereferenced assignment as a store
                    // TODO: Implement proper pointer handling
                }
                Node::Index { target: arr, index } => {
                    // Array index assignment: arr[i] = value
                    let _arr_value = block_expression(node_tree, arr, fn_builder, block_id);
                    let _index_value = block_expression(node_tree, index, fn_builder, block_id);
                    // TODO: Implement proper index assignment
                }
                Node::FieldAccess {
                    target: obj,
                    field: _,
                } => {
                    // Field assignment: obj.field = value
                    let _obj_value =
                        obj.map(|o| block_expression(node_tree, o, fn_builder, block_id));
                    // TODO: Implement proper field assignment
                }
                _ => panic!(
                    "assignment target must be assignable (identifier, dereference, index, or field access), got: {:?}",
                    target_node
                ),
            }

            block_id
        }
        Node::Return { value } => {
            let return_value = value.map(|v| block_expression(node_tree, v, fn_builder, block_id));
            fn_builder.set_terminator(
                block_id,
                Terminator::Return {
                    value: return_value,
                },
            );
            block_id
        }
        Node::Defer { block } => {
            // TODO: For now, just process the block statements (proper defer handling would need more context)
            let mut defer_block_id = fn_builder.add_block();
            let stmts = get_block_stmts(node_tree, block).clone();
            for stmt in stmts {
                defer_block_id = block_statement(node_tree, stmt, fn_builder, defer_block_id);
            }

            // TODO: just returning the original block for now while we figure out how defer should
            // actually work
            block_id
        }
        Node::Match { target, arms } => {
            let target_value = block_expression(node_tree, target, fn_builder, block_id);

            let merge_block_id = fn_builder.add_block();

            let mut arm_blocks = vec![];
            for arm_id in arms.iter() {
                let mut arm_block_id = fn_builder.add_block();
                arm_blocks.push(arm_block_id);

                let (pattern, stmts) = match node_tree.get_node(*arm_id) {
                    Some(Node::MatchArm { pattern, body }) => {
                        let stmts = get_block_stmts(node_tree, *body).clone();
                        (*pattern, stmts)
                    }
                    _ => panic!("match arm must be a MatchArm node"),
                };

                // TODO: Extract variables from the pattern and add them as locals
                // do we need these bindings?
                let _bindings = extract_pattern_variables(node_tree, pattern, fn_builder);

                for stmt in stmts {
                    arm_block_id = block_statement(node_tree, stmt, fn_builder, arm_block_id);
                }

                // TODO: So the block could be terminated already...
                // what do we do here? This is popping up alot
                fn_builder.set_terminator(
                    arm_block_id,
                    Terminator::Jump {
                        target: merge_block_id,
                    },
                );
            }

            let arm_variants = (0..arms.len()).map(|i| i as u32).collect();
            fn_builder.set_terminator(
                block_id,
                Terminator::SwitchVariant {
                    value: target_value,
                    arm_variants, // this is the variant tag id
                    arm_blocks,
                },
            );

            merge_block_id
        }
        Node::Block { statements } => {
            let mut current_block = block_id;
            for stmt in statements.clone() {
                current_block = block_statement(node_tree, stmt, fn_builder, current_block);
            }
            current_block
        }
        Node::FunctionDecl { .. }
        | Node::TypeDecl { .. }
        | Node::UseDecl { .. }
        | Node::ModDecl { .. } => {
            // These are top-level declarations and shouldn't appear as statements
            block_id
        }
        _ => {
            // For expression statements
            let _ = block_expression(node_tree, node_id, fn_builder, block_id);
            block_id
        }
    }
}

fn block_expression(
    node_tree: &NodeTree,
    node_id: NodeID,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
) -> ValueId {
    if fn_builder.block_is_closed(block_id) {
        panic!("can not block expressions on a closed block");
    }

    let node = match node_tree.get_node(node_id) {
        Some(n) => n,
        None => panic!("trying to block unknown node"),
    };
    let node = node.clone();

    match node {
        Node::IntLiteral(n) => {
            let result = fn_builder.add_value(TypeSpec::Int64);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstInt(n),
                },
            );
            result
        }
        Node::FloatLiteral(f) => {
            let result = fn_builder.add_value(TypeSpec::Float64);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstFloat(f),
                },
            );
            result
        }
        Node::StringLiteral(s) => {
            let result = fn_builder.add_value(TypeSpec::String);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstString(s),
                },
            );
            result
        }
        Node::BoolLiteral(b) => {
            let result = fn_builder.add_value(TypeSpec::Bool);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstBool(b),
                },
            );
            result
        }
        Node::Identifier(s) => {
            // TODO: this should be a fn_build.find_local call
            // Look up the local variable for this identifier
            let (local_id, type_spec) = {
                match fn_builder.locals.iter().find(|l| l.name == s) {
                    Some(local) => (local.id, local.type_spec.clone()),
                    None => panic!("Unknown identifier: {:?}", s),
                }
            };
            let result = fn_builder.add_value(type_spec);
            fn_builder.add_instruction(
                block_id,
                Instruction::LoadLocal {
                    result,
                    local: local_id,
                },
            );
            result
        }
        Node::Binary {
            left,
            operator,
            right,
        } => {
            let lhs = block_expression(node_tree, left, fn_builder, block_id);
            let rhs = block_expression(node_tree, right, fn_builder, block_id);
            let result = fn_builder.add_value(TypeSpec::Int64);
            fn_builder.add_instruction(
                block_id,
                Instruction::BinaryOp {
                    result,
                    op: operator,
                    lhs,
                    rhs,
                },
            );
            result
        }
        Node::Unary { operator, operand } => {
            let operand_val = block_expression(node_tree, operand, fn_builder, block_id);
            let result = fn_builder.add_value(TypeSpec::Int64);
            fn_builder.add_instruction(
                block_id,
                Instruction::UnaryOp {
                    result,
                    op: operator,
                    operand: operand_val,
                },
            );
            result
        }
        Node::Call { func, args } => {
            let func_name = match node_tree.get_node(func) {
                Some(Node::Identifier(s)) => *s,
                // TODO: this is only true for now, once we add closures we'll probably have to
                // rethink this since many expressions might return a callable closure
                _ => panic!("function in call must be an identifier"),
            };

            let arg_values: Vec<ValueId> = args
                .iter()
                .map(|arg| block_expression(node_tree, *arg, fn_builder, block_id))
                .collect();

            // TODO: we actually need to get the functions type spec
            let return_type = match node_tree.get_type(node_id) {
                Some(TypeSpec::Function(f)) => f.return_type.clone(),
                _ => panic!("missing type for function"),
            };

            let result = if let Some(t) = return_type {
                Some(fn_builder.add_value(*t))
            } else {
                None
            };

            fn_builder.add_instruction(
                block_id,
                Instruction::Call {
                    result,
                    func: func_name,
                    args: arg_values,
                },
            );

            result
        }
        Node::Index { target, index } => {
            let _target_val = block_expression(node_tree, target, fn_builder, block_id);
            let _index_val = block_expression(node_tree, index, fn_builder, block_id);
            let result = fn_builder.add_value(TypeSpec::Int64);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstInt(0),
                },
            );
            result
        }
        Node::FieldAccess { target, field: _ } => {
            if let Some(tgt) = target {
                let _target_val = block_expression(node_tree, tgt, fn_builder, block_id);
            }
            // Field access creates a special identifier reference to the field
            let result = fn_builder.add_value(TypeSpec::Int64);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstInt(0),
                },
            );
            result
        }
        Node::EnumConstructor {
            target,
            variant: _,
            payload,
        } => {
            let _target_module = target.and_then(|t| {
                if let Node::Identifier(_) = node_tree.get_node(t)? {
                    Some(t)
                } else {
                    None
                }
            });

            let _payload_val =
                payload.map(|p| block_expression(node_tree, p, fn_builder, block_id));
            let result = fn_builder.add_value(TypeSpec::Int64);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstInt(0),
                },
            );
            result
        }
        Node::Range { start, end } => {
            let _start_val = block_expression(node_tree, start, fn_builder, block_id);
            let _end_val = block_expression(node_tree, end, fn_builder, block_id);
            let result = fn_builder.add_value(TypeSpec::Int64);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstInt(0),
                },
            );
            result
        }
        Node::MetaType => {
            let result = fn_builder.add_value(TypeSpec::Int64);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstInt(0),
                },
            );
            result
        }
        Node::Alloc { meta_type, options } => {
            let _meta_type_val = block_expression(node_tree, meta_type, fn_builder, block_id);
            let _option_vals: Vec<_> = options
                .iter()
                .map(|opt| block_expression(node_tree, *opt, fn_builder, block_id))
                .collect();
            let result = fn_builder.add_value(TypeSpec::Int64);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstInt(0),
                },
            );
            result
        }
        Node::Free { expr } => {
            let _expr_val = block_expression(node_tree, expr, fn_builder, block_id);
            let result = fn_builder.add_value(TypeSpec::Int64);
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    result,
                    value: ConstValue::ConstInt(0),
                },
            );
            result
        }
        Node::ModuleAccess { module: _, expr } => {
            block_expression(node_tree, expr, fn_builder, block_id)
        }
        _ => panic!("Unexpected node type in block_expression: {:?}", node),
    }
}

fn extract_pattern_variables(
    node_tree: &NodeTree,
    pattern_id: NodeID,
    fn_builder: &mut FunctionBuilder,
) -> Vec<(StrID, LocalId)> {
    // Returns list of (identifier, local_id) pairs for variables bound by this pattern
    let mut bindings = vec![];

    let pattern_node = match node_tree.get_node(pattern_id) {
        Some(n) => n,
        None => return bindings,
    };

    let pattern = match pattern_node {
        Node::Pattern(pat) => pat,
        _ => return bindings,
    };

    match pattern {
        crate::hir::PatternNode::Payload {
            pat: _,
            payload_ident,
        } => {
            // Extract the identifier that will be bound from the payload
            if let Some(Node::Identifier(name)) = node_tree.get_node(*payload_ident) {
                // Check if this variable already exists (avoid duplicates)
                if !fn_builder.locals.iter().any(|l| l.name == *name) {
                    let local_id = LocalId::from_u32((fn_builder.locals.len() + 1) as u32);
                    fn_builder.locals.push(Local {
                        id: local_id,
                        type_spec: TypeSpec::Int64,
                        name: *name,
                    });
                    bindings.push((*name, local_id));
                } else if let Some(local) = fn_builder.locals.iter().find(|l| l.name == *name) {
                    bindings.push((*name, local.id));
                }
            }
        }
        crate::hir::PatternNode::DotAccess {
            target: _,
            field: _,
        } => {
            // DotAccess patterns don't bind variables directly
        }
        crate::hir::PatternNode::ModuleAccess { module: _, pat } => {
            // Recursively extract from the nested pattern
            bindings.extend(extract_pattern_variables(node_tree, *pat, fn_builder));
        }
        crate::hir::PatternNode::Identifier(ident_id) => {
            // Bind the identifier from a wildcard/identifier pattern
            if let Some(Node::Identifier(name)) = node_tree.get_node(*ident_id) {
                if !fn_builder.locals.iter().any(|l| l.name == *name) {
                    let local_id = LocalId::from_u32((fn_builder.locals.len() + 1) as u32);
                    fn_builder.locals.push(Local {
                        id: local_id,
                        type_spec: TypeSpec::Int64,
                        name: *name,
                    });
                    bindings.push((*name, local_id));
                } else if let Some(local) = fn_builder.locals.iter().find(|l| l.name == *name) {
                    bindings.push((*name, local.id));
                }
            }
        }
        // Literal and TypeSpec patterns don't bind variables
        _ => {}
    }

    bindings
}

fn get_block_stmts(node_tree: &NodeTree, node_id: NodeID) -> &Vec<NodeID> {
    let node = node_tree
        .get_node(node_id)
        .expect("missing then block of the if statement");

    match node {
        Node::Block { statements } => statements,
        _ => panic!("the node was not a valid block"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::path::Path;

    fn assert_file_path_eq(path: &std::path::Path, blocker_dir: &Path) {
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

        let node_tree = crate::noder::node_module(module);
        let mir_module = block_hir(&node_tree);

        let json_output = serde_json::to_string_pretty(&mir_module)
            .expect("Failed to serialize MirModule to JSON");

        let blocker_file = blocker_dir.join(format!("{}.json", file_name));

        if blocker_file.exists() {
            let expected_json = match fs::read_to_string(&blocker_file) {
                Ok(s) => s,
                Err(_) => panic!("Failed to read {}", blocker_file.display()),
            };

            assert_eq!(
                json_output, expected_json,
                "Blocker output mismatch for {}",
                file_name
            );
        } else {
            fs::create_dir_all(blocker_dir).expect("Failed to create blocker test directory");

            match fs::write(&blocker_file, &json_output) {
                Ok(_) => (),
                Err(_) => panic!("Failed to write blocker output to {:?}", blocker_file),
            };

            panic!(
                "Generated new blocker output file: {:?}. Please verify its correctness.",
                blocker_file
            );
        }
    }

    include!(concat!(env!("OUT_DIR"), "/generated_blocker_tests.rs"));
}

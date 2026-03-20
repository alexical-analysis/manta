use std::collections::HashSet;

use crate::hir::{FunctionType, Node, NodeID, TypeSpec};
use crate::mir::{
    BasicBlock, BlockId, Instruction, Local, LocalId, MirFunction, MirModule, Terminator, ValueId,
};
use crate::noder::NodeTree;
use crate::str_store::{self, StrID};

#[derive(Debug, Clone)]
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
        if self.terminator.is_some() {
            panic!("terminator was already set for this block")
        }

        self.terminator = Some(term);
    }

    fn is_closed(&self) -> bool {
        self.terminator.is_some()
    }

    fn to_basic_block(&self) -> BasicBlock {
        let term = match &self.terminator {
            Some(term) => term,
            None => {
                eprintln!(
                    "TODO: basic block must have a terminator setting to unreachable for now"
                );
                &Terminator::Unreachable
            }
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

    fn add_local(&mut self, name: StrID, type_spec: TypeSpec) -> LocalId {
        let local_id = LocalId::from_u32(self.locals.len() as u32);
        let local = Local { name, type_spec };

        self.locals.push(local);

        local_id
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

        // TODO: this logic is complex enough that I should move it into a dedicated function

        // it's possible for a basic block in the function to be empty if all other blocks
        // terminated without jumping to it. For example if every arm in a match statement
        // returns before the end of the function. here we take a quick walk through the
        // blocks and cull any that are not referenced by any other blocks in the function
        let mut valid_blocks = vec![];

        eprintln!("to_mir_function");

        let mut block_que = SetQueue::new();
        block_que.push(BlockId::from_u32(1));
        while let Some(b) = block_que.pop() {
            let block_builder = self.blocks[b.as_idx()].clone();
            eprintln!("\tvalid block {:?}", block_builder);
            let block = block_builder.to_basic_block();

            match block.terminator {
                Terminator::Return { .. } => {}
                Terminator::Unreachable => {}
                Terminator::Jump { target } => {
                    block_que.push(target);
                }
                Terminator::Branch {
                    true_target,
                    false_target,
                    ..
                } => {
                    block_que.push(true_target);
                    block_que.push(false_target);
                }
                Terminator::SwitchVariant { ref arms, .. } => {
                    for arm in arms {
                        block_que.push(arm.block);
                    }
                }
            }

            valid_blocks.push(block);
        }

        MirFunction {
            name: self.name,
            params: self.params.clone(),
            type_spec: self.type_spec.clone(),
            blocks: valid_blocks,
            entry_block: BlockId::from_u32(1),
            locals: self.locals.clone(),
            value_types: self.value_types.clone(),
        }
    }
}

struct SetQueue {
    queue: Vec<BlockId>,
    set: HashSet<BlockId>,
}

impl SetQueue {
    fn new() -> Self {
        SetQueue {
            queue: vec![],
            set: HashSet::new(),
        }
    }

    fn push(&mut self, block_id: BlockId) {
        if self.set.insert(block_id) {
            self.queue.push(block_id)
        }
    }

    fn pop(&mut self) -> Option<BlockId> {
        self.queue.pop()
    }
}

pub fn block_hir(node_tree: &NodeTree) -> MirModule {
    // init block constructed to contain global initializations
    let mut fn_builder = FunctionBuilder::new(
        str_store::INIT,
        vec![],
        TypeSpec::Function(FunctionType {
            params: vec![],
            return_type: Box::new(TypeSpec::Unit),
        }),
    );

    let mut init_block_id = fn_builder.add_block();

    for node_id in &node_tree.roots {
        match block_init_statement(node_tree, *node_id, &mut fn_builder, init_block_id) {
            Some(block) => init_block_id = block,
            None => break,
        }
    }

    // make sure we close the init block
    fn_builder.set_terminator(init_block_id, Terminator::Return { value: None });
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
            let name = get_ident_name(node_tree, ident);

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

                    if let Node::Identifier { name, .. } = node {
                        *name
                    } else {
                        panic!("param mut be an identifier but was a {:?}", node)
                    }
                })
                .collect();

            let func_type = match node_tree.get_type(node_id) {
                Some(ts) => ts.clone(),
                None => panic!("missing type for function decl"),
            };

            let mut fn_builder = FunctionBuilder::new(name, params, func_type.clone());

            let mut block_id = fn_builder.add_block();
            let body = get_block_stmts(node_tree, body);
            for stmt in body {
                match block_statement(node_tree, *stmt, &mut fn_builder, block_id) {
                    Some(block) => block_id = block,
                    None => break,
                }
            }

            Some(fn_builder.to_mir_function())
        }
        _ => None,
    }
}

pub fn block_init_statement(
    node_tree: &NodeTree,
    node_id: NodeID,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
) -> Option<BlockId> {
    if fn_builder.block_is_closed(block_id) {
        // if the block is closed just skip all the remaining instructions as they are no longer
        // reachable, trying to adding them would cause a panic
        return None;
    }

    let node = match node_tree.get_node(node_id) {
        Some(n) => n,
        None => panic!("type checking unknown node"),
    };
    let node = node.clone();

    match node {
        // Function declarations are skipped in the init function
        Node::FunctionDecl { .. } => Some(block_id),
        Node::Return { .. } => panic!("can not return from the root context"),
        _ => {
            let block_id = block_statement(node_tree, node_id, fn_builder, block_id);
            match block_id {
                Some(block) => Some(block),
                None => panic!("invalid root statement"),
            }
        }
    }
}

pub fn block_statement(
    node_tree: &NodeTree,
    node_id: NodeID,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
) -> Option<BlockId> {
    if fn_builder.block_is_closed(block_id) {
        // if the block is closed just skip all the remaining instructions as they are no longer
        // reachable, trying to adding them would cause a panic
        return None;
    }

    let node = match node_tree.get_node(node_id) {
        Some(n) => n,
        None => panic!("type checking unknown node"),
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

            // return a None because this block is closed and there's no more blocks that we know
            // about at this level
            None
        }
        Node::If {
            condition,
            then_block,
            else_block,
        } => {
            // the condition needs to be computed in the previous block, expressions can trigger
            // control flow so this should never create a new block
            let expr_value = block_expression(node_tree, condition, fn_builder, block_id);

            let true_block_id = fn_builder.add_block();
            let false_block_id = fn_builder.add_block();

            let block = get_block_stmts(node_tree, then_block);

            for stmt in block {
                block_statement(node_tree, *stmt, fn_builder, true_block_id);
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
                let block = get_block_stmts(node_tree, n);

                for stmt in block {
                    block_statement(node_tree, *stmt, fn_builder, false_block_id);
                }
            }

            let merge_block_id = fn_builder.add_block();
            fn_builder.set_terminator(
                true_block_id,
                Terminator::Jump {
                    target: merge_block_id,
                },
            );
            fn_builder.set_terminator(
                false_block_id,
                Terminator::Jump {
                    target: merge_block_id,
                },
            );

            Some(merge_block_id)
        }
        _ => {
            // TODO: remove me once all the nodes have been covered.
            // for now just return the block
            eprintln!(
                "TODO: skipping {:?} for now and just returning the block_id",
                node
            );
            Some(block_id)
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
        // if the block is closed just skip all the remaining instructions as they are no longer
        // reachable, trying to adding them would cause a panic
        panic!("can not block expressions on a closed block");
    }

    let node = match node_tree.get_node(node_id) {
        Some(n) => n,
        None => panic!("type checking unknown node"),
    };
    let node = node.clone();

    match node {
        _ => {
            // TODO: implement this
            ValueId::nil()
        }
    }
}

fn get_block_stmts(node_tree: &NodeTree, node_id: NodeID) -> &Vec<NodeID> {
    let node = node_tree.get_node(node_id).expect("missing block node");

    match node {
        Node::Block { statements } => statements,
        _ => panic!("the node was not a valid block"),
    }
}

fn get_ident_name(node_tree: &NodeTree, node_id: NodeID) -> StrID {
    let node = node_tree
        .get_node(node_id)
        .expect("missing identifier node");

    match node {
        Node::Identifier { name, .. } => *name,
        _ => panic!("node was not an identifier"),
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

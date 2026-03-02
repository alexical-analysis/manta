use crate::ast::{FunctionType, TypeSpec};
use crate::hir::{Node, NodeID};
use crate::mir::{
    BasicBlock, BlockId, ConstValue, Instruction, Local, MirFunction, MirModule, Terminator,
    ValueId,
};
use crate::noder::NodeTree;
use crate::str_store::{self, StrID};

pub struct BlockBuilder {
    block_args: Vec<ValueId>,
    instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
}

impl BlockBuilder {
    fn new() -> Self {
        BlockBuilder {
            block_args: vec![],
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

        if self.instructions.is_empty() {
            panic!("basic block is empty")
        }

        BasicBlock {
            block_args: self.block_args.clone(),
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
    locals: Vec<Local>,        // Indexed by LocalId
    blocks: Vec<BlockBuilder>, // Indexed by BlockId
}

impl FunctionBuilder {
    fn new(name: StrID, params: Vec<StrID>, type_spec: TypeSpec) -> Self {
        FunctionBuilder {
            name,
            type_spec,
            params,
            locals: vec![],
            blocks: vec![],
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
        // TODO: need a block builder so we don't have to have a bunch of placeholdrs like this
        self.blocks.push(BlockBuilder::new());

        BlockId::from_u32(self.blocks.len() as u32)
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

    let block_id = fn_builder.add_block();

    for node_id in &node_tree.roots {
        block_function(node_tree, *node_id, &mut fn_builder, block_id);
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

pub fn block_function(
    node_tree: &NodeTree,
    node_id: NodeID,
    fn_builder: &mut FunctionBuilder,
    block_id: BlockId,
) {
    if fn_builder.block_is_closed(block_id) {
        // if the block is closed just skip all the remaining instructions as they are no longer
        // reachable, trying to adding them would cause a panic
        return;
    }

    let node = match node_tree.get_node(node_id) {
        Some(n) => n,
        None => panic!("type checking unknown node"),
    };
    let node = node.clone();

    match node {
        Node::Invalid => {
            fn_builder.add_instruction(
                block_id,
                Instruction::Call {
                    func: str_store::PANIC,
                    // TODO: what are the args here? Should the 'Node::Invalid' have associated error
                    // info so that we panic with a syntax error or something?
                    args: vec![],
                },
            );
            fn_builder.set_terminator(block_id, Terminator::Unreachable);
        }
        _ => {
            // need to support all the rest of the nodes, for now just create a default block
            fn_builder.add_instruction(
                block_id,
                Instruction::Const {
                    type_spec: TypeSpec::Int64,
                    value: ConstValue::ConstInt(1),
                },
            );
            fn_builder.set_terminator(block_id, Terminator::Unreachable);
        }
    }
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
            let body = match node_tree.get_node(body) {
                Some(n) => n,
                None => panic!("missing function body (can not find node)"),
            };

            let body = match body {
                Node::Block { statements } => statements,
                _ => panic!("function body is not a block and thus is not valid"),
            };

            let name = match node_tree.get_node(ident) {
                Some(n) => n,
                None => panic!("missing function name (can not find node)"),
            };

            let name = match name {
                Node::Identifier(ident) => ident,
                _ => panic!("function name wasn't an identifier"),
            };

            let params = params
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

            let mut fn_builder = FunctionBuilder::new(*name, params, func_type);
            let init_block = fn_builder.add_block();
            for stmt in body {
                block_function(node_tree, *stmt, &mut fn_builder, init_block);
            }

            Some(fn_builder.to_mir_function())
        }
        _ => None,
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

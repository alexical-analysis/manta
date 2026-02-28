use crate::blocker::block_hir;
use crate::noder::node_module;
use crate::parser::Parser;
use crate::str_store::StrStore;

pub struct Compiler {
    _source: String,
}

impl Compiler {
    pub fn new(source: String) -> Self {
        Compiler { _source: source }
    }

    pub fn compile(&self) {
        // TODO: this should actually be created in the parse_module call and
        // be owned by the module itself
        let mut str_store = StrStore::new();

        let parser = Parser::new("mod main".to_string());
        let module = parser.parse_module(&mut str_store);
        if !module.get_errors().is_empty() {
            panic!("errors in the parser: {:?}", module.get_errors())
        }

        // build the HIR from the AST
        let node_tree = node_module(module);

        // build the MIR from the HIR
        let _blocker = block_hir(node_tree);

        // TODO: build the LLVM IR from the MIR

        // TODO: generate a .o file using llvm

        // TODO: run the system linker to generate a final binary file

        // TODO: print any errors the were encountered during compilation
    }
}

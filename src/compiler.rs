use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::Module;
use crate::noder::Noder;
use crate::parser::Parser;
use crate::str_store::StrStore;

pub struct Compiler {
    source: String,
}

impl Compiler {
    pub fn new(source: String) -> Self {
        Compiler { source }
    }

    pub fn compile(&self) {
        let mut str_store = StrStore::new();

        let parser = Parser::new("mod main".to_string());
        let module = parser.parse_module(&mut str_store).unwrap();

        // TODO: if the str_store was part of the module this wouldn't be necessary
        let noder = Noder::new(&mut str_store);
        let _node_tree = noder.node(&str_store, module);
    }
}

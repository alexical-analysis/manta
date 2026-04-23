use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::hir;
use crate::str_store::StrID;
use crate::str_store::StrStore;

// PubMod is the public interface to a module
#[derive(Debug, Serialize, Deserialize)]
pub struct PubMod {
    // TODO: we need this so we can convert from internal module strID to the strID's of whatever external
    // module is trying to use these types. We can probably slim down the StrStore to only contain referenced
    // types so that it serializes well but it needs to be a copy, not a reference
    str_store: StrStore,
    mod_name: StrID,
    hir_types: HashMap<StrID, hir::TypeSpec>,
}

impl PubMod {
    pub fn new(
        str_store: StrStore,
        mod_name: StrID,
        hir_types: HashMap<StrID, hir::TypeSpec>,
    ) -> Self {
        PubMod {
            str_store,
            mod_name,
            hir_types,
        }
    }

    pub fn name(&self) -> String {
        self.str_store
            .get_string(self.mod_name)
            .expect("failed to get module name")
    }
}

pub struct MappedPubMod<'str, 'p> {
    mapped_store: &'str StrStore,
    pub_mod: &'p PubMod,
}

impl<'str, 'p> MappedPubMod<'str, 'p> {
    pub fn new(str_store: &'str StrStore, pub_mod: &'p PubMod) -> Self {
        MappedPubMod {
            mapped_store: str_store,
            pub_mod,
        }
    }

    pub fn find_hir_typespec(&self, name: StrID) -> Option<&hir::TypeSpec> {
        match self.convert_str_id(name) {
            Some(key) => self.pub_mod.hir_types.get(&key),
            None => None,
        }
    }

    pub fn convert_str_id(&self, id: StrID) -> Option<StrID> {
        let key = match self.mapped_store.get_string(id) {
            Some(k) => k,
            None => return None,
        };

        self.pub_mod.str_store.find_id(&key)
    }
}

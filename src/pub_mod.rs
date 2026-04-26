use crate::noder::NodeTree;
use crate::str_store::StrID;

// PubMod is the public interface to a module
pub struct PubMod {
    name: StrID,
    // TODO: need to trim this down to be only public decls. Just keep everything for now.
    node_tree: NodeTree,
}

impl PubMod {
    pub fn new(name: StrID, node_tree: NodeTree) -> Self {
        PubMod { name, node_tree }
    }

    pub fn name(&self) -> StrID {
        self.name
    }
}

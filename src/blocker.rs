use crate::mir::MirModule;
use crate::noder::NodeTree;

pub fn block_hir(node_tree: NodeTree) -> MirModule {
    let _mir = MirModule::new();

    for node in node_tree.roots {}

    todo!("block out the mir module using the hir node tree");
}

use crate::noder::Module;
use crate::str_store::StrID;

// PubMod is the public interface to a module
pub struct PubMod {
    name: StrID,
    module: Module,
}

impl PubMod {
    pub fn new(name: StrID, module: Module) -> Self {
        PubMod { name, module }
    }

    pub fn name(&self) -> StrID {
        self.name
    }
}

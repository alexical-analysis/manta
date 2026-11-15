use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::ast::ast::{self, ImportDecl};
use crate::compiler::context::Context;
use crate::compiler::str_store::MStr;
use crate::hir::hir;
use crate::index_vec::Indexer;
use crate::mir::mir::Func;

#[derive(Debug, Clone)]
pub struct ImportList {
    path: Vec<Module>,
}

impl ImportList {
    fn new(root: Module) -> Self {
        Self { path: vec![root] }
    }

    fn push(&mut self, module: Module) {
        self.path.push(module)
    }

    fn pop(&mut self) {
        self.path.pop();
    }

    fn is_cycle(&self) -> bool {
        let last = match self.path.last() {
            Some(last) => last,
            None => return false,
        };

        for module in &self.path[..self.path.len() - 1] {
            if *module == *last {
                return true;
            }
        }

        return false;
    }
}

pub struct ModuleDag {
    root: Module,
}

impl ModuleDag {
    pub fn new(ctx: &mut Context, modules: &[Module]) -> Self {
        let mut root = None;
        // in pup the root module is always in the main.pup file
        let root_import_path = ctx.get_mstr("main");

        for &module in modules {
            let import_path = ctx.get_module_value(module).import_path;
            if import_path == root_import_path {
                root = Some(module);
                break;
            }
        }

        match root {
            Some(root) => Self { root },
            None => panic!("failed to find root module"),
        }
    }

    // returns any ImportLists that are cyclic
    pub fn validate(&self, ctx: &mut Context) -> Vec<ImportList> {
        let mut import_list = ImportList {
            path: vec![self.root],
        };

        self.find_import_cycles(ctx, &mut import_list)
    }

    fn find_import_cycles(
        &self,
        ctx: &mut Context,
        import_path: &mut ImportList,
    ) -> Vec<ImportList> {
        // check if we're in an import cycle, if we are we can stop recursing and return this as a
        // cyclical path in the module dag
        if import_path.is_cycle() {
            return vec![import_path.clone()];
        }

        let last = *import_path.path.last().expect("import path is empty");

        let mut cycles = vec![];
        let deps = ctx.get_module_value(last).deps.clone();
        for (_, dep) in deps {
            import_path.push(dep);
            let mut new_cycles = self.find_import_cycles(ctx, import_path);
            cycles.append(&mut new_cycles);
            import_path.pop();
        }

        cycles
    }

    pub fn iter(&self, ctx: &mut Context) -> ModuleDagIter {
        let mut stack = vec![self.root];
        self.build_stack(ctx, &mut stack);

        ModuleDagIter { stack }
    }

    fn build_stack(&self, ctx: &mut Context, stack: &mut Vec<Module>) {
        let last = match stack.last() {
            Some(last) => last,
            None => return,
        };

        let deps = ctx.get_module_value(*last).deps.clone();
        for (_, dep) in deps {
            if stack.contains(&dep) {
                continue;
            }

            stack.push(dep);
            self.build_stack(ctx, stack);
        }
    }
}

pub struct ModuleDagIter {
    stack: Vec<Module>,
}

impl<'a> Iterator for ModuleDagIter {
    type Item = Module;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop()
    }
}

pub struct ModuleValue {
    pub name: MStr,
    pub deps: HashMap<MStr, Module>,
    import_path: MStr,
    object_path: PathBuf,
    pub ast: Vec<ast::Decl>,
    pub hir: Vec<hir::Decl>,
    pub funcs: Vec<Func>,
}

impl ModuleValue {
    pub fn new(import_path: MStr) -> Self {
        Self {
            // leave the module name the same as the import path by default
            name: import_path,
            import_path,
            deps: HashMap::new(),
            object_path: PathBuf::from("tmp"),
            ast: vec![],
            hir: vec![],
            funcs: vec![],
        }
    }

    pub fn get_dep(&self, alias: MStr) -> Module {
        *self
            .deps
            .get(&alias)
            .expect("failed to get dependent module")
    }

    pub fn get_import_path(&self) -> MStr {
        self.import_path
    }

    pub fn get_object_path(&self) -> &Path {
        self.object_path.as_path()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module(u32);

impl Indexer for Module {
    fn index(&self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for Module {
    fn from(value: usize) -> Self {
        Module(value as u32)
    }
}

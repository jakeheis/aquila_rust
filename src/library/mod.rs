use crate::parsing::*;

mod metadata;
mod module;
mod node_type;
mod symbol_table;

pub use metadata::{
    FunctionKind, FunctionMetadata, GenericSpecialization, TraitMetadata, TypeMetadata, VarMetadata,
};
pub use module::{Module, ModuleBuilder};
pub use node_type::{FunctionType, NodeType};
pub use symbol_table::{Symbol, SymbolTable, SymbolStore, SymbolProvider};

pub struct Lib {
    pub name: String,
    pub type_decls: Vec<TypeDecl>,
    pub function_decls: Vec<FunctionDecl>,
    pub trait_decls: Vec<TraitDecl>,
    pub conformance_decls: Vec<ConformanceDecl>,
    pub main: Vec<Stmt>,
    pub symbols: SymbolTable,
    pub dependencies: SymbolStore,
}

impl Lib {
    pub fn new(name: &str) -> Self {
        Lib {
            name: String::from(name),
            type_decls: Vec::new(),
            function_decls: Vec::new(),
            trait_decls: Vec::new(),
            conformance_decls: Vec::new(),
            main: Vec::new(),
            symbols: SymbolTable::new(name),
            dependencies: SymbolStore::new(),
        }
    }
}

impl SymbolProvider for Lib {
    fn search<'a, F, U>(&'a self, block: &F) -> Option<&U>
    where
        F: Fn(&'a SymbolTable) -> Option<&'a U> {
            if let Some(found) = block(&self.symbols) {
                Some(found)
            } else {
                self.dependencies.search(block)
            }
    }
}

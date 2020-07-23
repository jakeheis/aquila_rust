use crate::parsing::*;

mod metadata;
mod module;
mod node_type;
mod symbol_table;

pub use metadata::{
    FunctionKind, FunctionMetadata, GenericSpecialization, TraitMetadata, TypeMetadata, VarMetadata
};
pub use module::{Module, ModuleBuilder, Program};
pub use node_type::{FunctionType, NodeType};
pub use symbol_table::{Symbol, SymbolTable, SymbolStore};

pub struct Lib {
    pub name: String,
    pub type_decls: Vec<TypeDecl>,
    pub function_decls: Vec<FunctionDecl>,
    pub trait_decls: Vec<TraitDecl>,
    pub conformance_decls: Vec<ConformanceDecl>,
    pub main: Vec<Stmt>,
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
        }
    }

    pub fn root_sym(&self) -> Symbol {
        Symbol::lib_root(&self.name)
    }
}

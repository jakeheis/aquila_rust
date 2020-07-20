// use crate::analysis::*;
use crate::codegen;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::parsing::*;
use crate::source;
use crate::type_checker::*;
use log::trace;
use std::rc::Rc;

mod metadata;
mod module;
mod node_type;
mod symbol_table;

pub use metadata::{
    FunctionKind, FunctionMetadata, GenericSpecialization, TraitMetadata, TypeMetadata, VarMetadata,
};
pub use module::Module;
pub use node_type::{FunctionType, NodeType};
pub use symbol_table::{Symbol, SymbolTable};

pub struct Lib {
    pub name: String,
    pub type_decls: Vec<TypeDecl>,
    pub function_decls: Vec<FunctionDecl>,
    pub trait_decls: Vec<TraitDecl>,
    pub conformance_decls: Vec<ConformanceDecl>,
    pub main: Vec<Stmt>,
    pub symbols: SymbolTable,
    pub dependencies: Vec<Module>,
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
            symbols: SymbolTable::new(),
            dependencies: Vec::new(),
        }
    }

    pub fn from_source(
        source: Source,
        reporter: Rc<dyn Reporter>,
        link_stdlib: bool,
    ) -> Result<Lib, &'static str> {
        let name = source.short_name().to_string();
        Lib::build_lib(source, &name, link_stdlib, reporter)
    }

    pub fn stdlib(reporter: Rc<dyn Reporter>) -> Lib {
        let src =
            source::file("/Users/jakeheiser/Desktop/Projects/Rust/aquila/src/codegen/stdlib.aq");
        let lib = Lib::build_lib(src, "stdlib", false, reporter)
            .expect("Standard library build should succeed");
        lib
    }

    fn build_lib(
        source: Source,
        name: &str,
        link_stdlib: bool,
        reporter: Rc<dyn Reporter>,
    ) -> Result<Lib, &'static str> {
        let dependencies = if link_stdlib {
            Lib::stdlib(Rc::clone(&reporter)).compile()
        } else {
            vec![]
        };

        let lexer = Lexer::new(source, Rc::clone(&reporter));
        let tokens = lexer.lex();

        let parser = Parser::new(tokens, Rc::clone(&reporter));
        let mut lib = parser.parse(name);

        if reporter.has_errored() {
            return Err("Parsing failed");
        }

        lib.dependencies = dependencies;

        lib = SymbolTableBuilder::build_symbols(lib, Rc::clone(&reporter));

        trace!(target: "symbol_table", "{}", lib.symbols);

        if reporter.has_errored() {
            return Err("Symbol table builder failed");
        }

        let lib = TypeChecker::check(lib, Rc::clone(&reporter));

        if reporter.has_errored() {
            return Err("Type checker failed");
        }

        // CycleChecker::check(&mut lib, Rc::clone(&reporter));
        // if reporter.has_errored() {
        //     return Err("Cycle checker failed");
        // }

        Ok(lib)
    }

    pub fn compile(self) -> Vec<Module> {
        codegen::compile(self)
    }

    fn deep_search<'a, F, U>(&'a self, search: &F) -> Option<&U>
    where
        F: Fn(&str, &'a SymbolTable) -> Option<&'a U>,
    {
        if let Some(found) = search(&self.name, &self.symbols) {
            Some(found)
        } else {
            for dep in &self.dependencies {
                if let Some(found) = search(&dep.name, &dep.symbols) {
                    return Some(found);
                }
            }
            None
        }
    }

    pub fn type_metadata(&self, symbol: &Symbol) -> Option<&TypeMetadata> {
        self.deep_search(&|_name, sym| sym.get_type_metadata(symbol))
    }

    pub fn type_metadata_named(&self, name: &str) -> Option<&TypeMetadata> {
        self.deep_search(&|lib_name, symbols| {
            let type_symbol = Symbol::lib_root(lib_name).child(name);
            symbols.get_type_metadata(&type_symbol)
        })
    }

    pub fn top_level_function_named(&self, name: &str) -> Option<&FunctionMetadata> {
        self.deep_search(&|lib_name, symbols| {
            let func_symbol = Symbol::lib_root(lib_name).child(name);
            symbols.get_func_metadata(&func_symbol)
        })
    }

    pub fn function_metadata(&self, symbol: &Symbol) -> Option<&FunctionMetadata> {
        self.deep_search(&|_name, symbols| symbols.get_func_metadata(symbol))
    }

    pub fn trait_metadata(&self, name: &str) -> Option<&TraitMetadata> {
        self.deep_search(&|lib_name, symbols| {
            let trait_symbol = Symbol::lib_root(lib_name).child(name);
            symbols.get_trait_metadata(&trait_symbol)
        })
    }

    pub fn trait_metadata_symbol(&self, name: &Symbol) -> Option<&TraitMetadata> {
        self.deep_search(&|_name, symbols| symbols.get_trait_metadata(name))
    }

    pub fn symbol_span(&self, symbol: &Symbol) -> Option<&Span> {
        self.deep_search(&|_name, symbols| symbols.get_span(symbol))
    }
}

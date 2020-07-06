use crate::analysis::*;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::parsing::*;
use crate::source;
use crate::type_checker::*;
use crate::codegen;
use log::trace;
use std::rc::Rc;

mod metadata;
mod node_type;
mod symbol_table;
mod module;

pub use metadata::{
    FunctionKind, FunctionMetadata, GenericSpecialization, TraitMetadata, TypeMetadata,
};
pub use node_type::{FunctionType, NodeType};
pub use symbol_table::{Symbol, SymbolTable};
pub use module::Module;

pub struct Lib {
    pub name: String,
    pub type_decls: Vec<TypeDecl>,
    pub function_decls: Vec<FunctionDecl>,
    pub trait_decls: Vec<TraitDecl>,
    pub conformance_decls: Vec<ConformanceDecl>,
    pub main: Vec<Stmt>,
    pub symbols: SymbolTable,
    pub dependencies: Vec<Module>,
    pub specialization_tracker: SpecializationTracker,
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
            specialization_tracker: SpecializationTracker::new(),
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

        let mut lib = TypeChecker::check(lib, Rc::clone(&reporter));

        if reporter.has_errored() {
            return Err("Type checker failed");
        }

        CycleChecker::check(&mut lib, Rc::clone(&reporter));

        if reporter.has_errored() {
            return Err("Cycle checker failed");
        }

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
            let type_symbol = Symbol::new_str(&Symbol::root(lib_name), name);
            symbols.get_type_metadata(&type_symbol)
        })
    }

    pub fn top_level_function_named(&self, name: &str) -> Option<&FunctionMetadata> {
        self.deep_search(&|lib_name, symbols| {
            let func_symbol = Symbol::new_str(&Symbol::root(lib_name), name);
            symbols.get_func_metadata(&func_symbol)
        })
    }

    pub fn function_metadata(&self, symbol: &Symbol) -> Option<&FunctionMetadata> {
        self.deep_search(&|_name, symbols| symbols.get_func_metadata(symbol))
    }

    pub fn trait_metadata(&self, name: &str) -> Option<&TraitMetadata> {
        self.deep_search(&|lib_name, symbols| {
            let trait_symbol = Symbol::new_str(&Symbol::root(lib_name), name);
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

use std::collections::HashMap;
use std::cell::RefCell;

pub type SpecializationTrackerMap = HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>;

#[derive(Clone, Debug)]
pub struct SpecializationTracker {
    pub call_map: RefCell<SpecializationTrackerMap>,
    pub explicit_type_map: RefCell<SpecializationTrackerMap>,
}

impl SpecializationTracker {
    pub fn new() -> Self {
        SpecializationTracker {
            call_map: RefCell::new(HashMap::new()),
            explicit_type_map: RefCell::new(HashMap::new()),
        }
    }

    pub fn add_call(&self, from: Symbol, to: Symbol, with: GenericSpecialization) {
        self.call_map
            .borrow_mut()
            .entry(from)
            .or_insert(Vec::new())
            .push((to, with));
    }

    pub fn add_required_type_spec(
        &self,
        in_func: Symbol,
        type_symbol: Symbol,
        spec: GenericSpecialization,
    ) {
        self.explicit_type_map
            .borrow_mut()
            .entry(in_func)
            .or_insert(Vec::new())
            .push((type_symbol, spec));
    }
}

impl std::fmt::Display for SpecializationTracker {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let call_map = self.call_map.borrow();
        for (caller, calls) in call_map.iter() {
            write!(f, "\nCalls for {}:", caller.id)?;
            for (call, spec) in calls {
                write!(f, "\n  {} -- {}", call, spec)?;
            }
        }

        let explicit_type_specializations = self.explicit_type_map.borrow();
        for (caller, explicit_types) in explicit_type_specializations.iter() {
            write!(f, "\nExplicit types for {}:", caller.id)?;
            for (et, spec) in explicit_types {
                write!(f, "\n  {} -- {}", et, spec)?;
            }
        }

        Ok(())
    }
}

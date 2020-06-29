use crate::analysis::*;
use crate::codegen::core;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::parsing::*;
use crate::source;
use crate::type_checker::*;
use log::trace;
use std::rc::Rc;

mod metadata;
mod node_type;
mod symbol_table;

pub use metadata::{FunctionKind, FunctionMetadata, TraitMetadata, GenericSpecialization, TypeMetadata};
pub use node_type::{FunctionType, NodeType};
pub use symbol_table::{Symbol, SymbolTable};

pub struct Lib {
    pub name: String,
    pub type_decls: Vec<TypeDecl>,
    pub function_decls: Vec<FunctionDecl>,
    pub trait_decls: Vec<TraitDecl>,
    pub conformance_decls: Vec<ConformanceDecl>,
    pub builtins: Vec<FunctionDecl>,
    pub other: Vec<Stmt>,
    pub symbols: SymbolTable,
    pub dependencies: Vec<Lib>,
    pub specialization_tracker: SpecializationTracker,
}

impl Lib {
    pub fn from_source(source: Source, reporter: Rc<dyn Reporter>) -> Result<Lib, &'static str> {
        let name = source.name().to_string();
        Lib::build_lib(source, &name, true, reporter)
    }

    pub fn stdlib(reporter: Rc<dyn Reporter>) -> Lib {
        let src =
            source::file("/Users/jakeheiser/Desktop/Projects/Rust/aquila/src/library/stdlib.aq");
        let lib = Lib::build_lib(src, "stdlib", false, reporter).expect("Standard library build should succeed");
        core::add_builtin_symbols(&lib);
        lib
    }

    fn build_lib(
        source: Source,
        name: &str,
        link_stdlib: bool,
        reporter: Rc<dyn Reporter>,
    ) -> Result<Lib, &'static str> {
        let dependencies = if link_stdlib {
            vec![Lib::stdlib(Rc::clone(&reporter))]
        } else {
            vec![]
        };

        let lexer = Lexer::new(source, Rc::clone(&reporter));
        let tokens = lexer.lex();

        let parser = Parser::new(tokens, Rc::clone(&reporter));
        let stmts = parser.parse();
        
        ASTPrinter::trace().print(&stmts);

        if reporter.has_errored() {
            return Err("Parsing failed");
        }

        let (type_decls, function_decls, trait_decls, conformance_decls, builtins, other) = Lib::organize_stms(stmts);

        let mut lib = Lib {
            name: String::from(name),
            type_decls,
            function_decls,
            trait_decls,
            conformance_decls,
            builtins,
            symbols: SymbolTable::new(),
            other,
            dependencies,
            specialization_tracker: SpecializationTracker::new(),
        };

        lib = SymbolTableBuilder::build_symbols(
            lib,
            Rc::clone(&reporter),
        );

        trace!(target: "symbol_table", "{}", lib.symbols);

        let mut lib = TypeChecker::check(lib, Rc::clone(&reporter));

        if reporter.has_errored() {
            return Err("Type checker failed");
        }

        CycleChecker::check(&mut lib, Rc::clone(&reporter));

        if reporter.has_errored() {
            return Err("Cycle checker failed");
        }

        let lib = Rewriter::rewrite(lib);

        Ok(lib)
    }

    pub fn deep_search<F, U>(&self, search: &F) -> Option<U> 
    where F: Fn(&Lib) -> Option<U>
    {
        if let Some(found) = search(self) {
            Some(found)
        }else {
            for dep in &self.dependencies {
                if let Some(found) = dep.deep_search(search) {
                    return Some(found);
                }
            }
            None
        }
    }

    pub fn type_metadata(&self, symbol: &Symbol) -> Option<TypeMetadata> {
        if let Some(found) = self.symbols.get_type_metadata(symbol) {
            Some(found.clone())
        } else {
            for dep in &self.dependencies {
                if let Some(found) = dep.type_metadata(symbol) {
                    return Some(found);
                }
            }
            None
        }
    }

    pub fn type_metadata_ref(&self, symbol: &Symbol) -> Option<&TypeMetadata> {
        if let Some(found) = self.symbols.get_type_metadata(symbol) {
            Some(found)
        } else {
            for dep in &self.dependencies {
                if let Some(found) = dep.type_metadata_ref(symbol) {
                    return Some(found);
                }
            }
            None
        }
    }

    pub fn type_metadata_mut(&mut self, symbol: &Symbol) -> Option<&mut TypeMetadata> {
        if let Some(found) = self.symbols.get_type_metadata_mut(symbol) {
            Some(found)
        } else {
            for dep in &mut self.dependencies {
                if let Some(found) = dep.type_metadata_mut(symbol) {
                    return Some(found);
                }
            }
            None
        }
    }

    pub fn top_level_function_named(&self, name: &str) -> Option<&FunctionMetadata> {
        let func_symbol = Symbol::new_str(&Symbol::lib_root(self), name);
        if let Some(found) = self.symbols.get_func_metadata(&func_symbol) {
            return Some(found);
        }

        for dep in &self.dependencies {
            if let Some(found) = dep.top_level_function_named(name) {
                return Some(found);
            }
        }
        None
    }

    pub fn function_metadata(&self, symbol: &Symbol) -> Option<FunctionMetadata> {
        if let Some(found) = self.symbols.get_func_metadata(symbol) {
            Some(found.clone())
        } else {
            for dep in &self.dependencies {
                if let Some(found) = dep.function_metadata(symbol) {
                    return Some(found);
                }
            }
            None
        }
    }

    pub fn function_metadata_mut(&mut self, symbol: &Symbol) -> Option<&mut FunctionMetadata> {
        if let Some(found) = self.symbols.get_func_metadata_mut(symbol) {
            Some(found)
        } else {
            for dep in &mut self.dependencies {
                if let Some(found) = dep.function_metadata_mut(symbol) {
                    return Some(found);
                }
            }
            None
        }
    }

    pub fn trait_metadata(&self, name: &str) -> Option<TraitMetadata> {
        let my_symbol = Symbol::new_str(&Symbol::lib_root(self), name);
        if let Some(found) = self.symbols.get_trait_metadata(&my_symbol) {
            Some(found.clone())
        } else {
            for dep in &self.dependencies {
                if let Some(found) = dep.trait_metadata(name) {
                    return Some(found);
                }
            }
            None
        }
    }

    pub fn trait_metadata_symbol(&self, name: &Symbol) -> Option<TraitMetadata> {
        if let Some(found) = self.symbols.get_trait_metadata(name) {
            Some(found.clone())
        } else {
            for dep in &self.dependencies {
                if let Some(found) = dep.trait_metadata_symbol(name) {
                    return Some(found);
                }
            }
            None
        }
    }

    pub fn symbol_span(&self, symbol: &Symbol) -> Option<Span> {
        if let Some(found) = self.symbols.span_map.get(symbol) {
            Some(found.clone())
        } else {
            for dep in &self.dependencies {
                if let Some(found) = dep.symbol_span(symbol) {
                    return Some(found);
                }
            }
            None
        }
    }

    fn organize_stms(
        stmts: Vec<Stmt>,
    ) -> (
        Vec<TypeDecl>,
        Vec<FunctionDecl>,
        Vec<TraitDecl>,
        Vec<ConformanceDecl>,
        Vec<FunctionDecl>,
        Vec<Stmt>,
    ) {
        let mut type_decls: Vec<TypeDecl> = Vec::new();
        let mut function_decls: Vec<FunctionDecl> = Vec::new();
        let mut trait_decls: Vec<TraitDecl> = Vec::new();
        let mut conformance_decls: Vec<ConformanceDecl> = Vec::new();
        let mut builtins: Vec<FunctionDecl> = Vec::new();
        let mut other: Vec<Stmt> = Vec::new();

        for stmt in stmts {
            match &stmt.kind {
                StmtKind::TypeDecl(..) => {
                    if let StmtKind::TypeDecl(decl) = stmt.kind {
                        type_decls.push(decl);
                    }
                }
                StmtKind::FunctionDecl(..) => {
                    if let StmtKind::FunctionDecl(decl) = stmt.kind {
                        function_decls.push(decl);
                    }
                }
                StmtKind::TraitDecl(..) => {
                    if let StmtKind::TraitDecl(decl) = stmt.kind {
                        trait_decls.push(decl);
                    }
                }
                StmtKind::ConformanceDecl(..) => {
                    if let StmtKind::ConformanceDecl(decl) = stmt.kind {
                        conformance_decls.push(decl);
                    }
                }
                StmtKind::Builtin(..) => {
                    if let StmtKind::Builtin(inner) = stmt.kind {
                        match inner.kind {
                            StmtKind::FunctionDecl(decl) => builtins.push(decl),
                            _ => panic!("Can only have builtin functions (right now)"),
                        }
                    }
                }
                _ => other.push(stmt),
            }
        }
        (type_decls, function_decls, trait_decls, conformance_decls, builtins, other)
    }
}

use super::{SymbolTable, SymbolStore, Symbol};
use crate::codegen::*;
use crate::diagnostic::*;
use crate::source;
use crate::parsing::*;
use crate::lexing::Lexer;
use crate::type_checker::{TypeChecker, SymbolTableBuilder};
use crate::analysis::CycleChecker;
use std::rc::Rc;
use log::trace;

pub struct ParsedModule {
    pub name: String,
    pub type_decls: Vec<TypeDecl>,
    pub function_decls: Vec<FunctionDecl>,
    pub trait_decls: Vec<TraitDecl>,
    pub conformance_decls: Vec<ConformanceDecl>,
    pub main: Vec<Stmt>,
}

impl ParsedModule {
    pub fn new(name: &str) -> Self {
        ParsedModule {
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

pub struct Module {
    pub name: String,
    pub structures: Vec<IRStructure>,
    pub functions: Vec<IRFunction>,
    pub symbols: Rc<SymbolTable>,
    pub specialization_record: SpecializationRecord,
}

impl Module {
    pub fn dump(&self) {
        println!("Structures:");
        for structure in &self.structures {
            println!("  {}", structure.name);
            for field in &structure.fields {
                println!("    {}: {}", field.name, field.var_type);
            }
        }

        println!("Functions:");
        for func in &self.functions {
            println!("  {}", func.name);
            for param in &func.parameters {
                println!("    param: {}: {}", param.name, param.var_type);
            }
            println!("    ret: {}", func.return_type);
        }
        self.specialization_record.dump();
    }
}

pub struct ModuleBuilder {
    modules: Vec<Module>,
    symbol_store: SymbolStore,
    reporter: Rc<dyn Reporter>,
}

impl ModuleBuilder {
    pub fn new(reporter: Rc<dyn Reporter>) -> Self {
        Self {
            modules: vec![],
            symbol_store: SymbolStore::new(),
            reporter,
        }
    }

    pub fn build_src(&mut self, source: Source) -> Result<Symbol, &'static str> {
        let module_name = source.short_name().to_owned();

        let lexer = Lexer::new(source, Rc::clone(&self.reporter));
        let tokens = lexer.lex();

        let parser = Parser::new(tokens, Rc::clone(&self.reporter));
        let mut module = parser.parse(&module_name);

        if self.reporter.has_errored() {
            return Err("Parsing failed");
        }

        let symbols = SymbolTableBuilder::build_symbols(&module, &self.symbol_store, Rc::clone(&self.reporter));
        let symbols = Rc::new(symbols);
        self.symbol_store.add_source(Rc::clone(&symbols));

        trace!(target: "symbol_table", "{}", symbols);

        if self.reporter.has_errored() {
            return Err("Symbol table builder failed");
        }

        TypeChecker::check(&module, self.symbol_store.clone(), Rc::clone(&symbols), Rc::clone(&self.reporter));

        if self.reporter.has_errored() {
            return Err("Type checker failed");
        }

        CycleChecker::check(&mut module, symbols.as_ref(), Rc::clone(&self.reporter));
        if self.reporter.has_errored() {
            return Err("Cycle checker failed");
        }

        let module = IRGen::new(self.symbol_store.clone()).generate(module, symbols);
        
        self.modules.push(module);
        
        Ok(Symbol::lib_root(&module_name))
    }

    pub fn build_stdlib(&mut self) {
        let src = source::file("/Users/jakeheiser/Desktop/Projects/Rust/aquila/src/codegen/stdlib.aq");
        self.build_src(src).expect("Standard library build should succeed");
    }

    pub fn take_program(self) -> Program {
        let ModuleBuilder { modules,  symbol_store, .. } = self;
        Program {
            modules,
            symbols: symbol_store
        }
    }
}

pub struct Program {
    pub modules: Vec<Module>,
    pub symbols: SymbolStore
}

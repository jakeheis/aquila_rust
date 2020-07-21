use super::{SymbolTable, SymbolStore, Symbol};
use crate::codegen::*;
use crate::diagnostic::*;
use crate::source;
use crate::parsing::Parser;
use crate::lexing::Lexer;
use crate::type_checker::{TypeChecker, SymbolTableBuilder};
use std::rc::Rc;
use log::trace;

#[derive(Debug)]
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
        let mut lib = parser.parse(&module_name);

        if self.reporter.has_errored() {
            return Err("Parsing failed");
        }

        lib.dependencies = self.symbol_store.clone();

        lib = SymbolTableBuilder::build_symbols(lib, Rc::clone(&self.reporter));

        trace!(target: "symbol_table", "{}", lib.symbols);

        if self.reporter.has_errored() {
            return Err("Symbol table builder failed");
        }

        let lib = TypeChecker::check(lib, Rc::clone(&self.reporter));

        if self.reporter.has_errored() {
            return Err("Type checker failed");
        }

        // CycleChecker::check(&mut lib, Rc::clone(&reporter));
        // if reporter.has_errored() {
        //     return Err("Cycle checker failed");
        // }

        let module = IRGen::new(lib).generate();
        
        self.symbol_store.add_source(Rc::clone(&module.symbols));
        self.modules.push(module);
        
        Ok(Symbol::lib_root(&module_name))
    }

    pub fn build_stdlib(&mut self) {
        let src = source::file("/Users/jakeheiser/Desktop/Projects/Rust/aquila/src/codegen/stdlib.aq");
        self.build_src(src).expect("Standard library build should succeed");
    }

    pub fn take_modules(self) -> Vec<Module> {
        self.modules
    }
}

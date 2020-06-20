use crate::analysis::*;
use crate::codegen::core;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::parsing::*;
use crate::source::*;
use crate::source::{self, Source};
use std::rc::Rc;

pub struct Lib {
    pub name: String,
    pub type_decls: Vec<TypeDecl>,
    pub function_decls: Vec<FunctionDecl>,
    pub builtins: Vec<FunctionDecl>,
    pub other: Vec<Stmt>,
    pub symbols: SymbolTable,
    pub dependencies: Vec<Lib>,
}

const LOG_LEXER: bool = false;
const LOG_PARSER: bool = false;
const LOG_SYMBOL_MAKER: bool = false;
const LOG_TYPE_CHECKER: bool = false;
const LOG_STDLIB: bool = false;

impl Lib {
    pub fn from_source(source: Source) -> Result<Lib, &'static str> {
        let name = source.name().to_string();
        Lib::build_lib(source, &name, true)
    }

    pub fn stdlib() -> Lib {
        let src =
            source::file("/Users/jakeheiser/Desktop/Projects/Rust/aquila/src/library/stdlib.aq");
        let lib = Lib::build_lib(src, "stdlib", false).unwrap();
        core::add_builtin_symbols(&lib);
        if LOG_STDLIB {
            println!("std {}", lib.symbols);
        }
        lib
    }

    fn build_lib(source: Source, name: &str, link_stdlib: bool) -> Result<Lib, &'static str> {
        let reporter: Rc<dyn Reporter> = DefaultReporter::new();

        let dependencies = if link_stdlib {
            vec![Lib::stdlib()]
        } else {
            vec![]
        };

        let lexer = Lexer::new(source, Rc::clone(&reporter));
        let tokens = lexer.lex();

        if LOG_LEXER {
            let slice: &[Token] = &tokens;
            println!("lexed {}", slice.token_string());
        }

        let parser = Parser::new(tokens, Rc::clone(&reporter));
        let stmts = parser.parse();

        if LOG_PARSER {
            let mut printer = ASTPrinter::new();
            printer.print(&stmts);
        }

        if reporter.has_errored() {
            return Err("Parsing failed");
        }

        let (type_decls, function_decls, builtins, other) = Lib::organize_stms(stmts);

        let mut lib = Lib {
            name: String::from(name),
            type_decls,
            function_decls,
            builtins,
            symbols: SymbolTable::new(),
            other,
            dependencies,
        };

        lib.symbols = SymbolTableBuilder::build_symbols(
            &lib.type_decls,
            &lib.function_decls,
            &lib.builtins,
            &lib.dependencies,
        );

        if LOG_SYMBOL_MAKER {
            println!("{}", lib.symbols);
        }

        let mut lib = TypeChecker::check(lib, Rc::clone(&reporter));

        if LOG_TYPE_CHECKER {
            let mut printer = ASTPrinter::new();
            for decl in &lib.type_decls {
                printer.visit_type_decl(decl);
            }
            for decl in &lib.function_decls {
                printer.visit_function_decl(decl);
            }
            printer.print(&lib.other);
        }

        if reporter.has_errored() {
            return Err("Type checker failed");
        }

        CycleChecker::check(&mut lib, Rc::clone(&reporter));

        if reporter.has_errored() {
            return Err("Cycle checker failed");
        }

        Ok(lib)
    }

    pub fn resolve_symbol(&self, symbol: &Symbol) -> Option<NodeType> {
        if let Some(found) = self.symbols.get_type(symbol) {
            Some(found.clone())
        } else {
            for dep in &self.dependencies {
                if let Some(found) = dep.resolve_symbol(symbol) {
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
        Vec<FunctionDecl>,
        Vec<Stmt>,
    ) {
        let mut type_decls: Vec<TypeDecl> = Vec::new();
        let mut function_decls: Vec<FunctionDecl> = Vec::new();
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
        (type_decls, function_decls, builtins, other)
    }
}

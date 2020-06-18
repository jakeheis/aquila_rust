use crate::analysis::*;
use crate::codegen::core;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::parsing::*;
use crate::source::{self, Source};
use std::cell::RefCell;
use std::rc::Rc;
use crate::source::*;

pub struct Lib {
    pub name: String,
    pub function_decls: Vec<Stmt>,
    pub type_decls: Vec<Stmt>,
    pub other: Vec<Stmt>,
    pub symbols: RefCell<SymbolTable>,
    pub dependencies: Vec<Rc<Lib>>,
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
            println!("std {}", lib.symbols.borrow());
        }
        lib
    }

    fn build_lib(source: Source, name: &str, link_stdlib: bool) -> Result<Lib, &'static str> {
        let reporter: Rc<dyn Reporter> = DefaultReporter::new();

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

        let (type_decls, function_decls, other) = Lib::organize_stms(stmts);

        let dependencies = if link_stdlib {
            vec![Rc::new(Lib::stdlib())]
        } else {
            vec![]
        };
        let lib = IncompleteLib {
            name: String::from(name),
            type_decls,
            function_decls,
            other,
            dependencies,
        };

        let lib = SymbolTableBuilder::build_symbols(lib);

        if LOG_SYMBOL_MAKER {
            println!("{}", lib.symbols.borrow());
        }

        let lib = TypeChecker::check(lib, Rc::clone(&reporter));

        if LOG_TYPE_CHECKER {
            let mut printer = ASTPrinter::new();
            printer.print(&lib.type_decls);
            printer.print(&lib.function_decls);
            printer.print(&lib.other);
        }

        if reporter.has_errored() {
            return Err("Type checker failed");
        }

        let lib = CycleChecker::check(lib, Rc::clone(&reporter));

        if reporter.has_errored() {
            return Err("Cycle checker failed");
        }

        Ok(lib)
    }

    pub fn resolve_symbol(&self, symbol: &Symbol) -> Option<NodeType> {
        if let Some(found) = self.symbols.borrow().get_type(symbol) {
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
        if let Some(found) = self.symbols.borrow().get_type_metadata(symbol) {
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
        if let Some(found) = self.symbols.borrow().get_func_metadata(symbol) {
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

    pub fn symbol_span(&self, symbol: &Symbol) -> Option<Span> {
        if let Some(found) = self.symbols.borrow().span_map.get(symbol) {
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

    fn organize_stms(stmts: Vec<Stmt>) -> (Vec<Stmt>, Vec<Stmt>, Vec<Stmt>) {
        let mut type_decls: Vec<Stmt> = Vec::new();
        let mut function_decls: Vec<Stmt> = Vec::new();
        let mut other: Vec<Stmt> = Vec::new();
        for stmt in stmts {
            match &stmt.kind {
                StmtKind::TypeDecl(..) => type_decls.push(stmt),
                StmtKind::FunctionDecl(..) => function_decls.push(stmt),
                StmtKind::Builtin(builtin) => match &builtin.kind {
                    StmtKind::TypeDecl(..) => type_decls.push(stmt),
                    StmtKind::FunctionDecl(..) => function_decls.push(stmt),
                    _ => other.push(stmt),
                },
                _ => other.push(stmt),
            }
        }
        (type_decls, function_decls, other)
    }
}

// IncompleteLib

pub struct IncompleteLib {
    pub name: String,
    pub function_decls: Vec<Stmt>,
    pub type_decls: Vec<Stmt>,
    pub other: Vec<Stmt>,
    pub dependencies: Vec<Rc<Lib>>,
}

impl IncompleteLib {
    pub fn add_symbols(self, symbols: SymbolTable) -> Lib {
        Lib {
            name: self.name,
            function_decls: self.function_decls,
            type_decls: self.type_decls,
            other: self.other,
            symbols: RefCell::new(symbols),
            dependencies: self.dependencies,
        }
    }

    pub fn resolve_symbol(&self, symbol: &Symbol) -> Option<NodeType> {
        for dep in &self.dependencies {
            if let Some(found) = dep.resolve_symbol(symbol) {
                return Some(found);
            }
        }
        None
    }
}

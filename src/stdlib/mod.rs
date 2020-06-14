use crate::analysis::*;
use crate::lexing::*;
use crate::parsing::*;
use crate::source::{self, Source};
use crate::diagnostic::*;
use std::rc::Rc;

pub struct Lib {
    pub name: String,
    pub function_decls: Vec<Stmt>,
    pub type_decls: Vec<Stmt>,
    pub other: Vec<Stmt>,
    pub symbols: SymbolTable,
    pub dependencies: Vec<Rc<Lib>>,
}

pub struct LogOptions {
    lexer: bool,
    parser: bool,
    symbol_maker: bool,
    type_checker: bool,
}

impl Lib {

    pub fn from_source(source: Source) -> Result<Lib, &'static str> {
        let name = source.name().to_string();
        Lib::build_lib(source, &name, true)
    }
    
    pub fn stdlib() -> Lib {
        let src = source::file("/Users/jakeheiser/Desktop/Projects/Rust/aquila/src/stdlib/stdlib.aq");
        Lib::build_lib(src, "stdlib", false).unwrap()
    }

    fn build_lib(source: Source, name: &str, link_stdlib: bool) -> Result<Lib, &'static str> {
        let log_options = LogOptions {
            lexer: false,
            parser: false,
            symbol_maker: true,
            type_checker: true,
        };
    
        let reporter: Rc<dyn Reporter> = DefaultReporter::new();
    
        let lexer = Lexer::new(source, Rc::clone(&reporter));
        let tokens = lexer.lex();
    
        if log_options.lexer {
            let slice: &[Token] = &tokens;
            println!("lexed {}", slice.token_string());
        }
    
        let parser = Parser::new(tokens, Rc::clone(&reporter));
        let stmts = parser.parse();
    
        if log_options.parser {
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

        if log_options.symbol_maker {
            println!("Table: {}", lib.symbols);
        }
    
        TypeChecker::check(Rc::clone(&lib), Rc::clone(&reporter));
    
        if log_options.type_checker {    
            let mut printer = ASTPrinter::new();
            printer.print(&lib.type_decls);
            printer.print(&lib.function_decls);
            printer.print(&lib.other);
        }
    
        if reporter.has_errored() {
            return Err("Type checker failed");
        }
    
        let mut lib = Rc::try_unwrap(lib).ok().unwrap();
        CycleChecker::check(&mut lib, Rc::clone(&reporter));

        if reporter.has_errored() {
            return Err("Cycle checker failed");
        }
    
        Ok(lib)
    }

    pub fn resolve_symbol(&self, symbol: &Symbol) -> Option<&NodeType> {
        if let Some(found) = self.symbols.get_type(symbol) {
            Some(found)
        } else {
            for dep in &self.dependencies {
                if let Some(found) = dep.resolve_symbol(symbol) {
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
                StmtKind::Builtin(builtin) => {
                    match &builtin.kind {
                        StmtKind::TypeDecl(..) => type_decls.push(stmt),
                        StmtKind::FunctionDecl(..) => function_decls.push(stmt),
                        _ => other.push(stmt),
                    }
                }
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
            symbols,
            dependencies: self.dependencies,
        }
    }

    pub fn resolve_symbol(&self, symbol: &Symbol) -> Option<&NodeType> {
        for dep in &self.dependencies {
            if let Some(found) = dep.resolve_symbol(symbol) {
                return Some(found);
            }
        }
        None
    }

}
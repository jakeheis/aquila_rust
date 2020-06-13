use crate::analysis::*;
// use crate::lexer::*;
use crate::parsing::*;
use crate::source;

pub struct Stdlib {
    pub function_decls: Vec<Stmt>,
    pub type_decls: Vec<Stmt>,
    pub symbols: SymbolTable
}

impl Stdlib {
    pub fn load() -> Stdlib {
        let src = source::file("/Users/jakeheiser/Desktop/Projects/Rust/aquila/src/stdlib/stdlib.aq");
        let (code, symbols) = crate::build_program(src, false).unwrap();
        Stdlib {
            function_decls: code.function_decls,
            type_decls: code.type_decls,
            symbols,
        }
    }
}

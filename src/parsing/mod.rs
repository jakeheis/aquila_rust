mod ast;
mod parser;

pub use ast::{ASTPrinter, Expr, ExprKind, ExprVisitor, Stmt, StmtKind, StmtVisitor};
pub use parser::Parser;

pub struct ParsedProgram {
    pub source: crate::source::Source,
    pub type_decls: Vec<Stmt>,
    pub function_decls: Vec<Stmt>,
    pub main: Vec<Stmt>,
}

impl ParsedProgram {
    pub fn new(source: crate::source::Source, stmts: Vec<Stmt>) -> Self {
        let mut type_decls: Vec<Stmt> = Vec::new();
        let mut function_decls: Vec<Stmt> = Vec::new();
        let mut other: Vec<Stmt> = Vec::new();
        for stmt in stmts {
            match stmt.kind {
                StmtKind::TypeDecl(..) => type_decls.push(stmt),
                StmtKind::FunctionDecl(..) => function_decls.push(stmt),
                _ => other.push(stmt),
            }
        }
        ParsedProgram {
            source,
            type_decls,
            function_decls,
            main: other,
        }
    }
}

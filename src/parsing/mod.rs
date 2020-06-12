mod ast;
mod parser;

pub use ast::{ASTPrinter, Expr, ExprKind, ExprVisitor, Stmt, StmtKind, StmtVisitor};
pub use parser::Parser;

pub struct ParsedProgram {
    pub source: crate::source::Source,
    pub statements: Vec<Stmt>,
}

impl ParsedProgram {

    pub fn organized(&self) -> (Vec<&Stmt>, Vec<&Stmt>, Vec<&Stmt>) {
        let mut type_decls: Vec<&Stmt> = Vec::new();
        let mut function_decls: Vec<&Stmt> = Vec::new();
        let mut other: Vec<&Stmt> = Vec::new();
        for stmt in &self.statements {
            match stmt.kind {
                StmtKind::TypeDecl(..) => type_decls.push(stmt),
                StmtKind::FunctionDecl(..) => function_decls.push(stmt),
                _ => other.push(stmt),
            }
        }
        (type_decls, function_decls, other)
    }

}

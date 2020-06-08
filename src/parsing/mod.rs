mod ast;
mod parser;

pub use ast::{ASTPrinter, Expr, ExprKind, ExprVisitor, Stmt, StmtKind, StmtVisitor};
pub use parser::Parser;

pub struct ParsedProgram {
    pub source: crate::source::Source,
    pub statements: Vec<Stmt>,
}

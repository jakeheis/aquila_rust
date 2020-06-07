use crate::lexing::Token;
use crate::parsing::Stmt;
use crate::source::Source;

pub struct LexedProgram {
    pub source: Source,
    pub tokens: Vec<Token>,
}

pub struct ParsedProgram {
    pub source: Source,
    pub statements: Vec<Stmt>,
}

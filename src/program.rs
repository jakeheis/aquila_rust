use crate::source::Source;
use crate::lexing::Token;
use crate::parsing::Stmt;

pub struct LexedProgram {
    pub source: Source,
    pub tokens: Vec<Token>
}

pub struct ParsedProgram {
    pub source: Source,
    pub statements: Vec<Stmt>
}

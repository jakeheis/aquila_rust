pub mod lexer;
pub mod token;

pub use lexer::Lexer;
pub use token::{Token, TokenKind, TokenString};

pub struct LexedProgram {
    pub source: crate::source::Source,
    pub tokens: Vec<Token>,
}

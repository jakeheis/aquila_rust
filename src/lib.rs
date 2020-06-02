pub mod lexer;
pub mod parsing;
pub mod source;

use parsing::parser;
pub use source::Source;
use crate::lexer::TokenString;

pub fn run(source: Source) {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.lex();

    println!("{}", tokens.token_string());

    let mut parser = parser::Parser::new(tokens);
    parser.parse();
}

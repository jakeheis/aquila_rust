pub mod lexer;
pub mod parsing;
pub mod source;

use crate::lexer::TokenString;
use parsing::Parser;
pub use source::Source;

pub fn run(source: Source) {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.lex();

    println!("{}", tokens.token_string());

    let mut parser = Parser::new(tokens);
    parser.parse();
}

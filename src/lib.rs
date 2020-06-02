pub mod lexing;
pub mod parsing;
pub mod source;

use lexing::*;
use parsing::*;
pub use source::*;
use std::rc::Rc;

pub fn run(source: Rc<Source>) {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex();

    println!("{}", tokens.token_string());

    let mut parser = Parser::new(tokens);
    parser.parse();
}

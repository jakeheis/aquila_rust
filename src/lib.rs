pub mod lexing;
pub mod parsing;
pub mod source;

pub use source::*;
use parsing::*;
use lexing::*;
use std::rc::Rc;

pub fn run(source: Rc<Source>) {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex();

    println!("{}", tokens.token_string());

    let mut parser = Parser::new(tokens);
    parser.parse();
}

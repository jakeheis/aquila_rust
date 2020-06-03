pub mod diagnostic;
pub mod lexing;
pub mod parsing;
pub mod source;

use lexing::*;
use parsing::*;
pub use source::*;
use std::rc::Rc;
use diagnostic::*;

pub fn run(source: Source) {
    let reporter: Rc<dyn Reporter> = diagnostic::DefaultReporter::new();

    let mut lexer = Lexer::new(source, Rc::clone(&reporter));
    let tokens = lexer.lex();

    println!("{}", tokens.token_string());

    let mut parser = Parser::new(tokens, reporter);
    parser.parse();
}

pub mod lexing;
pub mod parsing;
pub mod source;
pub mod diagnostic;

use lexing::*;
use parsing::*;
pub use source::*;

pub fn run(source: Source) {
    let reporter = diagnostic::DefaultReporter::new();

    let mut lexer = Lexer::new(source, reporter);
    let tokens = lexer.lex();
    
    println!("{}", tokens.token_string());

    let mut parser = Parser::new(tokens);
    parser.parse();
}

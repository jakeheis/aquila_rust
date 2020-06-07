pub mod analysis;
pub mod diagnostic;
pub mod lexing;
pub mod parsing;
pub mod program;
pub mod source;

use analysis::*;
use diagnostic::*;
use lexing::*;
use parsing::*;
pub use source::*;
use std::rc::Rc;

pub fn run(source: Source) {
    let reporter: Rc<dyn Reporter> = diagnostic::DefaultReporter::new();

    let lexer = Lexer::new(source, Rc::clone(&reporter));
    let lexed = lexer.lex();

    let mut parser = Parser::new(lexed, reporter);
    let parsed = parser.parse();

    if TypeChecker::check(parsed) == false {
        return;
    }
}

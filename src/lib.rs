pub mod diagnostic;
pub mod lexing;
pub mod parsing;
pub mod source;
pub mod analysis;
pub mod program;

use diagnostic::*;
use lexing::*;
use parsing::*;
use analysis::*;
pub use source::*;
use std::rc::Rc;

pub fn run(source: Source) {
    let reporter: Rc<dyn Reporter> = diagnostic::DefaultReporter::new();

    let lexer = Lexer::new(source, Rc::clone(&reporter));
    let lexed = lexer.lex();

    let mut parser = Parser::new(lexed, reporter);
    let parsed = parser.parse();

    let mut type_checker = TypeChecker::new();
    type_checker.check(parsed);
}

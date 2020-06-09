pub mod analysis;
pub mod diagnostic;
pub mod lexing;
pub mod parsing;
pub mod source;
pub mod codegen;

use analysis::*;
use diagnostic::*;
use lexing::*;
use parsing::*;
use codegen::*;
pub use source::*;
use std::rc::Rc;

pub fn run(source: Source) {
    let reporter: Rc<dyn Reporter> = diagnostic::DefaultReporter::new();

    let lexer = Lexer::new(source, Rc::clone(&reporter));
    let lexed = lexer.lex();

    let parser = Parser::new(lexed, Rc::clone(&reporter));
    let parsed = parser.parse();

    let mut printer = ASTPrinter::new();
    printer.print(&parsed);

    let (symbols, success) = TypeChecker::check(&parsed, Rc::clone(&reporter));
    if success == false {
        return;
    }

    Codegen::generate(parsed, symbols, reporter);
}

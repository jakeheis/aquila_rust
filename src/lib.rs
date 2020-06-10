pub mod analysis;
pub mod codegen;
pub mod diagnostic;
pub mod lexing;
pub mod parsing;
pub mod source;

use analysis::*;
use codegen::*;
use diagnostic::*;
use lexing::*;
use parsing::*;
pub use source::*;
use std::rc::Rc;

pub fn run(source: Source) {
    let reporter: Rc<dyn Reporter> = diagnostic::DefaultReporter::new();

    let lexer = Lexer::new(source, Rc::clone(&reporter));
    let lexed = lexer.lex();

    // let re: &[Token] = &lexed.tokens;
    // println!("lexed {}", re.token_string());

    let parser = Parser::new(lexed, Rc::clone(&reporter));
    let parsed = parser.parse();

    let (symbols, success) = TypeChecker::check(&parsed, Rc::clone(&reporter));
    if success == false {
        return;
    }

    let mut printer = ASTPrinter::new();
    printer.print(&parsed);

    Codegen::generate(parsed, symbols, reporter);
}

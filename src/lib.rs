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

pub struct LogOptions {
    lexer: bool,
    parser: bool,
    type_checker: bool,
}

pub fn run(source: Source) -> Result<(), &'static str> {
    let log_options = LogOptions {
        lexer: false,
        parser: true,
        type_checker: false,
    };

    let reporter: Rc<dyn Reporter> = diagnostic::DefaultReporter::new();

    let lexer = Lexer::new(source, Rc::clone(&reporter));
    let lexed = lexer.lex();

    if log_options.lexer {
        let re: &[Token] = &lexed.tokens;
        println!("lexed {}", re.token_string());
    }

    let parser = Parser::new(lexed, Rc::clone(&reporter));
    let parsed = parser.parse();

    if log_options.parser {
        let mut printer = ASTPrinter::new();
        printer.print(&parsed);
    }

    if reporter.has_errored() {
        return Err("Parsing failed");
    }

    let symbols = TypeChecker::check(&parsed, Rc::clone(&reporter));

    if log_options.type_checker {
        println!("Table: {}", symbols);

        let mut printer = ASTPrinter::new();
        printer.print(&parsed);
    }

    if reporter.has_errored() {
        return Err("Type checker failed");
    }

    Codegen::generate(parsed, symbols);

    Ok(())
}

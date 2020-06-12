pub mod analysis;
pub mod codegen;
pub mod diagnostic;
pub mod lexing;
pub mod parsing;
pub mod source;
pub mod stdlib;

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
    let (program, symbols) = build_program(source, true)?;

    Codegen::generate(program, symbols);

    Ok(())
}

pub fn build_program(source: Source, include_stdlib: bool) -> Result<(ParsedProgram, SymbolTable), &'static str> {
    let log_options = LogOptions {
        lexer: false,
        parser: false,
        type_checker: true,
    };

    let reporter: Rc<dyn Reporter> = diagnostic::DefaultReporter::new();

    let lexer = Lexer::new(source, Rc::clone(&reporter));
    let lexed = lexer.lex();

    if log_options.lexer {
        let re: &[Token] = &lexed.tokens;
        println!("lexed {}", re.token_string());
    }

    let parser = Parser::new(lexed, Rc::clone(&reporter));
    let parsed = parser.parse(include_stdlib);

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

    let parsed = CycleChecker::check(parsed, Rc::clone(&reporter));
    if reporter.has_errored() {
        return Err("Cycle checker failed");
    }

    Ok((parsed, symbols))
}

#[macro_export]
macro_rules! guard {
    ($pattern_path:path[$( $name:ident ), *] = $bound:expr) => {
        let ($($name), *) = if let $pattern_path($($name), *) = $bound {
            ($($name), *)
        } else {
            unreachable!()
        };
    };
}

#[macro_export]
macro_rules! guard_else {
    ($pattern_path:path[$name:ident] = $bound:expr, $else_body:block) => {
        let $name = if let $pattern_path($name) = $bound {
            $name
        } else $else_body;
    };
    ($pattern_path:path[$( $name:ident ), *] = $bound:expr, $else_body:block) => {
        let ($($name), *) = if let $pattern_path($($name), *) = $bound {
            ($($name), *)
        } else $else_body;
    };
}

pub mod analysis;
pub mod codegen;
pub mod diagnostic;
pub mod lexing;
pub mod library;
pub mod parsing;
pub mod source;

use codegen::Codegen;
use library::Lib;
use diagnostic::{Reporter, DefaultReporter};
pub use source::*;

pub fn run(source: Source) -> Result<(), &'static str> {
    run_with_reporter(source, DefaultReporter::new())
}

pub fn run_with_reporter(source: Source, reporter: std::rc::Rc<dyn Reporter>) -> Result<(), &'static str> {
    let lib = Lib::from_source(source, reporter)?;
    Codegen::generate(lib);
    Ok(())
}

#[macro_export]
macro_rules! guard {
    ($pattern_path:path[$name:ident] = $bound:expr) => {
        let $name = if let $pattern_path($name) = $bound {
            $name
        } else {
            unreachable!()
        };
    };
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

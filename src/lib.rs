pub mod analysis;
pub mod codegen;
pub mod diagnostic;
pub mod lexing;
pub mod library;
pub mod parsing;
pub mod source;

use codegen::Codegen;
use library::Lib;
pub use source::*;

pub fn run(source: Source) -> Result<(), &'static str> {
    let lib = Lib::from_source(source)?;
    Codegen::generate(lib);
    Ok(())
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

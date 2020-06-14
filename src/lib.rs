pub mod analysis;
pub mod codegen;
pub mod diagnostic;
pub mod lexing;
pub mod parsing;
pub mod source;
pub mod stdlib;

pub use source::*;
use stdlib::Lib;
use codegen::Codegen;

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

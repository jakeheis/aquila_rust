pub mod analysis;
pub mod codegen;
pub mod diagnostic;
pub mod lexing;
pub mod library;
pub mod parsing;
pub mod source;
pub mod type_checker;

use diagnostic::{DefaultReporter, Reporter};
use library::ModuleBuilder;
pub use source::*;

pub fn run(source: Source, link_stdlib: bool) -> Result<(), &'static str> {
    run_with_reporter(source, DefaultReporter::new(), link_stdlib)
}

pub fn run_with_reporter(
    source: Source,
    reporter: std::rc::Rc<dyn Reporter>,
    link_stdlib: bool,
) -> Result<(), &'static str> {
    let mut builder = ModuleBuilder::new(reporter);

    if link_stdlib {
        builder.build_stdlib();
    }
    let sym = builder.build_src(source)?;

    let modules = builder.take_modules();

    codegen::generate(modules, sym.child("main"))?;

    Ok(())
}

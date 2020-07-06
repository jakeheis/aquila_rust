mod builtins;
mod codewriter;
mod ir;
mod irgen;
mod irwriter;
mod memory;

pub use ir::{
    IRBinaryOperator, IRExpr, IRExprKind, IRFunction, IRStatement, IRStructure, IRUnaryOperator,
};
pub use irgen::IRGen;

use crate::analysis::SpecializationPropagator;
use crate::diagnostic::*;
use crate::library::{Lib, Module};
use std::fs::{self, File};
use std::process::Command;
use std::rc::Rc;

pub fn generate(lib: Lib, reporter: Rc<dyn Reporter>) -> Result<(), &'static str> {
    let ir_libs = compile(lib);
    // ir.dump();

    let spec_map = SpecializationPropagator::propagate(&ir_libs);

    fs::create_dir_all("build").unwrap();
    let file = File::create("build/main.c").unwrap();
    let code_writer = codewriter::CodeWriter::new(ir_libs, file, spec_map);
    code_writer.write();

    let status = Command::new("/usr/local/opt/llvm/bin/clang")
        .args(&[
            "-g",
            "-Iinclude",
            "-I/Library/Developer/CommandLineTools/usr/include/c++/v1",
            "-O0",
            "main.c",
            "-o",
            "program",
        ])
        .current_dir("build")
        .status()
        .unwrap();

    if !status.success() {
        reporter.report(Diagnostic::error(&Span::unknown(), "C build failed"));
        Err("C build failed")
    } else {
        Ok(())
    }
}

pub fn compile(lib: Lib) -> Vec<Module> {
    let mut mods = IRGen::new(lib).generate();
    let tables: Vec<_> = mods.iter().map(|m| Rc::clone(&m.symbols)).collect();
    for module in &mut mods {
        for func in &mut module.functions {
            let mut writer = memory::FreeWriter::new(&tables, func, &module.specialization_tracker);
            writer.write();
        }
    }
    mods
}

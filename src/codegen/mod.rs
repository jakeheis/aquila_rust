mod builtins;
mod codewriter;
mod ir;
mod irgen;
mod irwriter;
mod memory;
mod specialize;

pub use ir::{
    IRBinaryOperator, IRExpr, IRExprKind, IRFunction, IRStatement, IRStructure, IRUnaryOperator,
};
pub use irgen::IRGen;
pub use specialize::{SpecializationRecord, SpecializationPropagator};

use crate::library::{Lib, Module};
use std::fs::{self, File};
use std::process::Command;

pub fn generate(lib: Lib) -> Result<(), &'static str> {
    let ir_libs = compile(lib);

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
        panic!("C build failed")
    } else {
        Ok(())
    }
}

pub fn compile(lib: Lib) -> Vec<Module> {
    IRGen::new(lib).generate()
}

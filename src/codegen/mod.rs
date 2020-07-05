mod builtins;
mod codewriter;
mod ir;
mod irgen;
mod irwriter;

pub use ir::{
    IRBinaryOperator, IRExpr, IRExprKind, IRFunction, IRProgram, IRStatement, IRStructure,
    IRUnaryOperator,
};
pub use irgen::IRGen;

use crate::analysis::SpecializationPropagator;
use crate::diagnostic::*;
use crate::library::Lib;
use std::fs::{self, File};
use std::process::Command;
use std::rc::Rc;

pub fn generate(mut lib: Lib, reporter: Rc<dyn Reporter>) -> Result<(), &'static str> {
    builtins::record_implicit_calls(&mut lib);
    
    let (mut lib, ir) = gen_ir(lib);
    // ir.dump();

    let spec_map = SpecializationPropagator::propogate(&mut lib);

    fs::create_dir_all("build").unwrap();
    let file = File::create("build/main.c").unwrap();
    let code_writer = codewriter::CodeWriter::new(ir, file, spec_map);
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

fn gen_ir(lib: Lib) -> (Lib, IRProgram) {
    let lib = Rc::new(lib);
    let mut program = IRGen::new(Rc::clone(&lib)).generate();
    let mut lib = Rc::try_unwrap(lib).ok().unwrap();

    let deps = std::mem::replace(&mut lib.dependencies, Vec::new());
    let mut restored_deps: Vec<Lib> = Vec::new();

    for dep in deps {
        let (dep, mut dep_program) = gen_ir(dep);
        program.structures.append(&mut dep_program.structures);
        program.functions.append(&mut dep_program.functions);
        restored_deps.push(dep);
    }
    std::mem::replace(&mut lib.dependencies, restored_deps);

    (lib, program)
}

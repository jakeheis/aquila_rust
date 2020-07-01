
#[allow(dead_code)]
mod c_writer;

#[allow(dead_code)]
pub mod codegen;

pub mod core;
mod ir;
mod irgen;
mod irwriter;
mod codewriter;

pub use codegen::Codegen;
pub use ir::{IRProgram, IRStructure, IRFunction, IRStatement, IRExpr, IRExprKind};
pub use irgen::IRGen;

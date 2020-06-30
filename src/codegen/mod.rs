mod c_writer;
pub mod codegen;
pub mod core;
mod ir;
mod irgen;
mod codewriter;

pub use codegen::Codegen;
pub use ir::{IRProgram, IRStructure, IRFunction, IRStatement, IRExpr, IRExprKind};
pub use irgen::IRGen;

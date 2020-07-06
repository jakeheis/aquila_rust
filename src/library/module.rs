use crate::codegen::*;
use super::{SymbolTable, SpecializationTracker};

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub structures: Vec<IRStructure>,
    pub functions: Vec<IRFunction>,
    pub symbols: SymbolTable,
    pub specialization_tracker: SpecializationTracker,
}

impl Module {
    pub fn new() -> Self {
        Module {
            name: String::new(),
            structures: Vec::new(),
            functions: Vec::new(),
            symbols: SymbolTable::new(),
            specialization_tracker: SpecializationTracker::new(),
        }
    }

    pub fn dump(&self) {
        println!("Structures:");
        for structure in &self.structures {
            println!("  {}", structure.name);
            for field in &structure.fields {
                println!("    {}: {}", field.name, field.var_type);
            }
        }

        println!("Functions:");
        for func in &self.functions {
            println!("  {}", func.name);
            for param in &func.parameters {
                println!("    param: {}: {}", param.name, param.var_type);
            }
            println!("    ret: {}", func.return_type);
        }
    }
}

use super::SymbolTable;
use crate::codegen::*;
use std::rc::Rc;

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub structures: Vec<IRStructure>,
    pub functions: Vec<IRFunction>,
    pub symbols: Rc<SymbolTable>,
    pub specialization_record: SpecializationRecord,
}

impl Module {
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

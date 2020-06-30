use super::ir::*;
use std::fs::File;
use std::io::Write;
use std::cell::{RefCell, Cell};
use crate::library::NodeType;

pub struct CodeWriter {
    program: IRProgram,
    file: RefCell<File>,
    indent: Cell<u32>,
}

impl CodeWriter {
    pub fn new(program: IRProgram, file: File) -> Self {
        CodeWriter {
            program,
            file: RefCell::new(file),
            indent: Cell::new(0)
        }
    }

    pub fn write(&self) {
        self.write_includes();
        self.write_struct_prototypes();
        self.write_struct_bodies();
    }

    fn write_includes(&self) {
        self.writeln("#include <stdlib.h>");
        self.writeln("#include <stdbool.h>");
        self.writeln("#include <stdio.h>");
        self.writeln("#include <string.h>");
    }

    fn write_struct_prototypes(&self) {
        for struct_def in &self.program.structures {
            self.writeln("");
            self.writeln(&format!(
                "struct {};",
                struct_def.name
            ));
        }
    }

    fn write_struct_bodies(&self) {
        for struct_def in &self.program.structures {
            self.writeln("");
            self.writeln(&format!("typedef struct {} {{", struct_def.name));
            self.add_indent();

            for var in &struct_def.fields {
                let (c_type, name) = self.convert_type(&var.var_type, var.name.clone(), true);
                self.writeln(&format!("{} {};", c_type, name));
            }

            self.remove_indent();
            self.writeln(&format!("}} {};", struct_def.name));
        }
    }

    fn convert_type(
        &self,
        node_type: &NodeType,
        name: String,
        include_struct: bool,
    ) -> (String, String) {
        let simple = match node_type {
            NodeType::Void => Some("void"),
            NodeType::Int => Some("int"),
            NodeType::Double => Some("double"),
            NodeType::Bool => Some("bool"),
            NodeType::Byte => Some("char"),
            _ => None,
        };
        if let Some(simple) = simple {
            return (String::from(simple), name);
        }

        match node_type {
            NodeType::Instance(..) => {
                if include_struct {
                    (format!("struct {}", node_type.symbolic_form()), name)
                } else {
                    (node_type.symbolic_form(), name)
                }
            }
            NodeType::Pointer(ty) => {
                let ty: &NodeType = &ty.coerce_array_to_ptr();
                if let NodeType::Any = ty {
                    (String::from("void*"), name)
                } else {
                    let (type_portion, name_portion) = self.convert_type(ty, name, include_struct);
                    (format!("{}*", type_portion), name_portion)
                }
            }
            NodeType::Array(ty, count) => {
                let (type_portion, name_portion) = self.convert_type(ty, name, include_struct);
                (type_portion, format!("{}[{}]", name_portion, count))
            }
            other_type => panic!("Can't convert type {}", other_type),
        }
    }

    fn add_indent(&self) {
        let i = self.indent.get();
        self.indent.set(i + 1);
    }

    fn remove_indent(&self) {
        let i = self.indent.get();
        self.indent.set(i - 1);
    }

    fn writeln(&self, line: &str) {
        let mut file = self.file.borrow_mut();
        let indent = (0..self.indent.get()).map(|_| "    ").collect::<String>();
        let line = format!("{}{}", indent, line);
        writeln!(file, "{}", line).unwrap();
    }
}

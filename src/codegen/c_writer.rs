pub use crate::analysis::NodeType;
use std::fs::File;
use std::io::Write;

pub struct CWriter {
    file: File,
    indent: u32,
}

impl CWriter {
    pub fn new(file: File) -> Self {
        let mut writer = CWriter { file, indent: 0 };

        writer.write_includes();

        writer
    }

    fn write_includes(&mut self) {
        self.writeln("#include <stdlib.h>");
        self.writeln("#include <stdbool.h>");
        self.writeln("#include <stdio.h>");
        self.writeln("#include <string.h>");
    }

    pub fn write_struct_forward_decl(&mut self, name: &str) {
        self.writeln("");
        self.writeln(&format!("struct {};", name));
    }

    pub fn start_decl_struct(&mut self, name: &str) {
        self.writeln("");
        self.writeln(&format!("typedef struct {} {{", name));
        self.indent += 1;
    }

    pub fn end_decl_struct(&mut self, name: &str) {
        self.indent -= 1;
        self.writeln(&format!("}} {};", name));
    }

    pub fn write_function_prototype(
        &mut self,
        ret_type: &NodeType,
        name: &str,
        params: &[(NodeType, String)],
    ) {
        self.writeln("");
        self.write_function_header(ret_type, name, params, ";");
    }

    pub fn start_decl_func(
        &mut self,
        ret_type: &NodeType,
        name: &str,
        params: &[(NodeType, String)],
    ) {
        self.writeln("");
        self.write_function_header(ret_type, name, params, " {");
        self.indent += 1;
    }

    pub fn end_decl_func(&mut self) {
        self.indent -= 1;
        self.writeln("}");
    }

    pub fn decl_var(&mut self, var_type: &NodeType, name: &str, initial_value: Option<String>) {
        let start = self.type_and_name(var_type, name);
        let whole = if let Some(initial_value) = initial_value {
            format!("{} = {}", start, initial_value)
        } else {
            start
        };
        self.writeln(&(whole + ";"));
    }

    pub fn type_and_name(
        &self,
        var_type: &NodeType,
        name: &str,
    ) -> String {
        let (t, n) = self.convert_type(var_type, String::from(name));
        format!("{} {}", t, n)
    }

    pub fn start_condition_block(&mut self, name: &str, condition: String) {
        let line = format!("{} ({}) {{", name, condition);
        self.writeln(&line);
        self.indent += 1;
    }

    pub fn start_else_block(&mut self) {
        self.indent -= 1;
        self.writeln("} else {");
        self.indent += 1;
    }

    pub fn end_conditional_block(&mut self) {
        self.indent -= 1;
        self.writeln("}");
    }

    pub fn write_return(&mut self, value: Option<String>) {
        let val = value.unwrap_or(String::from(""));
        let line = format!("return {};", val);
        self.writeln(&line);
    }

    pub fn write_print(&mut self, expr: Option<(&NodeType, String)>) {
        if let Some((node_type, expr)) = expr {
            let format_specificer = match node_type {
                NodeType::Int | NodeType::Bool => "%i",
                node_type if node_type.is_pointer_to(NodeType::Byte) => "%s",
                _ => unreachable!(),
            };
            let line = format!("printf(\"{}\\n\", {});", format_specificer, expr);
            self.writeln(&line);
        } else {
            self.writeln("printf(\"\\n\");");
        }
    }

    fn write_function_header(
        &mut self,
        ret_type: &NodeType,
        name: &str,
        params: &[(NodeType, String)],
        terminator: &str,
    ) {
        let param_str: Vec<String> = params
            .iter()
            .map(|(param_type, name)| self.type_and_name(param_type, &name))
            .collect();
        let param_str = param_str.join(",");
        self.writeln(&format!(
            "{}({}){}",
            self.type_and_name(ret_type, name),
            param_str,
            terminator
        ));
    }

    pub fn write_assignment(&mut self, target: &str, value: &str) {
        let line = format!("{} = {};", target, value);
        self.writeln(&line);
    }

    pub fn convert_type(
        &self,
        node_type: &NodeType,
        name: String,
    ) -> (String, String) {
        let simple = match node_type {
            NodeType::Void => Some("void"),
            NodeType::Int => Some("int"),
            NodeType::Bool => Some("bool"),
            NodeType::Byte => Some("char"),
            _ => None,
        };
        if let Some(simple) = simple {
            return (String::from(simple), name);
        }

        match node_type {
            NodeType::Type(symbol) => (symbol.mangled(), name),
            NodeType::Pointer(ty) => {
                let ty: &NodeType = &ty.coerce_array_to_ptr();
                if let NodeType::Any = ty {
                    (String::from("void*"), name)
                } else {
                    let (type_portion, name_portion) = self.convert_type(ty, name);
                    (format!("{}*", type_portion), name_portion)
                }
            }
            NodeType::Array(ty, count) => {
                let (type_portion, name_portion) = self.convert_type(ty, name);
                (type_portion, format!("{}[{}]", name_portion, count))
            },
            other_type => panic!("Can't convert type {}", other_type),
        }
    }

    pub fn writeln(&mut self, line: &str) {
        let indent = (0..self.indent).map(|_| "    ").collect::<String>();
        let line = format!("{}{}", indent, line);
        writeln!(self.file, "{}", line).unwrap();
    }
}

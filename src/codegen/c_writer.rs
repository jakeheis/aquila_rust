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

    pub fn decl_var(&mut self, var_type: &NodeType, name: &str) {
        self.writeln(&format!("{} {};", self.convert_type(var_type), name));
    }

    pub fn start_if_block(&mut self, condition: String) {
        let line = format!("if ({}) {{", condition);
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
                NodeType::StringLiteral => "%s",
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
            .map(|(param_type, name)| format!("{} {}", self.convert_type(param_type), name))
            .collect();
        let param_str = param_str.join(",");
        self.writeln(&format!(
            "{} {}({}){}",
            self.convert_type(ret_type),
            name,
            param_str,
            terminator
        ));
    }

    pub fn write_assignment(&mut self, target: String, value: String) {
        let line = format!("{} = {};", target, value);
        self.writeln(&line);
    }

    fn convert_type(&self, node_type: &NodeType) -> String {
        match node_type {
            NodeType::Void => String::from("void"),
            NodeType::Int => String::from("int"),
            NodeType::Bool => String::from("bool"),
            NodeType::Byte => String::from("char"),
            NodeType::StringLiteral => String::from("char *"),
            NodeType::Type(symbol) => {
                if false {
                    format!("struct {}", symbol.mangled())
                } else {
                    symbol.mangled()
                }
            },
            NodeType::Pointer(ty) => format!("{}*", self.convert_type(ty)),
            other_type => panic!("Can't convert type {}", other_type),
        }
    }

    pub fn writeln(&mut self, line: &str) {
        let indent = (0..self.indent).map(|_| "    ").collect::<String>();
        let line = format!("{}{}", indent, line);
        writeln!(self.file, "{}", line).unwrap();
    }
}

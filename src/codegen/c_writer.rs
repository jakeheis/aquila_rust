pub use crate::analysis::NodeType;
use std::fs::File;
use std::io::Write;
// use std::io::BufWriter;

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
        self.writeln("#include <stdbool.h>");
        self.writeln("#include <stdio.h>");
        self.writeln("");
    }

    pub fn start_decl_struct(&mut self) {
        self.writeln(&format!("typedef struct {{"));
        self.indent += 1;
    }

    pub fn end_decl_struct(&mut self, name: &str) {
        self.indent -= 1;
        self.writeln(&format!("}} {};", name));
    }

    pub fn start_decl_func(
        &mut self,
        ret_type: &NodeType,
        name: &str,
        params: &[(NodeType, String)],
    ) {
        let param_str: Vec<String> = params
            .iter()
            .map(|(param_type, name)| format!("{} {}", Self::convert_type(param_type), name))
            .collect();
        let param_str = param_str.join(",");
        self.writeln(&format!(
            "{} {}({}) {{",
            Self::convert_type(ret_type),
            name,
            param_str
        ));
        self.indent += 1;
    }

    pub fn end_decl_func(&mut self) {
        self.indent -= 1;
        self.writeln("}");
    }

    pub fn decl_var(&mut self, var_type: &NodeType, name: &str) {
        self.writeln(&format!("{} {};", CWriter::convert_type(var_type), name));
    }

    fn convert_type(node_type: &NodeType) -> String {
        let slice = match node_type {
            NodeType::Void => "void",
            NodeType::Int => "int",
            NodeType::Bool => "bool",
            NodeType::Type(string) => &string,
            _ => panic!(),
        };
        String::from(slice)
    }

    // fn indent<F>(&mut self, block: F)
    // where
    //     F: Fn(&mut CWriter) -> (),
    // {
    //     self.indent += 1;
    //     block(self);
    //     self.indent -= 1;
    // }

    pub fn writeln(&mut self, line: &str) {
        let indent = (0..self.indent).map(|_| "    ").collect::<String>();
        let line = format!("{}{}", indent, line);
        // println!("{}", line);
        writeln!(self.file, "{}", line);
    }
}

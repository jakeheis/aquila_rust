pub use crate::analysis::NodeType;

pub struct CWriter {
    indent: u32,
}

impl CWriter {
    pub fn new() -> Self {
        CWriter { indent: 0 }
    }

    pub fn start_decl_struct(&mut self, name: &str) {
        self.writeln(&format!("struct {} {{", name));
        self.indent += 1;
    }

    pub fn end_decl_struct(&mut self) {
        self.indent -= 1;
        self.writeln("};");
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

    pub fn decl_var(&self, var_type: &NodeType, name: &str) {
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

    fn indent<F>(&mut self, block: F)
    where
        F: Fn(&mut CWriter) -> (),
    {
        self.indent += 1;
        block(self);
        self.indent -= 1;
    }

    pub fn writeln(&self, line: &str) {
        let indent = (0..self.indent).map(|_| "    ").collect::<String>();
        let line = format!("{}{}", indent, line);
        println!("{}", line);
    }
}

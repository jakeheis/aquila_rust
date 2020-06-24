use crate::type_checker::NodeType;
use crate::library::*;
use crate::source::ContainsSpan;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;

pub struct CWriter {
    lib: Rc<Lib>,
    file: File,
    indent: u32,
}

impl CWriter {
    pub fn new(lib: Rc<Lib>, file: File) -> Self {
        let mut writer = CWriter {
            lib,
            file,
            indent: 0,
        };

        writer.write_includes();

        writer
    }

    fn write_includes(&mut self) {
        self.writeln("#include <stdlib.h>");
        self.writeln("#include <stdbool.h>");
        self.writeln("#include <stdio.h>");
        self.writeln("#include <string.h>");
    }

    pub fn write_struct_forward_decl(
        &mut self,
        type_metadata: &TypeMetadata,
        specialization: &GenericSpecialization,
    ) {
        self.writeln("");
        self.writeln(&format!(
            "struct {};",
            type_metadata.type_name(specialization)
        ));
    }

    pub fn write_struct(
        &mut self,
        type_metadata: &TypeMetadata,
        specialization: &GenericSpecialization,
    ) {
        let name = type_metadata.type_name(specialization);

        self.writeln("");
        self.writeln(&format!("typedef struct {} {{", name));
        self.indent += 1;

        for (node_type, symbol) in type_metadata
            .field_types
            .iter()
            .zip(&type_metadata.field_symbols)
        {
            let (c_type, name) = self.convert_type(
                &node_type.specialize(self.lib.as_ref(), specialization),
                symbol.mangled(),
                true,
            );
            self.writeln(&format!("{} {};", c_type, name));
        }

        self.indent -= 1;
        self.writeln(&format!("}} {};", name));
    }

    pub fn write_function_prototype(
        &mut self,
        lib: &Lib,
        function: &FunctionMetadata,
        specialization: &GenericSpecialization,
    ) {
        self.write_function_header(lib, function, specialization, ";");
    }

    pub fn start_decl_func(
        &mut self,
        lib: &Lib,
        function: &FunctionMetadata,
        specialization: &GenericSpecialization,
    ) {
        self.write_function_header(lib, function, specialization, " {");
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

    pub fn type_and_name(&self, var_type: &NodeType, name: &str) -> String {
        let (t, n) = self.convert_type(var_type, String::from(name), false);
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
                NodeType::Double => "%f",
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
        lib: &Lib,
        function: &FunctionMetadata,
        specialization: &GenericSpecialization,
        terminator: &str,
    ) {
        let function_type = function.full_type().specialize(lib, specialization);

        let mut param_str: Vec<String> = function_type
            .parameters
            .iter()
            .zip(&function.parameter_symbols)
            .map(|(param_type, name)| self.type_and_name(param_type, &name.mangled()))
            .collect();

        if let FunctionKind::Method(owner) = &function.kind {
            let self_instance = NodeType::Instance(owner.clone(), specialization.subset(owner));
            let self_type = NodeType::pointer_to(self_instance);
            param_str.insert(0, self.type_and_name(&self_type, "self"));
        }

        let param_str = param_str.join(",");

        self.writeln("");
        self.writeln(&format!(
            "{}({}){}",
            self.type_and_name(
                &function_type.return_type,
                &function.function_name(lib, specialization)
            ),
            param_str,
            terminator
        ));
    }

    pub fn write_assignment(&mut self, target: &str, value: &str) {
        let line = format!("{} = {};", target, value);
        self.writeln(&line);
    }

    pub fn write_guard<T: ContainsSpan>(&mut self, guard: String, message: &str, span: &T) {
        self.start_condition_block("if", guard);

        let message = format!(
            "\\nFatal error: {}\\n\\n{}\\n",
            message,
            span.span().location(),
        );

        self.writeln(&format!("printf(\"{}\\n\");", message));
        self.writeln("exit(1);");
        self.end_conditional_block();
    }

    pub fn convert_type(
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

    pub fn writeln(&mut self, line: &str) {
        let indent = (0..self.indent).map(|_| "    ").collect::<String>();
        let line = format!("{}{}", indent, line);
        writeln!(self.file, "{}", line).unwrap();
    }
}

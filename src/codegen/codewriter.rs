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
        
        self.write_function_prototypes();
        self.write_function_bodies();
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
            self.increase_indent();

            for var in &struct_def.fields {
                let (c_type, name) = self.convert_type(&var.var_type, var.name.clone(), true);
                self.writeln(&format!("{} {};", c_type, name));
            }

            self.decrease_indent();
            self.writeln(&format!("}} {};", struct_def.name));
        }
    }

    fn write_function_prototypes(&self) {
        for func in &self.program.functions {
            self.write_function_header(func, ";");
        }
    }

    fn write_function_bodies(&self) {
        for func in &self.program.functions {
            self.write_function_header(func, " {");
            self.increase_indent();
            self.write_block(&func.statements);
            self.decrease_indent();
            self.writeln("}");
        }
    }

    fn write_block(&self, stmts: &[IRStatement]) {
        for stmt in stmts {
            self.write_statement(stmt);
        }
    }

    fn write_statement(&self, stmt: &IRStatement) {
        match stmt {
            IRStatement::DeclLocal(var) => {
                let line = format!("{};", self.type_and_name(&var.var_type, &var.name));
                self.writeln(&line);
            }
            IRStatement::AssignLocal(local, value) => {
                let value = self.form_expression(value);
                let line = format!("{} = {};", local, value);
                self.writeln(&line);
            }
            IRStatement::AssignField(object, field, value) => {
                let object = self.form_expression(object);
                let value = self.form_expression(value);
                let line = format!("{}.{} = {};", object, field, value);
                self.writeln(&line);
            }
            IRStatement::Loop(block) => {
                self.writeln("while (true) {");
                self.increase_indent();
                self.write_block(block);
                self.decrease_indent();
                self.writeln("}");
            }
            IRStatement::Condition(condition, if_block, else_block) => {
                let condition = format!("if ({}) {{", self.form_expression(condition));
                self.writeln(&condition);
                self.increase_indent();
                self.write_block(if_block);
                self.decrease_indent();
                
                if !else_block.is_empty() {
                    self.writeln("} else {");
                    self.increase_indent();
                    self.write_block(else_block);
                    self.decrease_indent();
                }

                self.writeln("}");
            }
            IRStatement::Execute(expr) => {
                let line = format!("{};", self.form_expression(expr));
                self.writeln(&line);
            }
            IRStatement::Return(value) => {
                let line = if let Some(value) = value {
                    format!("return {};", self.form_expression(value))
                } else {
                    String::from("return;")
                };
                self.writeln(&line);
            }
            IRStatement::Break => {
                self.writeln("break;");
            }
        }
    }

    fn form_expression(&self, expr: &IRExpr) -> String {
        match &expr.kind {
            IRExprKind::FieldAccess(target, field) => {
                let target = self.form_expression(target);
                format!("{}.{}", target, field)
            }
            IRExprKind::DerefFieldAccess(target, field) => {
                let target = self.form_expression(target);
                format!("{}->{}", target, field)
            }
            IRExprKind::Call(function, args) => {
                let args: Vec<_> = args.iter().map(|a| self.form_expression(a)).collect();
                let args = args.join(",");
                format!("{}({})", function, args)
            }
            IRExprKind::Subscript(target, value) => {
                let target = self.form_expression(target);
                let value = self.form_expression(value);
                format!("{}[{}]", target, value)
            }
            IRExprKind::Binary(lhs, op, rhs) => {
                let lhs = self.form_expression(lhs);
                let rhs = self.form_expression(rhs);
                format!("({}) {} ({})", lhs, op, rhs)
            }
            IRExprKind::Unary(operator, operand) => {
                let operand = self.form_expression(operand);
                format!("{}({})", operator, operand)
            }
            IRExprKind::Literal(l) => l.clone(),
            IRExprKind::Variable(var) => String::from(var),
            IRExprKind::ExplicitType => {
                let (to_type, _) = self.convert_type(&expr.expr_type, String::new(), false);
                to_type
            }
            IRExprKind::Cast(value) => {
                let value = self.form_expression(value);
                let (to_type, _) = self.convert_type(&expr.expr_type, String::new(), false);
                format!("({})({})", to_type, value)
            }
        }
    }

    fn write_function_header(
        &self,
        function: &IRFunction,
        terminator: &str,
    ) {
        let param_str: Vec<String> = function
            .parameters
            .iter()
            .map(|param| self.type_and_name(&param.var_type, &param.name))
            .collect();

        let param_str = param_str.join(",");

        self.writeln("");
        self.writeln(&format!(
            "{}({}){}",
            self.type_and_name(
                &function.return_type,
                &function.name
            ),
            param_str,
            terminator
        ));
    }

    pub fn type_and_name(&self, var_type: &NodeType, name: &str) -> String {
        let (t, n) = self.convert_type(var_type, String::from(name), false);
        format!("{} {}", t, n)
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

    fn increase_indent(&self) {
        let i = self.indent.get();
        self.indent.set(i + 1);
    }

    fn decrease_indent(&self) {
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

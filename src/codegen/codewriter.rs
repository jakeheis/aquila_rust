use super::ir::*;
use crate::analysis::FinalSpecializationMap;
use crate::library::{GenericSpecialization, Module, NodeType};
use std::cell::{Cell, RefCell};
use std::fs::File;
use std::io::Write;

pub struct CodeWriter {
    programs: Vec<Module>,
    file: RefCell<File>,
    indent: Cell<u32>,
    spec_map: FinalSpecializationMap,
}

impl CodeWriter {
    pub fn new(programs: Vec<Module>, file: File, spec_map: FinalSpecializationMap) -> Self {
        CodeWriter {
            programs,
            file: RefCell::new(file),
            indent: Cell::new(0),
            spec_map,
        }
    }

    pub fn write(&self) {
        self.write_includes();

        self.write_structs(true);
        self.write_structs(false);

        self.write_functions(true);
        self.write_functions(false);
    }

    fn write_includes(&self) {
        self.writeln("#include <stdlib.h>");
        self.writeln("#include <stdbool.h>");
        self.writeln("#include <stdio.h>");
        self.writeln("#include <string.h>");
    }

    fn write_structs(&self, prototype: bool) {
        let structures = self.programs.iter().flat_map(|p| &p.structures);
        for struct_def in structures {
            if let Some(specs) = self.spec_map.specs_for(&struct_def.name) {
                for spec in specs {
                    let struct_name = struct_def.name.specialized(spec);
                    self.writeln("");

                    if prototype {
                        self.writeln(&format!("struct {};", struct_name));
                    } else {
                        self.writeln(&format!("typedef struct {} {{", struct_name));
                        self.increase_indent();

                        for field in &struct_def.fields {
                            let field_type = field.var_type.specialize(spec);
                            let (c_type, name) =
                                self.convert_type(&field_type, field.name.clone(), true);
                            self.writeln(&format!("{} {};", c_type, name));
                        }

                        self.decrease_indent();
                        self.writeln(&format!("}} {};", struct_name));
                    }
                }
            }
        }
    }

    fn write_functions(&self, prototype: bool) {
        let functions = self.programs.iter().flat_map(|p| &p.functions);
        for func in functions {
            if let Some(specs) = self.spec_map.specs_for(&func.name) {
                for spec in specs {
                    if prototype {
                        self.write_function_header(func, spec, ";");
                    } else {
                        self.write_function_header(func, spec, " {");
                        self.increase_indent();
                        self.write_block(&func.statements, spec);
                        self.decrease_indent();
                        self.writeln("}");
                    }
                }
            }
        }
    }

    fn write_block(&self, stmts: &[IRStatement], spec: &GenericSpecialization) {
        for stmt in stmts {
            let end_block = self.write_statement(stmt, spec);
            if end_block {
                break;
            }
        }
    }

    fn write_statement(&self, stmt: &IRStatement, spec: &GenericSpecialization) -> bool {
        match stmt {
            IRStatement::DeclLocal(var) => {
                let var_type = var.var_type.specialize(spec);
                let line = format!("{};", self.type_and_name(&var_type, &var.name));
                self.writeln(&line);
            }
            IRStatement::Assign(object, value) => {
                let object = self.form_expression(object, spec);
                let value = self.form_expression(value, spec);
                let line = format!("{} = {};", object, value);
                self.writeln(&line);
            }
            IRStatement::Loop(block) => {
                self.writeln("while (true) {");
                self.increase_indent();
                self.write_block(block, spec);
                self.decrease_indent();
                self.writeln("}");
            }
            IRStatement::Condition(condition, if_block, else_block) => {
                let condition = format!("if ({}) {{", self.form_expression(condition, spec));
                self.writeln(&condition);
                self.increase_indent();
                self.write_block(if_block, spec);
                self.decrease_indent();

                if !else_block.is_empty() {
                    self.writeln("} else {");
                    self.increase_indent();
                    self.write_block(else_block, spec);
                    self.decrease_indent();
                }

                self.writeln("}");
            }
            IRStatement::Execute(expr) => {
                let line = format!("{};", self.form_expression(expr, spec));
                self.writeln(&line);
            }
            IRStatement::Return(value) => {
                let line = if let Some(value) = value {
                    format!("return {};", self.form_expression(value, spec))
                } else {
                    String::from("return;")
                };
                self.writeln(&line);
                return true;
            }
            IRStatement::Break => {
                self.writeln("break;");
                return true;
            }
        }

        return false;
    }

    fn form_expression(&self, expr: &IRExpr, enclosing_spec: &GenericSpecialization) -> String {
        let expr_type = expr.expr_type.specialize(enclosing_spec);

        match &expr.kind {
            IRExprKind::FieldAccess(target, field) => {
                let target = self.form_expression(target, enclosing_spec);
                format!("{}.{}", target, field)
            }
            IRExprKind::DerefFieldAccess(target, field) => {
                let target = self.form_expression(target, enclosing_spec);
                format!("{}->{}", target, field)
            }
            IRExprKind::Call(function, spec, args) => {
                let spec = spec.resolve_generics_using(enclosing_spec);
                let args: Vec<_> = args
                    .iter()
                    .map(|a| self.form_expression(a, enclosing_spec))
                    .collect();
                let args = args.join(",");
                let function_name = function.specialized(&spec);
                format!("{}({})", function_name, args)
            }
            IRExprKind::Binary(lhs, op, rhs) => {
                let lhs = self.form_expression(lhs, enclosing_spec);
                let rhs = self.form_expression(rhs, enclosing_spec);
                let op = match op {
                    IRBinaryOperator::Plus => "+",
                    IRBinaryOperator::Minus => "-",
                    IRBinaryOperator::Multiply => "*",
                    IRBinaryOperator::Divide => "/",
                    IRBinaryOperator::EqualEqual => "==",
                    IRBinaryOperator::BangEqual => "!=",
                    IRBinaryOperator::Greater => ">",
                    IRBinaryOperator::GreaterEqual => ">=",
                    IRBinaryOperator::Less => "<",
                    IRBinaryOperator::LessEqual => "<=",
                    IRBinaryOperator::And => "&&",
                    IRBinaryOperator::Or => "||",
                };
                format!("({}) {} ({})", lhs, op, rhs)
            }
            IRExprKind::Unary(operator, operand) => {
                let operand = self.form_expression(operand, enclosing_spec);
                let operator = match operator {
                    IRUnaryOperator::Negate => "-",
                    IRUnaryOperator::Invert => "!",
                    IRUnaryOperator::Reference => "&",
                    IRUnaryOperator::Dereference => "*",
                };
                format!("{}({})", operator, operand)
            }
            IRExprKind::Literal(l) => l.clone(),
            IRExprKind::Variable(var) => String::from(var),
            IRExprKind::ExplicitType => {
                let (to_type, _) = self.convert_type(&expr_type, String::new(), false);
                to_type
            }
            IRExprKind::Cast(value) => {
                let value = self.form_expression(value, enclosing_spec);
                let (to_type, _) = self.convert_type(&expr_type, String::new(), false);
                format!("({})({})", to_type, value)
            }
        }
    }

    fn write_function_header(
        &self,
        function: &IRFunction,
        spec: &GenericSpecialization,
        terminator: &str,
    ) {
        let param_str: Vec<String> = function
            .parameters
            .iter()
            .map(|param| self.type_and_name(&param.var_type.specialize(spec), &param.name))
            .collect();

        let function_name = function.name.specialized(spec);
        let param_str = param_str.join(",");

        self.writeln("");
        self.writeln(&format!(
            "{}({}){}",
            self.type_and_name(&function.return_type.specialize(spec), &function_name),
            param_str,
            terminator
        ));
    }

    fn type_and_name(&self, var_type: &NodeType, name: &str) -> String {
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

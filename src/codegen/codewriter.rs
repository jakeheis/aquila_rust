use super::ir::*;
use crate::library::{NodeType, GenericSpecialization};
use crate::analysis::FinalSpecializationMap;
use std::cell::{Cell, RefCell};
use std::fs::File;
use std::io::Write;

pub struct CodeWriter {
    program: IRProgram,
    file: RefCell<File>,
    indent: Cell<u32>,
    temp_count: Cell<u32>,
    spec_map: FinalSpecializationMap,
}

impl CodeWriter {
    pub fn new(program: IRProgram, file: File, spec_map: FinalSpecializationMap) -> Self {
        CodeWriter {
            program,
            file: RefCell::new(file),
            indent: Cell::new(0),
            temp_count: Cell::new(0),
            spec_map
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
            if let Some(specs) = self.spec_map.specs_for(&struct_def.name) {
                for spec in specs {
                    let struct_name = self.specialized_name(&struct_def.name.mangled(), spec);
                    self.writeln("");
                    self.writeln(&format!("struct {};", struct_name));
                }
            }
        }
    }

    fn write_struct_bodies(&self) {
        for struct_def in &self.program.structures {
            if let Some(specs) = self.spec_map.specs_for(&struct_def.name) {
                for spec in specs {
                    let struct_name = self.specialized_name(&struct_def.name.mangled(), spec);
                    self.writeln("");
                    self.writeln(&format!("typedef struct {} {{", struct_name));
                    self.increase_indent();
        
                    for field in &struct_def.fields {
                        let field_type = field.var_type.specialize(spec);
                        let (c_type, name) = self.convert_type(&field_type, field.name.clone(), true);
                        self.writeln(&format!("{} {};", c_type, name));
                    }
        
                    self.decrease_indent();
                    self.writeln(&format!("}} {};", struct_name));
                }
            }
        }
    }

    fn write_function_prototypes(&self) {
        for func in &self.program.functions {
            if let Some(specs) = self.spec_map.specs_for(&func.name) {
                for spec in specs {
                    self.write_function_header(func, spec, ";");
                }
            }
        }
    }

    fn write_function_bodies(&self) {
        for func in &self.program.functions {
            if let Some(specs) = self.spec_map.specs_for(&func.name) {
                for spec in specs {
                    self.write_function_header(func, spec, " {");
                    self.increase_indent();
                    self.write_block(&func.statements, spec);
                    self.decrease_indent();
                    self.writeln("}");
                }
            }
        }
    }

    fn write_block(&self, stmts: &[IRStatement], spec: &GenericSpecialization) {
        for stmt in stmts {
            self.write_statement(stmt, spec);
        }
    }

    fn write_statement(&self, stmt: &IRStatement, spec: &GenericSpecialization) {
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
            }
            IRStatement::Break => {
                self.writeln("break;");
            }
        }
    }

    fn form_expression(&self, expr: &IRExpr, enclosing_spec: &GenericSpecialization) -> String {
        let expr_type = expr.expr_type.specialize(enclosing_spec);

        match &expr.kind {
            IRExprKind::FieldAccess(target, field) => {
                let target_str = self.form_expression(target, enclosing_spec);
                let target = if let IRExprKind::Call(..) = &target.kind {
                    self.write_temp(&target.expr_type, target_str)
                } else {
                    target_str
                };
                format!("{}.{}", target, field)
            }
            IRExprKind::DerefFieldAccess(target, field) => {
                let target = self.form_expression(target, enclosing_spec);
                format!("{}->{}", target, field)
            }
            IRExprKind::Call(function, spec, args) => {
                let spec = spec.resolve_generics_using(enclosing_spec);
                let args: Vec<_> = args.iter().map(|a| self.form_expression(a, enclosing_spec)).collect();
                let args = args.join(",");
                let function_name = self.specialized_name(function, &spec);
                format!("{}({})", function_name, args)
            }
            IRExprKind::Array(elements) => {
                let elements: Vec<String> =
                    elements.iter().map(|e| self.form_expression(e, enclosing_spec)).collect();
                self.write_temp(&expr_type, format!("{{ {} }}", elements.join(",")))
            }
            IRExprKind::Subscript(target, value) => {
                let target = self.form_expression(target, enclosing_spec);
                let value = self.form_expression(value, enclosing_spec);
                format!("{}[{}]", target, value)
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
                let operand_str = self.form_expression(operand, enclosing_spec);
                let operand = if let IRUnaryOperator::Reference = operator {
                    match &operand.kind {
                        IRExprKind::Unary(IRUnaryOperator::Dereference, inner) => {
                            return self.form_expression(inner, enclosing_spec);
                        }
                        IRExprKind::Variable(..) | IRExprKind::FieldAccess(..) => operand_str,
                        _ => self.write_temp(&operand.expr_type, operand_str),
                    }
                } else {
                    operand_str
                };
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

    fn write_function_header(&self, function: &IRFunction, spec: &GenericSpecialization, terminator: &str) {
        let param_str: Vec<String> = function
            .parameters
            .iter()
            .map(|param| self.type_and_name(&param.var_type.specialize(spec), &param.name))
            .collect();

        let function_name = self.specialized_name(&function.name.mangled(), spec);
        let param_str = param_str.join(",");

        self.writeln("");
        self.writeln(&format!(
            "{}({}){}",
            self.type_and_name(&function.return_type.specialize(spec), &function_name),
            param_str,
            terminator
        ));
    }

    fn write_temp(&self, temp_type: &NodeType, value: String) -> String {
        let temp_count = self.temp_count.get();
        let temp_name = format!("_temp_{}", temp_count);
        self.temp_count.set(temp_count + 1);

        let temp_type = self.type_and_name(&temp_type, &temp_name);
        let temp = format!("{} = {};", temp_type, value);
        self.writeln(&temp);

        temp_name
    }

    fn specialized_name(&self, name: &str, spec: &GenericSpecialization) -> String {
        let func_specialization = spec.symbolic_list();
        if func_specialization.len() > 0 {
            name.to_owned() + "__" + &func_specialization
        } else {
            name.to_owned()
        }
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

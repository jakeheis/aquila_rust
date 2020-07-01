use super::ir::*;
use crate::library::*;
use std::rc::Rc;

pub struct IRWriter {
    lib: Rc<Lib>,
    pub program: IRProgram,
    blocks: Vec<Vec<IRStatement>>
}

impl IRWriter {
    pub fn new(lib: Rc<Lib>) -> Self {
        IRWriter {
            lib,
            program: IRProgram::new(),
            blocks: Vec::new()
        }
    }

    pub fn declare_struct(
        &mut self,
        type_metadata: &TypeMetadata,
        specialization: &GenericSpecialization,
    ) {
        let name = type_metadata.type_name(specialization);

        let fields: Vec<_> = type_metadata
            .field_types
            .iter()
            .zip(&type_metadata.field_symbols)
            .map(|(node_type, symbol)| {
                IRVariable {
                    name: symbol.mangled(),
                    var_type: node_type.specialize(self.lib.as_ref(), specialization)
                }
            }).collect();

        let structure = IRStructure {
            name,
            fields
        };
        self.program.structures.push(structure);
    }

    pub fn start_block(&mut self,) {
        self.blocks.push(Vec::new());
    }

    pub fn end_decl_main(&mut self) {
        let main = IRFunction {
            name: String::from("main"),
            parameters: Vec::new(),
            return_type: NodeType::Int,
            statements: self.blocks.pop().unwrap(),
        };
        self.program.functions.push(main);
    }

    pub fn end_decl_func(&mut self, function: &FunctionMetadata, specialization: &GenericSpecialization) {
        let mut parameters: Vec<_> = function
            .parameter_types
            .iter()
            .zip(&function.parameter_symbols)
            .map(|(param_type, symbol)| {
                IRVariable {
                    name: symbol.mangled(),
                    var_type: param_type.specialize(self.lib.as_ref(), specialization),
                }
            })
            .collect();

        if let FunctionKind::Method(owner) = &function.kind {
            let self_instance = NodeType::Instance(owner.clone(), specialization.subset(owner));
            let self_type = NodeType::pointer_to(self_instance);
            parameters.insert(0, IRVariable {
                name: String::from("self"),
                var_type: self_type
            });
        }

        let function = IRFunction {
            name: function.function_name(self.lib.as_ref(), specialization),
            parameters,
            return_type: function.return_type.specialize(self.lib.as_ref(), specialization),
            statements: self.blocks.pop().unwrap()
        };

        self.program.functions.push(function);
    }

    pub fn end_if_block(&mut self, condition: IRExpr) {
        let if_block = self.blocks.pop().unwrap();
        self.add_stmt(IRStatement::Condition(condition, if_block, Vec::new()));
    }

    pub fn end_if_else_blocks(&mut self, condition: IRExpr) {
        let else_block = self.blocks.pop().unwrap();
        let if_block = self.blocks.pop().unwrap();
        self.add_stmt(IRStatement::Condition(condition, if_block, else_block));
    }

    pub fn end_loop(&mut self) {
        let loop_stmt = IRStatement::Loop(self.blocks.pop().unwrap());
        self.add_stmt(loop_stmt);
    }

    pub fn declare_local(&mut self, symbol: Symbol, var_type: NodeType) -> IRVariable {
        self.declare_local_str(&symbol.mangled(), var_type)
    }

    pub fn declare_local_str(&mut self, name: &str, var_type: NodeType) -> IRVariable {
        let var = IRVariable {
            name: String::from(name),
            var_type,
        };
        self.add_stmt(IRStatement::DeclLocal(var.clone()));
        var
    }

    pub fn declare_var(&mut self, var: &IRVariable) {
        self.add_stmt(IRStatement::DeclLocal(var.clone()));
    }

    pub fn assign(&mut self, var: IRExpr, value: IRExpr) {
        self.add_stmt(IRStatement::Assign(var, value));
    }

    pub fn assign_var(&mut self, var: &IRVariable, value: IRExpr) {
        self.add_stmt(IRStatement::Assign(IRExpr::variable(var), value));
    }

    // pub fn assign_field(&mut self, var: IRExpr, field: &str, value: IRExpr) {
    //     self.add_stmt(IRStatement::AssignField(var, String::from(field), value));
    // }

    pub fn return_value(&mut self, expr: Option<IRExpr>) {
        self.add_stmt(IRStatement::Return(expr));
    }

    pub fn return_var(&mut self, var: &IRVariable) {
        let var = IRExpr::variable(var);
        self.add_stmt(IRStatement::Return(Some(var)));
    }

    pub fn expr(&mut self, expr: IRExpr) {
        self.add_stmt(IRStatement::Execute(expr));
    }

    pub fn break_loop(&mut self) {
        self.add_stmt(IRStatement::Break);
    }

    pub fn add_stmt(&mut self, stmt: IRStatement) {
        self.body().push(stmt);
    }

    pub fn body(&mut self) -> &mut Vec<IRStatement> {
        self.blocks.last_mut().unwrap()
    }

    // pub fn write_guard<T: ContainsSpan>(&mut self, guard: String, message: &str, span: &T) {
    //     self.start_condition_block("if", guard);

    //     let message = format!(
    //         "\\nFatal error: {}\\n\\n{}\\n",
    //         message,
    //         span.span().location(),
    //     );

    //     self.writeln(&format!("printf(\"{}\\n\");", message));
    //     self.writeln("exit(1);");
    //     self.end_conditional_block();
    // }
}

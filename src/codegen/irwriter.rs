use super::ir::*;
use crate::library::*;
use crate::source::Span;
use std::rc::Rc;

pub struct IRWriter {
    pub lib: Rc<Lib>,
    pub all_symbols: SymbolStore,
    pub structures: Vec<IRStructure>,
    pub functions: Vec<IRFunction>,
    pub blocks: Vec<Vec<IRStatement>>,
    temp_count: usize,
}

impl IRWriter {
    pub fn new(lib: Rc<Lib>, all_symbols: SymbolStore) -> Self {
        IRWriter {
            lib,
            all_symbols,
            structures: Vec::new(),
            functions: Vec::new(),
            blocks: Vec::new(),
            temp_count: 0,
        }
    }

    pub fn declare_struct(&mut self, type_metadata: &TypeMetadata) {
        let fields: Vec<_> = type_metadata
            .fields
            .iter()
            .map(|field| {
                let sym = type_metadata.symbol.child(&field.name);
                IRVariable::new_sym(&sym, field.var_type.clone())
            })
            .collect();

        let structure = IRStructure {
            name: type_metadata.symbol.clone(),
            fields,
        };
        self.structures.push(structure);
    }

    pub fn start_block(&mut self) {
        self.blocks.push(Vec::new());
    }

    pub fn end_decl_main(&mut self) {
        let main = IRFunction {
            name: Symbol::main_symbol(&self.lib.name),
            parameters: Vec::new(),
            return_type: NodeType::Int,
            statements: self.blocks.pop().unwrap(),
        };
        self.functions.push(main);
    }

    pub fn end_decl_func(&mut self, function: &FunctionMetadata) {
        let mut parameters: Vec<_> = function
            .parameters
            .iter()
            .map(|param| IRVariable::new_meta(param, &function.symbol))
            .collect();

        if let FunctionKind::Method(owner) = &function.kind {
            let owner_metadata = self.all_symbols.type_metadata(owner).unwrap();
            parameters.insert(
                0,
                IRVariable::self_var(owner_metadata),
            );
        }

        let function = IRFunction {
            name: function.symbol.clone(),
            parameters,
            return_type: function.return_type.clone(),
            statements: self.blocks.pop().unwrap(),
        };

        self.functions.push(function);
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

    pub fn end_conformance_check(&mut self, gen_name: Symbol, trait_name: Symbol) {
        let block = self.blocks.pop().unwrap();
        let check = IRCompilationCondition::ConformanceCheck(gen_name, trait_name);
        self.add_stmt(IRStatement::CompilationCondition(check, block));
    }

    pub fn end_type_equality_check(&mut self, gen_name: Symbol, equal_type: NodeType) {
        let block = self.blocks.pop().unwrap();
        let check = IRCompilationCondition::TypeEqualityCheck(gen_name, equal_type);
        self.add_stmt(IRStatement::CompilationCondition(check, block));
    }

    pub fn declare_local(&mut self, symbol: Symbol, var_type: NodeType) -> IRVariable {
        let var = IRVariable {
            name: symbol.mangled(),
            var_type,
        };
        self.add_stmt(IRStatement::DeclLocal(var.clone()));
        var
    }

    pub fn declare_var(&mut self, var: &IRVariable) {
        self.add_stmt(IRStatement::DeclLocal(var.clone()));
    }

    pub fn declare_temp(&mut self, expr: IRExpr) -> IRVariable {
        let var = self.declare_temp_no_init(expr.expr_type.clone());
        self.assign(IRExpr::variable(&var), expr);
        var
    }

    pub fn declare_temp_no_init(&mut self, var_type: NodeType) -> IRVariable {
        self.temp_count = self.temp_count + 1;
        let var = IRVariable {
            name: format!("_ir_tmp_{}", self.temp_count),
            var_type,
        };
        self.declare_var(&var);
        var
    }

    pub fn addres_of_expr(&mut self, value: IRExpr) -> IRExpr {
        if let IRExprKind::Unary(IRUnaryOperator::Dereference, target) = value.kind {
            return *target;
        }

        let object = if value.has_defined_location() {
            value
        } else {
            let var = self.declare_temp(value);
            IRExpr::variable(&var)
        };

        let expr_type = NodeType::pointer_to(object.expr_type.clone());
        IRExpr {
            kind: IRExprKind::Unary(IRUnaryOperator::Reference, Box::new(object)),
            expr_type,
        }
    }

    pub fn assign(&mut self, var: IRExpr, value: IRExpr) {
        self.add_stmt(IRStatement::Assign(var, value));
    }

    pub fn assign_var(&mut self, var: &IRVariable, value: IRExpr) {
        self.add_stmt(IRStatement::Assign(IRExpr::variable(var), value));
    }

    pub fn return_opt(&mut self, expr: Option<IRExpr>) {
        self.add_stmt(IRStatement::Return(expr));
    }

    pub fn return_value(&mut self, expr: IRExpr) {
        self.add_stmt(IRStatement::Return(Some(expr)));
    }

    pub fn _return_none(&mut self) {
        self.add_stmt(IRStatement::Return(None));
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

    pub fn write_guard(&mut self, guard: IRExpr, message: &str, span: &Span) {
        self.start_block();

        let message = format!("\\nFatal error: {}\\n\\n{}\\n", message, span.location(),);

        let arg = IRExpr::string_literal(&message);
        self.expr(IRExpr::call_extern("printf", vec![arg], NodeType::Void));
        self.expr(IRExpr::call_extern(
            "exit",
            vec![IRExpr::int_literal("1")],
            NodeType::Void,
        ));
        self.end_if_block(guard);
    }
}

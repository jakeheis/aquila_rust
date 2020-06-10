use crate::diagnostic::*;
use crate::lexing::*;
use crate::parsing::*;
use crate::analysis::*;
use super::c_writer::*;
use std::rc::Rc;

pub struct Codegen {
    writer: CWriter,
    global_symbols: SymbolTable,
    reporter: Rc<dyn Reporter>,
}

impl Codegen {

    pub fn generate(program: ParsedProgram, global_symbols: SymbolTable, reporter: Rc<dyn Reporter>) {
        let mut codegen = Codegen {
            writer: CWriter::new(),
            global_symbols,
            reporter,
        };
        codegen.gen_stmts(&program.statements);
    }

    fn gen_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            stmt.accept(self);
        }
    }

    fn map_vars(&self, parent: Option<&Symbol>, vars: &[Stmt]) -> Vec<(NodeType, String)> {
        vars.iter().map(|f| {
            if let StmtKind::VariableDecl(name, _, _) = &f.kind {
                let field_symbol = Symbol::new(parent, &name);
                let field_type = self.global_symbols.get_type(&field_symbol).unwrap().clone();
                (field_type, field_symbol.mangled())
            } else {
                unreachable!()
            }
        }).collect()
    }

}

impl StmtVisitor for Codegen {
    type StmtResult = ();

    fn visit_type_decl(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        fields: &[Stmt],
        methods: &[Stmt],
    ) -> Self::StmtResult {
        let borrowed_symbol = stmt.symbol.borrow();
        let struct_symbol = borrowed_symbol.as_ref().unwrap();

        let struct_fields = self.map_vars(Some(struct_symbol), fields);

        self.writer.decl_struct(name.lexeme(), &struct_fields);
    }

    fn visit_function_decl(
        &mut self,
        stmt: &Stmt,
        _name: &Token,
        params: &[Stmt],
        _return_type: &Option<Token>,
        body: &[Stmt],
    ) -> Self::StmtResult {
        let borrowed_symbol = stmt.symbol.borrow();
        let func_symbol = borrowed_symbol.as_ref().unwrap();

        let func_type = self.global_symbols.get_type(func_symbol).unwrap();
        if let NodeType::Function(param_types, ret_type) = func_type {
            let params: Vec<(NodeType, String)> = param_types.iter().zip(params).map(|(param_type, param_stmt)| {
                (param_type.clone(), param_stmt.symbol.borrow().as_ref().unwrap().mangled())
            }).collect();

            self.writer.start_decl_func(ret_type, &func_symbol.mangled(), &params);
            self.gen_stmts(body);
            self.writer.end_decl_func();
        } else {
            unreachable!();
        }
    }

    fn visit_variable_decl(
        &mut self,
        stmt: &Stmt,
        _name: &Token,
        _kind: &Option<Token>,
        _value: &Option<Expr>,
    ) -> Self::StmtResult {
        let borrowed_symbol = stmt.symbol.borrow();
        let var_symbol = borrowed_symbol.as_ref().unwrap();

        let borrowed_type = stmt.stmt_type.borrow();
        let var_type = borrowed_type.as_ref().unwrap();

        self.writer.decl_var(var_type, &var_symbol.mangled());
    }

    fn visit_if_stmt(
        &mut self,
        _stmt: &Stmt,
        _condition: &Expr,
        _body: &[Stmt],
        _else_body: &[Stmt],
    ) -> Self::StmtResult {

    }

    fn visit_return_stmt(&mut self, stmt: &Stmt, _expr: &Option<Expr>) -> Self::StmtResult {
        self.writer.writeln(stmt.span.lexeme());
    }

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, expr: &Expr)  {
        self.writer.writeln(expr.span.lexeme());
    }
}

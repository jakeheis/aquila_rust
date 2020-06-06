// use std::collections::HashMap;
// use crate::parsing::*;
// use crate::lexing::*;

// struct SymbolTable {
    // type_map: HashMap

// }
/*
struct SymbolTableBuilder {
    table: SymbolTable
}

impl SymbolTableBuilder {
    fn build(program: Vec<Stmt>) {

    }
}

impl StmtVisitor for SymbolTableBuilder {
    type StmtResult = ();

    fn visit_type_decl(
        &mut self,
        name: &Token,
        fields: &[Stmt],
        methods: &[Stmt],
    ) {
        
    }

    fn visit_function_decl(
        &mut self,
        name: &Token,
        params: &[Stmt],
        return_type: &Option<Token>,
        body: &[Stmt],
    ) {
        
    }

    fn visit_variable_decl(
        &mut self,
        name: &Token,
        kind: &Option<Token>,
        value: &Option<Expr>,
    ) {
        
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) {}

    fn visit_expression_stmt(&mut self, expr: &Expr) {}

}
*/
use super::c_writer::*;
use crate::analysis::*;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::parsing::*;
use std::rc::Rc;

pub struct Codegen {
    writer: CWriter,
    global_symbols: SymbolTable,
    current_type: Option<String>,
    reporter: Rc<dyn Reporter>,
}

macro_rules! guard {
    ($pattern_path:path[$( $name:ident ), *] = $bound:expr) => {
        let ($($name), *) = if let $pattern_path($($name), *) = $bound {
            ($($name), *)
        } else {
            unreachable!()
        };
    };
}

impl Codegen {
    pub fn generate(
        program: ParsedProgram,
        global_symbols: SymbolTable,
        reporter: Rc<dyn Reporter>,
    ) {
        let mut codegen = Codegen {
            writer: CWriter::new(),
            global_symbols,
            current_type: None,
            reporter,
        };
        codegen.gen_stmts(&program.statements);
    }

    fn gen_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            stmt.accept(self);
        }
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

        let struct_fields: Vec<(NodeType, String)> = fields
            .iter()
            .map(|f| {
                let borrowed_symbol = f.symbol.borrow();
                let borrowed_type = f.stmt_type.borrow();

                (
                    borrowed_type.as_ref().unwrap().clone(),
                    borrowed_symbol.as_ref().unwrap().mangled(),
                )
            })
            .collect();

        self.writer.decl_struct(name.lexeme(), &struct_fields);

        self.current_type = Some(struct_symbol.mangled());
        for method in methods {
            method.accept(self);
        }
        self.current_type = None;
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

        guard!(NodeType::Function[param_types, ret_type] = func_type);

        let mut params: Vec<(NodeType, String)> = param_types
            .iter()
            .zip(params)
            .map(|(param_type, param_stmt)| {
                (
                    param_type.clone(),
                    param_stmt.symbol.borrow().as_ref().unwrap().mangled(),
                )
            })
            .collect();

        if let Some(current_type) = &self.current_type {
            params.insert(
                0,
                (NodeType::Type(current_type.clone()), String::from("self")),
            );
        }

        self.writer
            .start_decl_func(ret_type, &func_symbol.mangled(), &params);
        self.gen_stmts(body);
        self.writer.end_decl_func();
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

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, expr: &Expr) {
        self.writer.writeln(expr.span.lexeme());
    }
}

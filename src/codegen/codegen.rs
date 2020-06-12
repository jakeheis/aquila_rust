use super::c_writer::*;
use crate::analysis::*;
use crate::guard;
use crate::lexing::*;
use crate::parsing::*;
use std::fs::{self, File};
use std::process::Command;
use super::core;

#[derive(PartialEq)]
enum CodegenStage {
    ForwardStructDecls,
    StructBodies,
    FuncPrototypes,
    FuncBodies,
}

pub struct Codegen {
    writer: CWriter,
    global_symbols: SymbolTable,
    current_type: Option<Symbol>,
    stage: CodegenStage,
    is_builtin: bool,
}

impl Codegen {

    pub fn generate(program: ParsedProgram, global_symbols: SymbolTable) {
        fs::create_dir_all("build").unwrap();
        let file = File::create("build/main.c").unwrap();

        let mut codegen = Codegen {
            writer: CWriter::new(file),
            global_symbols,
            current_type: None,
            stage: CodegenStage::ForwardStructDecls,
            is_builtin: false,
        };

        if let Some(lib) = program.stdlib {
            codegen.gen_stmts(&lib.function_decls);
        }

        codegen.gen_stmts(&program.type_decls);
        codegen.stage = CodegenStage::StructBodies;
        codegen.gen_stmts(&program.type_decls);

        codegen.stage = CodegenStage::FuncPrototypes;
        codegen.gen_stmts(&program.type_decls);
        codegen.gen_stmts(&program.function_decls);

        codegen.stage = CodegenStage::FuncBodies;
        codegen.gen_stmts(&program.type_decls);
        codegen.gen_stmts(&program.function_decls);

        codegen.write_main(&program.main);

        Command::new("/usr/local/opt/llvm/bin/clang")
            .args(&[
                "-g",
                "-Iinclude",
                "-I/Library/Developer/CommandLineTools/usr/include/c++/v1",
                "-O0",
                "main.c",
                "-o",
                "program",
            ])
            .current_dir("build")
            .status()
            .unwrap();
    }

    fn write_main(&mut self, stmts: &[Stmt]) {
        self.writer.start_decl_func(&NodeType::Int, "main", &[]);
        for stmt in stmts {
            stmt.accept(self);
        }
        self.writer.write_return(Some(String::from("0")));
        self.writer.end_decl_func();
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
        _name: &Token,
        fields: &[Stmt],
        methods: &[Stmt],
    ) -> Self::StmtResult {
        let borrowed_symbol = stmt.symbol.borrow();
        let struct_symbol = borrowed_symbol.as_ref().unwrap();

        match self.stage {
            CodegenStage::ForwardStructDecls => self
                .writer
                .write_struct_forward_decl(&struct_symbol.mangled()),
            CodegenStage::StructBodies => {
                self.writer.start_decl_struct(&struct_symbol.mangled());
                for field in fields {
                    field.accept(self);
                }
                self.writer.end_decl_struct(&struct_symbol.mangled());
            }
            CodegenStage::FuncPrototypes | CodegenStage::FuncBodies => {
                self.current_type = Some(struct_symbol.clone());
                for method in methods {
                    method.accept(self);
                }

                let init_symbol = Symbol::new_str(Some(struct_symbol), "init");
                let init_type = self.global_symbols.get_type(&init_symbol).unwrap();
                guard!(NodeType::Function[field_types, ret_type] = init_type);
                
                let field_list: Vec<_> = field_types.iter().zip(fields).map(|(ft, f)| {
                    (ft.clone(), f.symbol.borrow().as_ref().unwrap().mangled(),)
                }).collect();

                if let CodegenStage::FuncPrototypes = self.stage {
                    self.writer.write_function_prototype(
                        &NodeType::Type(struct_symbol.clone()), 
                        &init_symbol.mangled(), 
                        &field_list,
                    );
                } else {
                    self.writer.start_decl_func(
                        &NodeType::Type(struct_symbol.clone()), 
                        &init_symbol.mangled(), 
                        &field_list,
                    );
                    self.writer.decl_var(&ret_type, "new_item");
                    for (_, f) in field_list {
                        let target = format!("new_item.{}", f);
                        self.writer.write_assignment(target, f);
                    }
                    self.writer.write_return(Some(String::from("new_item")));
                    self.writer.end_decl_func();
                }
                    
                self.current_type = None;
            }
        }
    }

    fn visit_function_decl(
        &mut self,
        stmt: &Stmt,
        _name: &Token,
        params: &[Stmt],
        _return_type: &Option<Expr>,
        body: &[Stmt],
    ) -> Self::StmtResult {
        let borrowed_symbol = stmt.symbol.borrow();
        let func_symbol = borrowed_symbol.as_ref().unwrap();
        let borrowed_type = stmt.stmt_type.borrow();
        let func_type = borrowed_type.as_ref().unwrap();

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
                (NodeType::Pointer(
                    Box::new(NodeType::Type(current_type.clone()))
                ), String::from("self")),
            );
        }

        if self.stage == CodegenStage::FuncPrototypes {
            self.writer
                .write_function_prototype(ret_type, &func_symbol.mangled(), &params);
        } else {
            self.writer
                .start_decl_func(ret_type, &func_symbol.mangled(), &params);
            if self.is_builtin {
                let borrowed_symbol = stmt.symbol.borrow();
                let symbol = borrowed_symbol.as_ref().unwrap();
                core::write(symbol, &mut self.writer);
            } else {
                self.gen_stmts(body);
            }
            self.writer.end_decl_func();
        }
    }

    fn visit_variable_decl(
        &mut self,
        stmt: &Stmt,
        _name: &Token,
        _kind: &Option<Expr>,
        value: &Option<Expr>,
    ) -> Self::StmtResult {
        let borrowed_symbol = stmt.symbol.borrow();
        let var_symbol = borrowed_symbol.as_ref().unwrap();

        let borrowed_type = stmt.stmt_type.borrow();
        let var_type = borrowed_type.as_ref().unwrap();

        self.writer.decl_var(var_type, &var_symbol.mangled());

        if let Some(value) = value.as_ref() {
            let value = value.accept(self);
            self.writer.write_assignment(var_symbol.mangled(), value);
        }
    }

    fn visit_if_stmt(
        &mut self,
        _stmt: &Stmt,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) -> Self::StmtResult {
        let condition = condition.accept(self);
        self.writer.start_if_block(condition);
        self.gen_stmts(body);
        if !else_body.is_empty() {
            self.writer.start_else_block();
            self.gen_stmts(else_body);
        }
        self.writer.end_conditional_block();
    }

    fn visit_return_stmt(&mut self, _stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult {
        let val = expr.as_ref().map(|e| e.accept(self));
        self.writer.write_return(val)
    }

    fn visit_print_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult {
        let borrowed_type = stmt.stmt_type.borrow();

        if let Some(expr) = expr.as_ref() {
            let expr_str = expr.accept(self);
            self.writer
                .write_print(Some((borrowed_type.as_ref().unwrap(), expr_str)));
        } else {
            self.writer.write_print(None);
        }
    }

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, expr: &Expr) -> Self::StmtResult {
        let line = format!("{};", expr.accept(self));
        self.writer.writeln(&line);
    }

    fn visit_builtin_stmt(&mut self, _stmt: &Stmt, inner: &Box<Stmt>) {
        self.is_builtin = true;
        inner.accept(self);
        self.is_builtin = false;
    }

}

impl ExprVisitor for Codegen {
    type ExprResult = String;

    fn visit_assignment_expr(
        &mut self,
        _expr: &Expr,
        target: &Expr,
        value: &Expr,
    ) -> Self::ExprResult {
        format!("{} = {}", target.accept(self), value.accept(self))
    }

    fn visit_binary_expr(
        &mut self,
        _expr: &Expr,
        lhs: &Expr,
        op: &Token,
        rhs: &Expr,
    ) -> Self::ExprResult {
        format!(
            "({}) {} ({})",
            lhs.accept(self),
            op.lexeme(),
            rhs.accept(self)
        )
    }

    fn visit_unary_expr(&mut self, _expr: &Expr, op: &Token, operand: &Expr) -> Self::ExprResult {
        format!("{}({})", op.lexeme(), operand.accept(self))
    }

    fn visit_call_expr(&mut self, _expr: &Expr, target: &Expr, args: &[Expr]) -> Self::ExprResult {
        let mut args: Vec<String> = args.iter().map(|a| a.accept(self)).collect();

        match &target.kind {
            ExprKind::Field(field_target, _) => {
                let ptr = format!("&({})", field_target.accept(self));
                args.insert(0, ptr);
            }
            ExprKind::Variable(_) => (),
            _ => unimplemented!(),
        }

        let borrowed_symbol = target.symbol.borrow();
        let mut symbol = borrowed_symbol.as_ref().unwrap().clone();

        if let Some(NodeType::Metatype(metatype_symbol)) = self.global_symbols.get_type(&symbol) {
            symbol = Symbol::new_str(Some(metatype_symbol), "init");
        }

        format!(
            "{}({})",
            symbol.mangled(),
            args.join(",")
        )
    }

    fn visit_field_expr(&mut self, expr: &Expr, target: &Expr, _field: &Token) -> Self::ExprResult {
        let borrowed_symbol = expr.symbol.borrow();
        let symbol = borrowed_symbol.as_ref().unwrap();
        match self.global_symbols.get_type(symbol) {
            Some(NodeType::Function(_, _)) => symbol.mangled(),
            Some(NodeType::Pointer(_)) => {
                format!("{}->{}", target.accept(self), symbol.mangled())
            },
            _ => format!("{}.{}", target.accept(self), symbol.mangled()),
        }
    }

    fn visit_literal_expr(&mut self, _expr: &Expr, token: &Token) -> Self::ExprResult {
        String::from(token.lexeme())
    }

    fn visit_variable_expr(&mut self, expr: &Expr, _name: &Token) -> Self::ExprResult {
        let borrowed_symbol = expr.symbol.borrow();
        let symbol = borrowed_symbol.as_ref().unwrap();
        if self
            .current_type
            .as_ref()
            .map(|t| t.owns(symbol))
            .unwrap_or(false)
        {
            format!("self->{}", symbol.mangled())
        } else {
            symbol.mangled()
        }
    }

    fn visit_explicit_type_expr(&mut self, _expr: &Expr, _name: &Token, _modifier: &Option<Token>) -> Self::ExprResult {
        unreachable!()
    }

}

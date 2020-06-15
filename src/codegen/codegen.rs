use super::c_writer::*;
use super::core;
use crate::analysis::*;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use std::fs::{self, File};
use std::process::Command;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(PartialEq)]
enum CodegenStage {
    ForwardStructDecls,
    StructBodies,
    FuncPrototypes,
    FuncBodies,
}

pub struct Codegen {
    writer: CWriter,
    lib: Rc<Lib>,
    current_type: Option<Symbol>,
    stage: CodegenStage,
    is_builtin: bool,
}

impl Codegen {
    pub fn generate(lib: Lib) {
        fs::create_dir_all("build").unwrap();

        let lib = Rc::new(lib);
        let file = File::create("build/main.c").unwrap();
        let writer = CWriter::new(file);

        Codegen::write(lib, writer);

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

    fn write(lib: Rc<Lib>, writer: CWriter) -> CWriter {
        let mut writer = writer;
        for dep in &lib.dependencies {
            writer = Codegen::write(Rc::clone(dep), writer);
        }

        writer.writeln(&format!("\n// {}", lib.name));

        let mut codegen = Codegen {
            writer: writer,
            lib: Rc::clone(&lib),
            current_type: None,
            stage: CodegenStage::ForwardStructDecls,
            is_builtin: false,
        };

        codegen.stage = CodegenStage::ForwardStructDecls;
        codegen.gen_stmts(&lib.type_decls);
        codegen.stage = CodegenStage::StructBodies;
        codegen.gen_stmts(&lib.type_decls);

        codegen.stage = CodegenStage::FuncPrototypes;
        codegen.gen_stmts(&lib.type_decls);
        codegen.gen_stmts(&lib.function_decls);

        codegen.stage = CodegenStage::FuncBodies;
        codegen.gen_stmts(&lib.type_decls);
        codegen.gen_stmts(&lib.function_decls);

        if !lib.other.is_empty() {
            codegen.write_main(&lib.other);
        }

        codegen.writer
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

    fn handle_expr_breakdown(&mut self, breakdown: ExprBreakdown) -> String {
        for temp in breakdown.temps {
            self.writer.writeln(&temp);
        }
        breakdown.main
    }

    fn decl_symbol(&self, decl: &Stmt) -> Symbol {
        match &decl.kind {
            StmtKind::TypeDecl(name, ..) | StmtKind::FunctionDecl(name, ..) | StmtKind::VariableDecl(name, ..) => {
                name.get_symbol().unwrap()
            },
            _ => panic!(),
        }
    }
}

impl StmtVisitor for Codegen {
    type StmtResult = ();

    fn visit_type_decl(
        &mut self,
        _stmt: &Stmt,
        name: &TypedToken,
        fields: &[Stmt],
        methods: &[Stmt],
        meta_methods: &[Stmt],
    ) -> Self::StmtResult {
        let struct_symbol = name.get_symbol().unwrap();

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
                for method in meta_methods {
                    method.accept(self);
                }

                let meta_symbol = Symbol::meta_symbol(Some(&struct_symbol));
                let init_symbol = Symbol::init_symbol(Some(&meta_symbol));
                let init_type = self.lib.resolve_symbol(&init_symbol).unwrap();
                guard!(NodeType::Function[field_types, ret_type] = init_type);

                let field_list: Vec<_> = field_types
                    .iter()
                    .zip(fields)
                    .map(|(ft, f)| (ft.clone(), self.decl_symbol(f).mangled()))
                    .collect();

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
        _stmt: &Stmt,
        name: &TypedToken,
        params: &[Stmt],
        _return_type: &Option<Expr>,
        body: &[Stmt],
        is_meta: bool,
    ) -> Self::StmtResult {
        let func_symbol = name.get_symbol().unwrap();
        let func_type = name.get_type().unwrap();

        guard!(NodeType::Function[param_types, ret_type] = func_type);
        let ret_type: &NodeType = &ret_type;

        let mut params: Vec<(NodeType, String)> = param_types
            .iter()
            .zip(params)
            .map(|(param_type, param_stmt)| {
                (
                    param_type.clone(),
                    self.decl_symbol(param_stmt).mangled(),
                )
            })
            .collect();

        if let (Some(current_type), false) = (&self.current_type, is_meta) {
            params.insert(
                0,
                (
                    NodeType::Pointer(Box::new(NodeType::Type(current_type.clone()))),
                    String::from("self"),
                ),
            );
        }

        if self.stage == CodegenStage::FuncPrototypes {
            if self.is_builtin {
                if core::should_write_builtin(&func_symbol) {
                    self.writer
                        .write_function_prototype(ret_type, &func_symbol.mangled(), &params);
                }
            } else {
                self.writer
                    .write_function_prototype(ret_type, &func_symbol.mangled(), &params);
            }
        } else {
            if self.is_builtin {
                if core::should_write_builtin(&func_symbol) {
                    self.writer
                        .start_decl_func(ret_type, &func_symbol.mangled(), &params);
                    core::write(&func_symbol, &mut self.writer);
                    self.writer.end_decl_func();
                }
            } else {
                self.writer
                    .start_decl_func(ret_type, &func_symbol.mangled(), &params);
                self.gen_stmts(body);
                self.writer.end_decl_func();
            }
        }
    }

    fn visit_variable_decl(
        &mut self,
        _stmt: &Stmt,
        name: &TypedToken,
        _kind: &Option<Expr>,
        value: &Option<Expr>,
    ) -> Self::StmtResult {
        let var_symbol = name.get_symbol().unwrap();
        let var_type = name.get_type().unwrap();

        self.writer.decl_var(&var_type, &var_symbol.mangled());

        if let Some(value) = value.as_ref() {
            let value = value.accept(self);
            let value = self.handle_expr_breakdown(value);
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
        let condition = self.handle_expr_breakdown(condition);
        self.writer.start_if_block(condition);
        self.gen_stmts(body);
        if !else_body.is_empty() {
            self.writer.start_else_block();
            self.gen_stmts(else_body);
        }
        self.writer.end_conditional_block();
    }

    fn visit_return_stmt(&mut self, _stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult {
        let val = expr.as_ref().map(|e| {
            let val = e.accept(self);
            self.handle_expr_breakdown(val)
        });
        self.writer.write_return(val)
    }

    fn visit_print_stmt(&mut self, _stmt: &Stmt, expr: &Option<Expr>, print_type: &RefCell<Option<NodeType>>) -> Self::StmtResult {
        let borrowed_type = print_type.borrow();

        if let Some(expr) = expr.as_ref() {
            let expr = expr.accept(self);
            let expr_str = self.handle_expr_breakdown(expr);
            self.writer
                .write_print(Some((borrowed_type.as_ref().unwrap(), expr_str)));
        } else {
            self.writer.write_print(None);
        }
    }

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, expr: &Expr) -> Self::StmtResult {
        let expr = expr.accept(self);
        let expr = self.handle_expr_breakdown(expr);
        let line = format!("{};", expr);
        self.writer.writeln(&line);
    }

    fn visit_builtin_stmt(&mut self, _stmt: &Stmt, inner: &Box<Stmt>) {
        self.is_builtin = true;
        inner.accept(self);
        self.is_builtin = false;
    }
}

pub struct ExprBreakdown {
    temps: Vec<String>,
    main: String
}

impl ExprBreakdown {
    fn new(temps: Vec<String>, main: String) -> Self {
        ExprBreakdown {
            temps,
            main,
        }
    }

    fn atom(simple: String) -> Self {
        ExprBreakdown {
            temps: Vec::new(),
            main: simple,
        }
    }

    fn parent(simple: String, children: Vec<ExprBreakdown>) -> Self {
        ExprBreakdown {
            temps: children.into_iter().flat_map(|c| c.temps).collect(),
            main: simple,
        }
    }
}

impl ExprVisitor for Codegen {
    type ExprResult = ExprBreakdown;

    fn visit_assignment_expr(
        &mut self,
        _expr: &Expr,
        target: &Expr,
        value: &Expr,
    ) -> Self::ExprResult {
        let (lhs, rhs) = (target.accept(self), value.accept(self));
        let line = format!("{} = {}", lhs.main, rhs. main);
        ExprBreakdown::parent(line, vec![lhs, rhs])
    }

    fn visit_binary_expr(
        &mut self,
        _expr: &Expr,
        lhs: &Expr,
        op: &Token,
        rhs: &Expr,
    ) -> Self::ExprResult {
        let (lhs, rhs) = (lhs.accept(self), rhs.accept(self));
        let line = format!(
            "({}) {} ({})",
            lhs.main,
            op.lexeme(),
            rhs.main
        );
        ExprBreakdown::parent(line, vec![lhs, rhs])
    }

    fn visit_unary_expr(&mut self, _expr: &Expr, op: &Token, operand: &Expr) -> Self::ExprResult {
        let operand = operand.accept(self);
        let line = format!("{}({})", op.lexeme(), operand.main);
        ExprBreakdown::parent(line, vec![operand])
    }

    fn visit_function_call_expr(&mut self, _expr: &Expr, function: &ResolvedToken, args: &[Expr]) -> Self::ExprResult {
        let args: Vec<ExprBreakdown> = args.iter().map(|a| a.accept(self)).collect();
        let mut arg_strs: Vec<String> = args.iter().map(|a| a.main.clone()).collect();

        let function_symbol = function.get_symbol().unwrap();
        if let Some(parent_symbol) = function_symbol.parent() {
            if let Some(NodeType::Metatype(_)) = self.lib.resolve_symbol(&parent_symbol) {
                arg_strs.insert(0, String::from("self"));
            }
        }

        let line = format!("{}({})", function.get_symbol().unwrap().mangled(), arg_strs.join(","));
        ExprBreakdown::parent(line, args)
    }

    fn visit_method_call_expr(&mut self, _expr: &Expr, object: &Expr, method: &ResolvedToken, args: &[Expr]) -> Self::ExprResult {
        let mut args: Vec<ExprBreakdown> = args.iter().map(|a| a.accept(self)).collect();

        let method_symbol = method.get_symbol().unwrap();

        let mut breakdown = object.accept(self);

        match &object.kind {
            ExprKind::FunctionCall(first_called, _) | ExprKind::MethodCall(_, first_called, _) => {
                let first_called_symbol = first_called.get_symbol().unwrap();
                let first_called_type = self.lib.resolve_symbol(&first_called_symbol).unwrap();
                guard!(NodeType::Function[_params, first_called_ret_type] = first_called_type);
                let ret_type = self.writer.convert_type(first_called_ret_type);

                let temp_name = format!("temp__{}", method_symbol.mangled());

                let temp = format!("{} {} = {};", ret_type, temp_name, breakdown.main);
                breakdown.temps.push(temp);
                breakdown.main = temp_name;
            },
            _ => (),
        }

        if !method_symbol.is_meta_owned() {
            let main = format!("&({})", breakdown.main);
            let object = ExprBreakdown::new(breakdown.temps, main);
            args.insert(0, object);
        }

        let arg_strs: Vec<String> = args.iter().map(|a| a.main.clone()).collect();

        let line = format!("{}({})", method_symbol.mangled(), arg_strs.join(","));
        ExprBreakdown::parent(line, args)
    }

    fn visit_field_expr(&mut self, _whole_expr: &Expr, target: &Expr, field: &ResolvedToken) -> Self::ExprResult {
        let mut target_breakdown = target.accept(self);

        let field_symbol = field.get_symbol().unwrap();

        match &target.kind {
            ExprKind::FunctionCall(function, _) | ExprKind::MethodCall(_, function, _) => {
                let target_symbol = function.get_symbol().unwrap();
                let target_type = self.lib.resolve_symbol(&target_symbol).unwrap();
                guard!(NodeType::Function[_params, ret_type] = target_type);
                let ret_type = self.writer.convert_type(ret_type);

                let temp_name = format!("temp__{}", target_symbol.mangled());

                let temp = format!("{} {} = {};", ret_type, temp_name, target_breakdown.main);
                target_breakdown.temps.push(temp);

                let line = format!("{}.{}", temp_name, field_symbol.mangled());
                ExprBreakdown::new(target_breakdown.temps, line)
            },
            _ => {
                let line = format!("{}.{}", target_breakdown.main, field_symbol.mangled());
                ExprBreakdown::new(target_breakdown.temps, line)
            }
        }
    }

    fn visit_literal_expr(&mut self, _expr: &Expr, token: &Token) -> Self::ExprResult {
        ExprBreakdown::atom(String::from(token.lexeme()))
    }

    fn visit_variable_expr(&mut self, _expr: &Expr, name: &ResolvedToken) -> Self::ExprResult {
        let symbol = name.get_symbol().unwrap();
        if self
            .current_type
            .as_ref()
            .map(|t| t.owns(&symbol))
            .unwrap_or(false)
        {
            ExprBreakdown::atom(format!("self->{}", symbol.mangled()))
        } else {
            ExprBreakdown::atom(symbol.mangled())
        }
    }

    fn visit_explicit_type_expr(
        &mut self,
        _expr: &Expr,
        _name: &Token,
        _modifier: &Option<Token>,
    ) -> Self::ExprResult {
        unreachable!()
    }
}

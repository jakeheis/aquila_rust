use super::c_writer::*;
use super::core;
use crate::analysis::*;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use std::cell::RefCell;
use std::fs::{self, File};
use std::process::Command;
use std::rc::Rc;

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
    temp_count: u32,
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
            temp_count: 0,
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

    fn decl_symbol(&self, decl: &Stmt) -> Symbol {
        match &decl.kind {
            StmtKind::TypeDecl(name, ..)
            | StmtKind::FunctionDecl(name, ..)
            | StmtKind::VariableDecl(name, ..) => name.get_symbol().unwrap(),
            _ => panic!(),
        }
    }

    fn write_temp(&mut self, temp_type: &NodeType, value: String) -> String {
        let temp_name = format!("_temp_{}", self.temp_count);
        self.temp_count += 1;

        let temp_type = self.writer.convert_type(temp_type);

        let temp = format!("{} {} = {};", temp_type, temp_name, value);
        self.writer.writeln(&temp);

        temp_name
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
                    self.writer.decl_var(&ret_type, "new_item", None);
                    for (_, f) in field_list {
                        let target = format!("new_item.{}", f);
                        self.writer.write_assignment(&target, &f);
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
                (param_type.clone(), self.decl_symbol(param_stmt).mangled())
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

        let decl_name = if let NodeType::Array(_) = var_type {
            format!("{}[]", var_symbol.mangled())
        } else {
            var_symbol.mangled()
        };

        let value = value.as_ref().map(|v| v.accept(self));
        self.writer.decl_var(&var_type, &decl_name, value);
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

    fn visit_print_stmt(
        &mut self,
        _stmt: &Stmt,
        expr: &Option<Expr>,
        print_type: &RefCell<Option<NodeType>>,
    ) -> Self::StmtResult {
        let borrowed_type = print_type.borrow();

        if let Some(expr) = expr.as_ref() {
            let expr = expr.accept(self);
            self.writer
                .write_print(Some((borrowed_type.as_ref().unwrap(), expr)));
        } else {
            self.writer.write_print(None);
        }
    }

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, expr: &Expr) -> Self::StmtResult {
        let expr = expr.accept(self);
        let line = format!("{};", expr);
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

    fn visit_function_call_expr(
        &mut self,
        _expr: &Expr,
        function: &ResolvedToken,
        args: &[Expr],
    ) -> Self::ExprResult {
        let mut args: Vec<String> = args.iter().map(|a| a.accept(self)).collect();

        let function_symbol = function.get_symbol().unwrap();
        if let Some(parent_symbol) = function_symbol.parent() {
            if let Some(NodeType::Metatype(_)) = self.lib.resolve_symbol(&parent_symbol) {
                args.insert(0, String::from("self"));
            }
        }

        format!(
            "{}({})",
            function.get_symbol().unwrap().mangled(),
            args.join(",")
        )
    }

    fn visit_method_call_expr(
        &mut self,
        _expr: &Expr,
        object: &Expr,
        method: &ResolvedToken,
        args: &[Expr],
    ) -> Self::ExprResult {
        let mut args: Vec<String> = args.iter().map(|a| a.accept(self)).collect();

        let method_symbol = method.get_symbol().unwrap();

        let object_str = object.accept(self);

        let object_str = match &object.kind {
            ExprKind::FunctionCall(first_called, _) | ExprKind::MethodCall(_, first_called, _) => {
                let first_called_symbol = first_called.get_symbol().unwrap();
                let first_called_type = self.lib.resolve_symbol(&first_called_symbol).unwrap();
                guard!(NodeType::Function[_params, first_called_ret_type] = first_called_type);
                let first_called_ret_type = (**first_called_ret_type).clone();

                self.write_temp(&first_called_ret_type, object_str)
            }
            _ => object_str,
        };

        if !method_symbol.is_meta_owned() {
            let main = format!("&({})", object_str);
            args.insert(0, main);
        }

        format!("{}({})", method_symbol.mangled(), args.join(","))
    }

    fn visit_field_expr(
        &mut self,
        _whole_expr: &Expr,
        target: &Expr,
        field: &ResolvedToken,
    ) -> Self::ExprResult {
        let target_str = target.accept(self);

        let field_symbol = field.get_symbol().unwrap();

        match &target.kind {
            ExprKind::FunctionCall(function, _) | ExprKind::MethodCall(_, function, _) => {
                let target_symbol = function.get_symbol().unwrap();
                let target_type = self.lib.resolve_symbol(&target_symbol).unwrap();
                guard!(NodeType::Function[_params, ret_type] = target_type);
                let ret_type = (**ret_type).clone();

                let temp_name = self.write_temp(&ret_type, target_str);

                format!("{}.{}", temp_name, field_symbol.mangled())
            }
            _ => format!("{}.{}", target_str, field_symbol.mangled()),
        }
    }

    fn visit_literal_expr(&mut self, _expr: &Expr, token: &Token) -> Self::ExprResult {
        String::from(token.lexeme())
    }

    fn visit_variable_expr(&mut self, _expr: &Expr, name: &ResolvedToken) -> Self::ExprResult {
        let symbol = name.get_symbol().unwrap();
        if self
            .current_type
            .as_ref()
            .map(|t| t.owns(&symbol))
            .unwrap_or(false)
        {
            format!("self->{}", symbol.mangled())
        } else {
            symbol.mangled()
        }
    }

    fn visit_array_expr(&mut self, _expr: &Expr, elements: &[Expr]) -> Self::ExprResult {
        let elements: Vec<String> = elements.iter().map(|e| e.accept(self)).collect();
        format!("{{ {} }}", elements.join(","))
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

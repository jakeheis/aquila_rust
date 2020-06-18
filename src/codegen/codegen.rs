use super::c_writer::*;
use super::core;
use crate::analysis::*;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::*;
use std::cell::RefCell;
use std::collections::HashMap;
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
    current_func: Option<Symbol>,
    stage: CodegenStage,
    is_builtin: bool,
    temp_count: u32,
    specialization_map: HashMap<Symbol, NodeType>,
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
            current_func: None,
            stage: CodegenStage::ForwardStructDecls,
            is_builtin: false,
            temp_count: 0,
            specialization_map: HashMap::new(),
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

        let temp_type = self.writer.type_and_name(temp_type, &temp_name);
        let temp = format!("{} = {};", temp_type, value);
        self.writer.writeln(&temp);

        temp_name
    }

    fn write_guard<T: ContainsSpan>(&mut self, guard: String, message: &str, span: &T) {
        self.writer.start_condition_block("if", guard);

        let message = format!(
            "\\nFatal error: {}\\n\\n{}\\n",
            message,
            span.span().location(),
        );

        self.writer.writeln(&format!("printf(\"{}\\n\");", message));
        self.writer.writeln("exit(1);");
        self.writer.end_conditional_block();
    }

    fn flatten_generic_opt(&self, function: &Option<Symbol>, node_type: &NodeType) -> NodeType {
        if let Some(function) = function.as_ref() {
            self.flatten_generic(function, node_type)
        } else {
            node_type.clone()
        }
    }

    fn flatten_generic(&self, function: &Symbol, node_type: &NodeType) -> NodeType {
        match &node_type {
            NodeType::Type(ty) => {
                // let meta_symbol =
                if function.owns(ty) && self.lib.resolve_symbol(&ty).is_some() {
                    NodeType::Void
                } else {
                    node_type.clone()
                }
            }
            NodeType::Pointer(to) => NodeType::pointer_to(self.flatten_generic(function, to)),
            NodeType::Array(of, size) => {
                let of = self.flatten_generic(function, of);
                NodeType::Array(Box::new(of), *size)
            }
            _ => node_type.clone(),
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
        generics: &[TypedToken],
        params: &[Stmt],
        _return_type: &Option<ExplicitType>,
        body: &[Stmt],
        is_meta: bool,
    ) -> Self::StmtResult {
        let func_symbol = name.get_symbol().unwrap();
        let func_type = name.get_type().unwrap();

        self.current_func = Some(func_symbol.clone());

        guard!(NodeType::Function[param_types, ret_type] = func_type);
        let ret_type: &NodeType = &ret_type;
        let ret_type = self.flatten_generic(&func_symbol, ret_type);

        let mut params: Vec<(NodeType, String)> = param_types
            .iter()
            .zip(params)
            .map(|(param_type, param_stmt)| {
                let param_symbol = self.decl_symbol(param_stmt);

                let param_type = self.flatten_generic(&func_symbol, param_type);
                // if self.lib.resolve_symbol(&param_symbol)
                println!("param {} is typed {}", param_symbol, param_type);
                // if param_symbol.is_generic() {
                //     (NodeType::pointer_to(NodeType::Void), param_symbol.mangled())
                // } else {
                (param_type.clone(), param_symbol.mangled())
                // }
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
            println!("Writing prototypes for {}", func_symbol);

            if self.is_builtin {
                if core::should_write_builtin(&func_symbol) {
                    println!("trying to actually write builtin {}", func_symbol);
                    self.writer.write_function_prototype(
                        &ret_type,
                        &func_symbol.mangled(),
                        &params,
                    );
                }
            } else {
                self.writer
                    .write_function_prototype(&ret_type, &func_symbol.mangled(), &params);
            }
        } else {
            if self.is_builtin {
                if core::should_write_builtin(&func_symbol) {
                    self.writer
                        .start_decl_func(&ret_type, &func_symbol.mangled(), &params);
                    core::write(&func_symbol, &mut self.writer);
                    self.writer.end_decl_func();
                }
            } else {
                self.writer
                    .start_decl_func(&ret_type, &func_symbol.mangled(), &params);
                self.gen_stmts(body);
                self.writer.end_decl_func();
            }
        }

        self.current_func = None;
    }

    fn visit_variable_decl(
        &mut self,
        _stmt: &Stmt,
        name: &TypedToken,
        _kind: &Option<ExplicitType>,
        value: &Option<Expr>,
    ) -> Self::StmtResult {
        let var_symbol = name.get_symbol().unwrap();
        let mut var_type = name.get_type().unwrap();

        if let NodeType::Array(of, _) = var_type {
            var_type = NodeType::Pointer(of.clone());
        }

        println!(
            "writing {} as kind {} in {:?}",
            var_symbol, var_type, self.current_func
        );

        let var_type = self.flatten_generic_opt(&self.current_func, &var_type);

        let value = value.as_ref().map(|v| v.accept(self));
        self.writer
            .decl_var(&var_type, &var_symbol.mangled(), value);
    }

    fn visit_if_stmt(
        &mut self,
        _stmt: &Stmt,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) -> Self::StmtResult {
        let condition = condition.accept(self);
        self.writer.start_condition_block("if", condition);
        self.gen_stmts(body);
        if !else_body.is_empty() {
            self.writer.start_else_block();
            self.gen_stmts(else_body);
        }
        self.writer.end_conditional_block();
    }

    fn visit_while_stmt(&mut self, _stmt: &Stmt, condition: &Expr, body: &[Stmt]) {
        let condition = condition.accept(self);
        self.writer.start_condition_block("while", condition);
        self.gen_stmts(body);
        self.writer.end_conditional_block();
    }

    fn visit_for_stmt(
        &mut self,
        _stmt: &Stmt,
        variable: &TypedToken,
        array_expr: &Expr,
        body: &[Stmt],
    ) {
        let array = array_expr.accept(self);
        guard!(NodeType::Array[of, count] = array_expr.get_type().unwrap());

        let of: &NodeType = &of;

        let condition = format!("int i = 0; i < {}; i++", count);
        self.writer.start_condition_block("for", condition);

        let subscript = format!("&{}[i]", array);
        self.writer.decl_var(
            &NodeType::pointer_to(of.clone()),
            &variable.get_symbol().unwrap().mangled(),
            Some(subscript),
        );
        self.gen_stmts(body);
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
        let operand_text = operand.accept(self);

        let operand_text = if let &TokenKind::Ampersand = &op.kind {
            self.write_temp(
                &operand.get_type().unwrap().coerce_array_to_ptr(),
                operand_text,
            )
        } else {
            operand_text
        };

        format!("{}({})", op.lexeme(), operand_text)
    }

    fn visit_function_call_expr(
        &mut self,
        _expr: &Expr,
        target: Option<&Expr>,
        function: &ResolvedToken,
        specializations: &[ExplicitType],
        args: &[Expr],
    ) -> Self::ExprResult {
        let function_symbol = function.get_symbol().unwrap();
        let mut args: Vec<String> = args.iter().map(|a| a.accept(self)).collect();

        let metadata = self.lib.function_metadata(&function_symbol).unwrap();
        let resolved = self.lib.resolve_symbol(&function_symbol).unwrap();
        guard!(NodeType::Function[_one, _two] = resolved);

        for (specialization, generic) in specializations.iter().zip(&metadata.generics) {
            let gen_type = specialization.guarantee_resolved();
            self.specialization_map.insert(generic.clone(), gen_type);
        }

        let rendered = if let Some(target) = target {
            let target_str = target.accept(self);
            let target_str = match &target.kind {
                ExprKind::FunctionCall(_, first_called, _, _) => {
                    let first_called_symbol = first_called.get_symbol().unwrap();
                    let first_called_type = self.lib.resolve_symbol(&first_called_symbol).unwrap();
                    guard!(NodeType::Function[_params, first_called_ret_type] = first_called_type);

                    self.write_temp(&first_called_ret_type, target_str)
                }
                _ => target_str,
            };

            if !function_symbol.is_meta_owned() {
                let main = format!("&({})", target_str);
                args.insert(0, main);
            }

            format!("{}({})", function_symbol.mangled(), args.join(","))
        } else {
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
        };

        self.specialization_map.clear();

        rendered
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
            ExprKind::FunctionCall(_, function, _, _) => {
                let target_symbol = function.get_symbol().unwrap();
                let target_type = self.lib.resolve_symbol(&target_symbol).unwrap();
                guard!(NodeType::Function[_params, ret_type] = target_type);

                let temp_name = self.write_temp(&ret_type, target_str);

                format!("{}.{}", temp_name, field_symbol.mangled())
            }
            _ => format!("{}.{}", target_str, field_symbol.mangled()),
        }
    }

    fn visit_literal_expr(&mut self, _expr: &Expr, token: &Token) -> Self::ExprResult {
        String::from(token.lexeme())
    }

    fn visit_variable_expr(&mut self, expr: &Expr, name: &ResolvedToken) -> Self::ExprResult {
        let symbol = name.get_symbol().unwrap();
        let expr_type = expr.get_type().unwrap();

        if let Some(mapped) = self.specialization_map.get(&symbol) {
            println!("FOUND MAPPED {}", mapped);
            return mapped.to_string();
        }

        println!(
            "Expr {} type {} map {}",
            symbol,
            expr_type,
            self.specialization_map.len()
        );
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

    fn visit_array_expr(&mut self, expr: &Expr, elements: &[Expr]) -> Self::ExprResult {
        let elements: Vec<String> = elements.iter().map(|e| e.accept(self)).collect();
        self.write_temp(
            expr.get_type().as_ref().unwrap(),
            format!("{{ {} }}", elements.join(",")),
        )
    }

    fn visit_subscript_expr(
        &mut self,
        _expr: &Expr,
        target_expr: &Expr,
        index_expr: &Expr,
    ) -> Self::ExprResult {
        let target = target_expr.accept(self);
        let arg = index_expr.accept(self);
        let index = self.write_temp(&NodeType::Int, arg);

        guard!(NodeType::Array[_inside, count] = target_expr.get_type().unwrap());
        self.write_guard(
            format!("{} >= {}", index, count),
            "index out of bounds",
            index_expr,
        );

        format!("{}[{}]", target, index)
    }

    fn visit_cast_expr(
        &mut self,
        _expr: &Expr,
        explicit_type: &ExplicitType,
        value: &Expr,
    ) -> Self::ExprResult {
        let cast_type = explicit_type.guarantee_resolved();
        let cast_type = self.flatten_generic_opt(&self.current_func, &cast_type);
        let (cast_type, _) = self.writer.convert_type(&cast_type, String::new());
        let value = value.accept(self);

        format!("({})({})", cast_type, value)
    }
}

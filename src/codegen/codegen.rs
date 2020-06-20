use super::c_writer::*;
use super::core;
use crate::analysis::*;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use log::trace;
use std::fs::{self, File};
use std::process::Command;
use std::rc::Rc;
// use crate::source::*;

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
    specialization: Option<GenericSpecialization>,
}

impl Codegen {
    pub fn generate(lib: Lib) {
        fs::create_dir_all("build").unwrap();

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

    fn write(mut lib: Lib, writer: CWriter) {
        SpecializationPropagator::propogate(&mut lib);

        let mut writer = writer;

        writer.writeln(&format!("\n// {}", lib.name));

        let lib = Rc::new(lib);

        let mut codegen = Codegen {
            writer: writer,
            lib: Rc::clone(&lib),
            current_type: None,
            stage: CodegenStage::ForwardStructDecls,
            is_builtin: false,
            temp_count: 0,
            specialization: None,
        };

        let lib = lib.as_ref();

        codegen.stage = CodegenStage::ForwardStructDecls;
        codegen.chunk(lib, &mut |codegen, lib| {
            codegen.gen_type_decls(&lib.type_decls);
        });

        codegen.stage = CodegenStage::StructBodies;
        codegen.chunk(lib, &mut |codegen, lib| {
            codegen.gen_type_decls(&lib.type_decls);
        });

        codegen.stage = CodegenStage::FuncPrototypes;
        codegen.chunk(lib, &mut |codegen, lib| {
            codegen.gen_type_decls(&lib.type_decls);
            codegen.gen_stmts(&lib.function_decls);
        });

        codegen.stage = CodegenStage::FuncBodies;
        codegen.chunk(lib, &mut |codegen, lib| {
            codegen.gen_type_decls(&lib.type_decls);
            codegen.gen_stmts(&lib.function_decls);
        });

        if !lib.other.is_empty() {
            codegen.write_main(&lib.other);
        }
    }

    fn chunk<F>(&mut self, lib: &Lib, function: &mut F)
    where
        F: Fn(&mut Codegen, &Lib) -> (),
    {
        for dep in &lib.dependencies {
            self.chunk(dep, function);
        }
        function(self, lib);
    }

    fn write_main(&mut self, stmts: &[Stmt]) {
        let main_func = FunctionMetadata {
            symbol: Symbol::main_symbol(),
            kind: FunctionKind::TopLevel,
            generics: Vec::new(),
            parameter_symbols: Vec::new(),
            parameter_types: Vec::new(),
            return_type: NodeType::Int,
            specializations: Vec::new(),
        };

        self.writer.start_decl_func(&main_func, None);
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

    fn gen_type_decls(&mut self, decls: &[TypeDecl]) {
        for decl in decls {
            self.visit_type_decl(decl);
        }
    }

    fn write_temp(&mut self, temp_type: &NodeType, value: String) -> String {
        let temp_name = format!("_temp_{}", self.temp_count);
        self.temp_count += 1;

        let temp_type = temp_type.specialize_opt(self.specialization.as_ref());
        let temp_type = self.writer.type_and_name(&temp_type, &temp_name);
        let temp = format!("{} = {};", temp_type, value);
        self.writer.writeln(&temp);

        temp_name
    }

    fn write_function(
        &mut self,
        func_symbol: &Symbol,
        body: &[Stmt],
        specialization: Option<&GenericSpecialization>,
    ) {
        if !core::should_write_builtin(&func_symbol) {
            return;
        }

        let func_metadata = self.lib.function_metadata(&func_symbol).unwrap();

        let message = if let Some(specialization) = specialization {
            format!(
                "Writing function {} with specialization {}",
                func_symbol, specialization
            )
        } else {
            format!("Writing function {}", func_symbol)
        };
        trace!(target: "codegen", "{}", message);

        self.specialization = specialization.map(|s| s.clone());

        if self.stage == CodegenStage::FuncPrototypes {
            self.writer
                .write_function_prototype(&func_metadata, specialization);
        } else {
            self.writer.start_decl_func(&func_metadata, specialization);
            if self.is_builtin {
                core::write(&func_symbol, &mut self.writer);
            } else {
                self.gen_stmts(body);
            }
            self.writer.end_decl_func();
        }

        self.specialization = None;
    }
}

impl StmtVisitor for Codegen {
    type StmtResult = ();

    fn visit_type_decl(
        &mut self,
        decl: &TypeDecl
    ) -> Self::StmtResult {
        let type_symbol = decl.name.get_symbol().unwrap();
        let type_metadata = self.lib.type_metadata(&type_symbol).unwrap();

        match self.stage {
            CodegenStage::ForwardStructDecls => self
                .writer
                .write_struct_forward_decl(&type_symbol.mangled()),
            CodegenStage::StructBodies => {
                self.writer.start_decl_struct(&type_symbol.mangled());
                for field in &decl.fields {
                    field.accept(self);
                }
                self.writer.end_decl_struct(&type_symbol.mangled());
            }
            CodegenStage::FuncPrototypes | CodegenStage::FuncBodies => {
                self.current_type = Some(type_symbol.clone());
                for method in &decl.methods {
                    method.accept(self);
                }
                for method in &decl.meta_methods {
                    method.accept(self);
                }

                let meta_symbol = Symbol::meta_symbol(Some(&type_symbol));
                let init_symbol = Symbol::init_symbol(Some(&meta_symbol));
                let init_metadata = self.lib.function_metadata(&init_symbol).unwrap();

                if let CodegenStage::FuncPrototypes = self.stage {
                    self.writer.write_function_prototype(&init_metadata, None);
                } else {
                    self.writer.start_decl_func(&init_metadata, None);
                    self.writer
                        .decl_var(&init_metadata.return_type, "new_item", None);
                    for field in type_metadata.field_symbols {
                        let target = format!("new_item.{}", field.mangled());
                        self.writer.write_assignment(&target, &field.mangled());
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
        name: &TypedToken,
        generics: &[TypedToken],
        _params: &[Stmt],
        _return_type: &Option<ExplicitType>,
        body: &[Stmt],
        _is_meta: bool,
    ) -> Self::StmtResult {
        let func_symbol = name.get_symbol().unwrap();

        let func_metadata = self.lib.function_metadata(&func_symbol).unwrap();

        if !func_metadata.specializations.is_empty() {
            for spec in &func_metadata.specializations {
                self.write_function(&func_symbol, body, Some(spec));
            }
        } else if generics.is_empty() {
            self.write_function(&func_symbol, body, None);
        }
    }

    fn visit_variable_decl(
        &mut self,
        name: &TypedToken,
        _kind: &Option<ExplicitType>,
        value: &Option<Expr>,
    ) -> Self::StmtResult {
        let var_symbol = name.get_symbol().unwrap();
        let mut var_type = name.get_type().unwrap();

        if let NodeType::Array(of, _) = var_type {
            var_type = NodeType::Pointer(of.clone());
        }

        let var_type = var_type.specialize_opt(self.specialization.as_ref());

        let value = value.as_ref().map(|v| v.accept(self));
        self.writer
            .decl_var(&var_type, &var_symbol.mangled(), value);
    }

    fn visit_trait_decl(
        &mut self,
        _name: &TypedToken,
        _requirements: &[Stmt],
    ) {
        // TODO
    }

    fn visit_if_stmt(
        &mut self,
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

    fn visit_while_stmt(&mut self, condition: &Expr, body: &[Stmt]) {
        let condition = condition.accept(self);
        self.writer.start_condition_block("while", condition);
        self.gen_stmts(body);
        self.writer.end_conditional_block();
    }

    fn visit_for_stmt(
        &mut self,
        variable: &TypedToken,
        array_expr: &Expr,
        body: &[Stmt],
    ) {
        let array = array_expr.accept(self);
        guard!(NodeType::Array[_of, count] = array_expr.get_type().unwrap());

        let condition = format!("int i = 0; i < {}; i++", count);
        self.writer.start_condition_block("for", condition);

        let subscript = format!("&{}[i]", array);
        self.writer.decl_var(
            &variable.get_type().unwrap(),
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

    fn visit_print_stmt(&mut self, expr: &Option<Expr>) -> Self::StmtResult {
        if let Some(expr) = expr.as_ref() {
            let expr_str = expr.accept(self);
            let print_type = expr.get_type().unwrap();
            self.writer.write_print(Some((&print_type, expr_str)));
        } else {
            self.writer.write_print(None);
        }
    }

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Self::StmtResult {
        let expr = expr.accept(self);
        let line = format!("{};", expr);
        self.writer.writeln(&line);
    }

    fn visit_builtin_stmt(&mut self, inner: &Box<Stmt>) {
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
        let function_metadata = self.lib.function_metadata(&function_symbol).unwrap();

        let specialization = if specializations.is_empty() {
            let arg_types: Vec<_> = args.iter().map(|a| a.get_type().unwrap()).collect();
            GenericSpecialization::infer(&function_metadata, &arg_types)
                .ok()
                .unwrap()
        } else {
            let explicit_types: Vec<_> = specializations
                .iter()
                .map(|s| s.guarantee_resolved())
                .collect::<Vec<_>>();
            GenericSpecialization::new(&function_symbol, explicit_types)
        };

        let specialization = if let Some(caller_specs) = self.specialization.as_ref() {
            specialization.resolve_generics_using(caller_specs)
        } else {
            specialization
        };

        let function_name = specialization.id;
        let mut args: Vec<String> = args.iter().map(|a| a.accept(self)).collect();

        let rendered = if let Some(target) = target {
            let target_str = target.accept(self);
            let target_str = match &target.kind {
                ExprKind::FunctionCall(..) => {
                    self.write_temp(&target.get_type().unwrap(), target_str)
                }
                _ => target_str,
            };

            if let FunctionKind::Method(..) = function_metadata.kind {
                let main = format!("&({})", target_str);
                args.insert(0, main);
            }

            format!("{}({})", function_name, args.join(","))
        } else {
            if let FunctionKind::Method(..) = function_metadata.kind {
                args.insert(0, String::from("self"));
            }

            format!("{}({})", function_name, args.join(","))
        };

        rendered
    }

    fn visit_field_expr(
        &mut self,
        _expr: &Expr,
        target: &Expr,
        field: &ResolvedToken,
    ) -> Self::ExprResult {
        let target_str = target.accept(self);

        let field_symbol = field.get_symbol().unwrap();

        match &target.kind {
            ExprKind::FunctionCall(..) => {
                let temp_name = self.write_temp(&target.get_type().unwrap(), target_str);
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

        if let NodeType::GenericMeta(_, num) = expr_type {
            let specialized_type = &self.specialization.as_ref().unwrap().node_types[num];
            return self.writer.convert_type(specialized_type, String::new()).0;
        }

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
        self.writer.write_guard(
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
        let cast_type = cast_type.specialize_opt(self.specialization.as_ref());
        let (cast_type, _) = self.writer.convert_type(&cast_type, String::new());
        let value = value.accept(self);

        format!("({})({})", cast_type, value)
    }
}

use super::expr_checker::*;
use super::{check, ContextTracker, ScopeType};
use crate::diagnostic::*;
use crate::library::*;
use crate::parsing::*;
use std::rc::Rc;

pub struct TypeChecker {
    reporter: Rc<dyn Reporter>,
    lib: Rc<Lib>,
    context: ContextTracker,
}

pub struct Analysis {
    guarantees_return: bool,
}

impl TypeChecker {
    pub fn check(lib: Lib, reporter: Rc<dyn Reporter>) -> Lib {
        let lib = Rc::new(lib);

        let mut checker = TypeChecker {
            reporter,
            lib: Rc::clone(&lib),
            context: ContextTracker::new(Rc::clone(&lib)),
        };

        for decl in &lib.type_decls {
            checker.check_type_decl(decl);
        }
        for decl in &lib.function_decls {
            checker.check_function_decl(decl);
        }
        for decl in &lib.conformance_decls {
            checker.check_conformance_decl(decl);
        }

        let main_func = FunctionMetadata::main();
        checker.context.push_scope(
            main_func.symbol.clone(),
            ScopeType::InsideFunction(main_func),
        );
        checker.check_list(&lib.main);
        checker.context.pop_scope();

        std::mem::drop(checker);

        let lib = Rc::try_unwrap(lib).ok().unwrap();

        lib
    }

    fn check_list(&mut self, stmt_list: &[Stmt]) -> Analysis {
        let mut warned_unused = false;
        let mut guarantees_return = false;
        for stmt in stmt_list {
            if guarantees_return && warned_unused == false {
                self.reporter
                    .report(Diagnostic::warning(stmt, "Code will never be executed"));
                warned_unused = true;
            }
            let analysis = stmt.accept(self);
            if analysis.guarantees_return {
                guarantees_return = true;
            }
        }
        Analysis { guarantees_return }
    }

    fn check_expr(&mut self, expr: &Expr) -> Option<NodeType> {
        let context =
            std::mem::replace(&mut self.context, ContextTracker::new(Rc::clone(&self.lib)));
        let mut expr_checker = ExprChecker::new(Rc::clone(&self.lib), context);
        let result = expr.accept(&mut expr_checker);
        std::mem::swap(&mut self.context, &mut expr_checker.context);
        match result {
            Ok(t) => Some(t),
            Err(diag) => {
                self.report_error(diag);
                None
            }
        }
    }

    fn report_error(&self, diag: Diagnostic) {
        self.reporter.report(diag);
    }

    fn check_type_decl(&mut self, decl: &TypeDecl) -> Analysis {
        let (type_symbol, metadata) = self.context.push_type_scope(&decl.name);

        self.context.push_scope_meta(&metadata);
        for meta_method in &metadata.meta_methods {
            let method_metadata = self.lib.function_metadata(&meta_method).unwrap();
            self.context
                .put_in_scope(meta_method, &method_metadata.node_type());
        }
        for meta_method in &decl.meta_methods {
            self.check_function_decl(meta_method);
        }
        self.context.pop_scope();

        for (symbol, field_type) in metadata.field_symbols.iter().zip(&metadata.field_types) {
            self.context.put_in_scope(symbol, field_type);
        }

        let self_symbol = Symbol::self_symbol(&type_symbol);
        self.context
            .put_in_scope(&self_symbol, &metadata.unspecialized_type());

        for method in &metadata.methods {
            let method_metadata = self.lib.function_metadata(&method).unwrap();
            self.context
                .put_in_scope(method, &method_metadata.node_type());
        }
        for method in &decl.methods {
            self.check_function_decl(method);
        }

        self.context.pop_scope();

        Analysis {
            guarantees_return: false,
        }
    }

    fn check_function_decl(&mut self, decl: &FunctionDecl) -> Analysis {
        let (func_symbol, metadata) = self.context.push_function_scope(&decl.name);
        decl.name.set_symbol(func_symbol.clone());

        if decl.is_builtin {
            self.context.pop_scope();
            return Analysis {
                guarantees_return: false,
            };
        }

        let return_type = match &metadata.return_type {
            NodeType::Array(..) => {
                self.report_error(Diagnostic::error(
                    decl.return_type.as_ref().unwrap(),
                    "Cannot return an array",
                ));
                &NodeType::Ambiguous
            }
            NodeType::Ambiguous => {
                self.report_error(Diagnostic::error(
                    decl.return_type.as_ref().unwrap(),
                    "Undefined type",
                ));
                &NodeType::Ambiguous
            }
            value => value,
        };

        for (symbol, param_type) in metadata
            .parameter_symbols
            .iter()
            .zip(&metadata.parameter_types)
        {
            self.context.put_in_scope(symbol, param_type);
        }

        if decl.include_caller {
            let symbol  = Symbol::new_str(&func_symbol, "caller");
            self.context.put_in_scope(&symbol, &NodeType::pointer_to(NodeType::Byte));
        }

        for (index, param_type) in metadata.parameter_types.iter().enumerate() {
            if let NodeType::Array(_, 0) = param_type {
                self.report_error(Diagnostic::error(
                    &decl.parameters[index],
                    "A zero length array cannot be a parameter",
                ));
            }
        }

        let analysis = self.check_list(&decl.body);

        self.context.pop_scope();

        if !analysis.guarantees_return && !return_type.matches(&NodeType::Void) {
            let last_param = decl.parameters.last().map(|p| p.span().clone());
            let span_params = Span::join_opt(&decl.name, &last_param);
            let span = Span::join_opt(&span_params, &decl.return_type);
            self.report_error(Diagnostic::error(&span, "Function may not return"));
        }

        Analysis {
            guarantees_return: false,
        }
    }

    fn check_conformance_decl(&mut self, decl: &ConformanceDecl) {
        let type_symbol = Symbol::top_level(self.lib.as_ref(), &decl.target.token);
        
        let target_metadata = self.lib.type_metadata(&type_symbol);
        if target_metadata.is_none() {
            self.report_error(Diagnostic::error(&decl.target, "Type not found"));
            return;
        }
        decl.target.set_symbol(type_symbol.clone());

        let trait_metadata = self.lib.trait_metadata(decl.trait_name.lexeme());
        if trait_metadata.is_none() {
            self.report_error(Diagnostic::error(&decl.trait_name, "Trait not found"));
            return;
        }
        let trait_metadata = trait_metadata.unwrap();

        let type_metadata = self.lib.type_metadata(&type_symbol).unwrap();
        type_metadata.add_trait_impl(&trait_metadata.symbol);
        let type_metadata = type_metadata.clone();

        self.context.push_scope(
            type_metadata.symbol.clone(),
            ScopeType::InsideType(type_metadata.clone()),
        );

        for (symbol, field_type) in type_metadata
            .field_symbols
            .iter()
            .zip(&type_metadata.field_types)
        {
            self.context.put_in_scope(symbol, field_type);
        }

        for function in &decl.implementations {
            self.check_function_decl(function);
        }

        self.context.pop_scope();

        let trait_metadata = self
            .lib
            .trait_metadata(decl.trait_name.lexeme())
            .unwrap();
        for requirement in &trait_metadata.function_requirements {
            let requirement_metadata = self.lib.function_metadata(&requirement).unwrap();
            let impl_symbol = Symbol::new_str(&type_metadata.symbol, requirement.last_component());
            let impl_metadata = self.lib.function_metadata(&impl_symbol);
            if let Some(impl_metadata) = impl_metadata {
                if !impl_metadata
                    .node_type()
                    .matches(&requirement_metadata.node_type())
                {
                    let message = format!(
                        "Type implements requirement '{}' but with wrong signature",
                        requirement.last_component()
                    );
                    self.report_error(Diagnostic::error(&decl.target, &message));
                }
            } else {
                let message = format!(
                    "Type doesn't implement requirement '{}'",
                    requirement.last_component()
                );
                self.report_error(Diagnostic::error(&decl.target, &message));
            }
        }
    }
}

impl StmtVisitor for TypeChecker {
    type StmtResult = Analysis;

    fn visit_assignment_stmt(
        &mut self,
        target: &Expr,
        value: &Expr,
    ) -> Analysis {
        let target_type = self.check_expr(target);
        let value_type = self.check_expr(value);

        if let (Some(target_type), Some(value_type)) = (target_type, value_type) {
            if let Err(diag) = check::check_type_match(value, &value_type, &target_type) {
                self.report_error(diag);
            }
        }
        
        Analysis { guarantees_return: false }
    }

    fn visit_local_variable_decl(&mut self, decl: &LocalVariableDecl) -> Analysis {
        let explicit_type =
            decl.explicit_type
                .as_ref()
                .and_then(|k| match self.context.resolve_type(k) {
                    Ok(explicit_type) => Some(explicit_type),
                    Err(diagnostic) => {
                        self.report_error(diagnostic);
                        None
                    }
                });

        let implicit_type = decl.initial_value.as_ref().and_then(|v| self.check_expr(v));

        let var_type = match (explicit_type, implicit_type) {
            (Some(explicit), Some(implicit)) => {
                if let Err(diag) = check::check_type_match(
                    decl.initial_value.as_ref().unwrap(),
                    &implicit,
                    &explicit,
                ) {
                    self.report_error(diag);
                }
                explicit
            }
            (Some(explicit), None) => explicit,
            (None, Some(implicit)) if !implicit.contains_ambiguity() => implicit,
            _ => {
                self.report_error(Diagnostic::error(&decl.name, "Can't infer type"));
                NodeType::Ambiguous
            }
        };

        self.context.define_var(&decl.name, &var_type);
        decl.set_type(var_type);

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_if_stmt(&mut self, condition: &Expr, body: &[Stmt], else_body: &[Stmt]) -> Analysis {
        if let Some(cond_type) = self.check_expr(condition) {
            if let Err(diag) = check::check_type_match(condition, &cond_type, &NodeType::Bool) {
                self.report_error(diag);
            }
            let _ = condition.set_type(NodeType::Bool);
        }

        self.context.push_scope_named("if");
        let body_analysis = self.check_list(body);
        self.context.pop_scope();

        self.context.push_scope_named("else");
        let else_analysis = self.check_list(else_body);
        self.context.pop_scope();

        let guarantees_return = body_analysis.guarantees_return && else_analysis.guarantees_return;
        Analysis {  guarantees_return }
    }

    fn visit_while_stmt(&mut self, condition: &Expr, body: &[Stmt]) -> Analysis {
        if let Some(cond_type) = self.check_expr(condition) {
            if let Err(diag) = check::check_type_match(condition, &cond_type, &NodeType::Bool) {
                self.report_error(diag);
            }
            let _ = condition.set_type(NodeType::Bool);
        }

        self.context.push_scope_named("while");
        let body_analysis = self.check_list(body);
        self.context.pop_scope();

        body_analysis
    }

    fn visit_for_stmt(&mut self, variable: &SymbolicToken, array: &Expr, body: &[Stmt]) -> Analysis {
        let array_element_type = match self.check_expr(array) {
            Some(NodeType::Array(of, _)) => of,
            None => {
                return Analysis {
                    guarantees_return: false,
                }
            }
            _ => {
                self.report_error(Diagnostic::error(array, "Can only iterate over arrays"));
                return Analysis {
                    guarantees_return: false,
                };
            }
        };

        self.context.push_scope_named("for");
        self.context
            .define_var(variable, &NodeType::Pointer(array_element_type));
        let body_analysis = self.check_list(body);
        self.context.pop_scope();

        body_analysis
    }

    fn visit_return_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Analysis {
        let ret_type = expr
            .as_ref()
            .and_then(|e| self.check_expr(e))
            .unwrap_or(NodeType::Void);

        let expected_return = self
            .context
            .enclosing_function()
            .as_ref()
            .map(|m| &m.return_type);
        let expected_return = expected_return.unwrap_or(&NodeType::Void);

        if let Some(expr) = expr.as_ref() {
            let _ = expr.set_type(expected_return.clone());
            if let Err(diag) = check::check_type_match(expr, &ret_type, &expected_return) {
                self.report_error(diag);
            }
        } else {
            if !expected_return.matches(&NodeType::Void) {
                self.report_error(Diagnostic::error(stmt, "Function expects a return value"));
            }
        }

        Analysis {
            guarantees_return: true,
        }
    }

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Analysis {
        self.check_expr(expr);
        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_break_stmt(&mut self) -> Analysis {
        Analysis {
            guarantees_return: false,
        }
    }
}

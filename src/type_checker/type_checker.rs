use super::expr_checker::*;
use super::ContextTracker;
use super::check;
use crate::diagnostic::*;
use crate::guard;
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
            checker.visit_type_decl(decl);
        }
        for decl in &lib.function_decls {
            checker.visit_function_decl(decl);
        }
        for decl in &lib.builtins {
            decl.name.set_symbol(Symbol::new(None, &decl.name.token));
        }

        checker.context.push_scope_named("main");
        checker.check_list(&lib.other);
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

    fn report_error(&mut self, diag: Diagnostic) {
        self.reporter.report(diag);
    }
}

impl StmtVisitor for TypeChecker {
    type StmtResult = Analysis;

    fn visit_type_decl(&mut self, decl: &TypeDecl) -> Analysis {
        let (_, metadata) = self.context.push_type_scope(&decl.name);

        self.context.push_scope_meta(&metadata);
        for meta_method in &metadata.meta_methods {
            let method_metadata = self.lib.function_metadata(&meta_method).unwrap();
            self.context
                .put_in_scope(meta_method, &method_metadata.node_type());
        }
        for meta_method in &decl.meta_methods {
            self.visit_function_decl(meta_method);
        }
        self.context.pop_scope();

        for field in &decl.fields {
            self.visit_variable_decl(field);
        }

        self.context
            .put_in_scope(&Symbol::self_symbol(), &metadata.unspecialized_type());

        for method in &metadata.methods {
            let method_metadata = self.lib.function_metadata(&method).unwrap();
            self.context
                .put_in_scope(method, &method_metadata.node_type());
        }
        for method in &decl.methods {
            self.visit_function_decl(method);
        }

        self.context.pop_scope();

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_function_decl(&mut self, decl: &FunctionDecl) -> Analysis {
        let (func_symbol, metadata) = self.context.push_function_scope(&decl.name);
        decl.name.set_symbol(func_symbol.clone());

        let explicit_return_type = decl.return_type.as_ref();
        if let Some(e) = explicit_return_type {
            if let Err(diag) =
            check::confirm_fully_specialized(self.lib.as_ref(), e, &metadata.return_type)
            {
                self.report_error(diag);
            }
        }
        let return_type = match &metadata.return_type {
            NodeType::Array(..) => {
                self.report_error(Diagnostic::error(
                    explicit_return_type.unwrap(),
                    "Cannot return an array",
                ));
                &NodeType::Ambiguous
            }
            NodeType::Ambiguous => {
                self.report_error(Diagnostic::error(
                    explicit_return_type.unwrap(),
                    "Undefined type",
                ));
                &NodeType::Ambiguous
            }
            value => value,
        };

        for param in &decl.parameters {
            self.visit_variable_decl(param);
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

    fn visit_variable_decl(&mut self, decl: &VariableDecl) -> Analysis {
        let explicit_type =
            decl.explicit_type
                .as_ref()
                .and_then(|k| match self.context.resolve_explicit_type(k) {
                    Ok(explicit_type) => Some(explicit_type),
                    Err(diagnostic) => {
                        self.report_error(diagnostic);
                        None
                    }
                });

        let implicit_type = decl.initial_value.as_ref().and_then(|v| self.check_expr(v));

        match (explicit_type, implicit_type) {
            (Some(explicit), Some(implicit)) => {
                self.context.define_var(&decl.name, &explicit);

                if let Err(diag) = check::check_type_match(
                    decl.initial_value.as_ref().unwrap(),
                    &implicit,
                    &explicit,
                ) {
                    self.report_error(diag);
                }
                let _ = decl
                    .initial_value
                    .as_ref()
                    .unwrap()
                    .set_type(explicit.clone());
            }
            (Some(explicit), None) => self.context.define_var(&decl.name, &explicit),
            (None, Some(implicit)) if !implicit.contains_ambiguity() => {
                self.context.define_var(&decl.name, &implicit)
            }
            _ => {
                self.report_error(Diagnostic::error(&decl.name, "Can't infer type"));
            }
        }

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_trait_decl(&mut self, _name: &TypedToken, _requirements: &[Stmt]) -> Analysis {
        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_if_stmt(&mut self, condition: &Expr, body: &[Stmt], else_body: &[Stmt]) -> Analysis {
        if let Some(cond_type) = self.check_expr(condition) {
            if let Err(diag) = check::check_type_match(condition, &cond_type, &NodeType::Bool)
            {
                self.report_error(diag);
            }
            let _ = condition.set_type(NodeType::Bool);
        }

        let mut guarantees_return = true;

        self.context.push_scope_named("if");
        let body_analysis = self.check_list(body);
        if body_analysis.guarantees_return == false {
            guarantees_return = false;
        }
        self.context.pop_scope();

        self.context.push_scope_named("else");
        let else_analysis = self.check_list(else_body);
        if else_analysis.guarantees_return == false {
            guarantees_return = false;
        }
        self.context.pop_scope();

        Analysis { guarantees_return }
    }

    fn visit_while_stmt(&mut self, condition: &Expr, body: &[Stmt]) -> Analysis {
        if let Some(cond_type) = self.check_expr(condition) {
            if let Err(diag) = check::check_type_match(condition, &cond_type, &NodeType::Bool)
            {
                self.report_error(diag);
            }
            let _ = condition.set_type(NodeType::Bool);
        }

        self.context.push_scope_named("while");
        let body_analysis = self.check_list(body);
        self.context.pop_scope();

        body_analysis
    }

    fn visit_for_stmt(&mut self, variable: &TypedToken, array: &Expr, body: &[Stmt]) -> Analysis {
        let array_type = match self.check_expr(array) {
            Some(NodeType::Array(of, size)) => Some(NodeType::Array(of, size)),
            None => None,
            _ => {
                self.report_error(Diagnostic::error(array, "Can only iterate over arrays"));
                None
            }
        };
        if array_type.is_none() {
            return Analysis {
                guarantees_return: false,
            };
        }
        let array_type = array_type.unwrap();
        guard!(NodeType::Array[of, _size] = &array_type);

        self.context.push_scope_named("for");
        let of: &NodeType = &of;
        self.context
            .define_var(variable, &NodeType::pointer_to(of.clone()));
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

    fn visit_print_stmt(&mut self, expr: &Option<Expr>) -> Analysis {
        if let Some(expr) = expr.as_ref() {
            if let Some(node_type) = self.check_expr(expr) {
                match node_type {
                    NodeType::Int | NodeType::Double | NodeType::Bool => {
                        let _ = expr.set_type(node_type);
                    }
                    node_type if node_type.is_pointer_to(NodeType::Byte) => {
                        let _ = expr.set_type(node_type);
                    }
                    _ => {
                        let message = format!("Can't print object of type {}", node_type);
                        self.report_error(Diagnostic::error(expr, &message));
                    }
                }
            }
        }

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Analysis {
        self.check_expr(expr);
        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_builtin_stmt(&mut self, _inner: &Box<Stmt>) -> Self::StmtResult {
        unreachable!()
    }
}

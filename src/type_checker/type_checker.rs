use super::check;
use super::expr_checker::*;
use super::scope::{ContextTracker, ScopeDefinition, ScopeType, SymbolResolution, GenericInfo};
use crate::diagnostic::*;
use crate::library::*;
use crate::parsing::*;
use std::rc::Rc;
use std::collections::HashMap;

pub struct TypeChecker {
    reporter: Rc<dyn Reporter>,
    all_symbols: SymbolStore,
    lib_symbols: Rc<SymbolTable>,
    context: ContextTracker,
}

pub struct Analysis {
    guarantees_return: bool,
}

impl TypeChecker {
    pub fn check(module: &ParsedModule, all_symbols: SymbolStore, lib_symbols: Rc<SymbolTable>, reporter: Rc<dyn Reporter>) {
        let mut checker = TypeChecker {
            reporter,
            all_symbols,
            lib_symbols,
            context: ContextTracker::new(module.root_sym()),
        };

        for decl in &module.type_decls {
            checker.check_type_decl(decl);
        }
        for decl in &module.function_decls {
            checker.check_function_decl(decl);
        }
        for decl in &module.conformance_decls {
            checker.check_conformance_decl(decl);
        }

        checker
            .context
            .push_scope(Symbol::main_symbol(&module.name), ScopeType::InsideFunction);
        checker.check_list(&module.main);
        checker.context.pop_scope();
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
        let mut expr_checker = ExprChecker::new(self.lib_symbols.lib.clone(), self.symbol_resolver(), &self.all_symbols);
        let result = expr.accept(&mut expr_checker);
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

    fn check_type_decl(&mut self, decl: &TypeDecl) {
        let type_symbol = self.context.current_symbol().child_token(&decl.name.token);
        self.context
            .push_scope(type_symbol.clone(), ScopeType::InsideType);

        self.context.push_meta_scope();
        let metadata = self.all_symbols.type_metadata(&type_symbol).unwrap();
        for method in metadata.meta_method_symbols() {
            self.context
                .put_in_scope(method.name().to_owned(), ScopeDefinition::Function(method));
        }
        for meta_method in &decl.meta_methods {
            self.check_function_decl(meta_method);
        }
        self.context.pop_scope();

        let metadata = self.all_symbols.type_metadata(&type_symbol).unwrap();
        TypeChecker::put_type_in_scope(&mut self.context, &metadata);
      
        for method in &decl.methods {
            self.check_function_decl(method);
        }

        self.context.pop_scope();
    }

    fn check_function_decl(&mut self, decl: &FunctionDecl) {
        if decl.is_builtin {
            return;
        }

        let func_symbol = self.context.current_symbol().child_token(&decl.name.token);

        self.context
            .push_scope(func_symbol.clone(), ScopeType::InsideFunction);

        let metadata = self.all_symbols.function_metadata(&func_symbol).unwrap();

        let mut restrictions: HashMap<Symbol, Vec<GenericInfo>> = HashMap::new();
        for (gen, trait_name) in &metadata.generic_restrictions {
            restrictions.entry(gen.clone())
               .or_insert(Vec::new())
               .push(GenericInfo::Conforms(trait_name.clone()));
        }

        self.context.current_scope().generic_restrictions = restrictions;

        let return_type = match &metadata.return_type {
            NodeType::Array(..) => {
                self.report_error(Diagnostic::error(
                    decl.return_type.as_ref().unwrap(),
                    "Cannot return an array",
                ));
                NodeType::Ambiguous
            }
            NodeType::Ambiguous => {
                self.report_error(Diagnostic::error(
                    decl.return_type.as_ref().unwrap(),
                    "Undefined type",
                ));
                NodeType::Ambiguous
            }
            value => value.clone(),
        };

        for (index, param) in metadata.parameters.iter().enumerate() {
            let def =
                ScopeDefinition::Variable(func_symbol.child(&param.name), param.var_type.clone());
            self.context.put_in_scope(param.name.clone(), def);

            if let NodeType::Array(_, 0) = param.var_type {
                self.report_error(Diagnostic::error(
                    &decl.parameters[index],
                    "A zero length array cannot be a parameter",
                ));
            }
        }

        if decl.include_caller {
            let def = ScopeDefinition::Variable(
                func_symbol.caller_symbol(),
                NodeType::pointer_to(NodeType::Byte),
            );
            self.context.put_in_scope("caller".to_owned(), def);
        }

        let analysis = self.check_list(&decl.body);

        self.context.pop_scope();

        if !analysis.guarantees_return && !return_type.matches(&NodeType::Void) {
            let last_param = decl.parameters.last().map(|p| p.span().clone());
            let span_params = Span::join_opt(&decl.name, &last_param);
            let span = Span::join_opt(&span_params, &decl.return_type);
            self.report_error(Diagnostic::error(&span, "Function may not return"));
        }
    }

    fn check_conformance_decl(&mut self, decl: &ConformanceDecl) {
        let type_symbol = self.lib_symbols.lib.child_token(&decl.target.token);

        let target_metadata = self.all_symbols.type_metadata(&type_symbol);
        if target_metadata.is_none() {
            self.report_error(Diagnostic::error(&decl.target, "Type not found"));
            return;
        }
        decl.target.set_symbol(type_symbol.clone());

        let type_metadata = self.all_symbols.type_metadata(&type_symbol).unwrap();

        self.context
            .push_scope(type_metadata.symbol.clone(), ScopeType::InsideType);

        TypeChecker::put_type_in_scope(&mut self.context, &type_metadata);

        for function in &decl.implementations {
            self.check_function_decl(function);
        }

        self.context.pop_scope();

        let type_metadata = self.all_symbols.type_metadata(&type_symbol).unwrap();
        let trait_metadata = self.all_symbols.trait_metadata(decl.trait_name.lexeme()).unwrap();
        for requirement in &trait_metadata.function_requirements {
            let trait_symbol = trait_metadata.symbol.child(&requirement);
            let requirement_metadata = self.all_symbols.function_metadata(&trait_symbol).unwrap();
            let impl_symbol = type_metadata.symbol.child(&requirement);
            let impl_metadata = self.all_symbols.function_metadata(&impl_symbol);

            if let Some(impl_metadata) = impl_metadata {
                if !impl_metadata
                    .node_type()
                    .matches(&requirement_metadata.node_type())
                {
                    let message = format!(
                        "Type implements requirement '{}' but with wrong signature",
                        requirement
                    );
                    self.report_error(Diagnostic::error(&decl.target, &message));
                }
            } else {
                let message = format!("Type doesn't implement requirement '{}'", requirement);
                self.report_error(Diagnostic::error(&decl.target, &message));
            }
        }
    }

    fn put_type_in_scope(context: &mut ContextTracker, metadata: &TypeMetadata) {
        for field in &metadata.fields {
            let definition =
                ScopeDefinition::Variable(metadata.symbol.child(&field.name), field.var_type.clone());
            context.put_in_scope(field.name.clone(), definition);
        }

        let self_symbol = metadata.symbol.self_symbol();
        let def = ScopeDefinition::Variable(
            self_symbol.clone(),
            metadata.ref_to_unspecialized_type(),
        );
        context.put_in_scope(self_symbol.name().to_owned(), def);

        for method in metadata.method_symbols() {
            context
                .put_in_scope(method.name().to_owned(), ScopeDefinition::Function(method));
        }
    }


    fn symbol_resolver(&self) -> SymbolResolution {
        self.context.symbol_resolver(&self.lib_symbols, &self.all_symbols)
    }
}

impl StmtVisitor for TypeChecker {
    type StmtResult = Analysis;

    fn visit_assignment_stmt(&mut self, target: &Expr, value: &Expr) -> Analysis {
        let target_type = self.check_expr(target);
        let value_type = self.check_expr(value);

        if let (Some(target_type), Some(value_type)) = (target_type, value_type) {
            if let Err(diag) = check::check_type_match(value, &value_type, &target_type) {
                self.report_error(diag);
            }
        }

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_local_variable_decl(&mut self, decl: &LocalVariableDecl) -> Analysis {
        let explicit_type =
            decl.explicit_type
                .as_ref()
                .and_then(|k| match self.symbol_resolver().resolve_explicit_type_or_err(k) {
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

        self.context.define_variable(&decl.name, &var_type);
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

        self.context.push_subscope();
        let body_analysis = self.check_list(body);
        self.context.pop_scope();

        self.context.push_subscope();
        let else_analysis = self.check_list(else_body);
        self.context.pop_scope();

        let guarantees_return = body_analysis.guarantees_return && else_analysis.guarantees_return;
        Analysis { guarantees_return }
    }

    fn visit_conditional_compilation_stmt(
        &mut self,
        condition: &CompilerCondition,
        body: &[Stmt],
    ) -> Analysis {
        self.context.push_subscope();

        match condition {
            CompilerCondition::Conformance(gen_name, trait_name) => {
                let gen = self.symbol_resolver().resolve_generic(gen_name.token.lexeme());
                if gen.is_none() {
                    self.report_error(Diagnostic::error(gen_name, "Unrecognized generic parameter"));
                    return Analysis {
                        guarantees_return: false,
                    };
                }
        
                gen_name.set_symbol(gen.as_ref().unwrap().clone());
        
                let trait_metadata = self.all_symbols.trait_metadata(trait_name.token.lexeme());
                if trait_metadata.is_none() {
                    self.report_error(Diagnostic::error(trait_name, "Unrecognized trait"));
                    return Analysis {
                        guarantees_return: false,
                    };
                }
        
                trait_name.set_symbol(trait_metadata.unwrap().symbol.clone());

                let info = GenericInfo::Conforms(trait_metadata.unwrap().symbol.clone());
                self
                    .context
                    .current_scope()
                    .generic_restrictions
                    .insert(gen.unwrap(), vec![info]);
            }
            CompilerCondition::Equality(gen_name, type_name, node_type) => {
                let gen = self.symbol_resolver().resolve_generic(gen_name.token.lexeme());
                if gen.is_none() {
                    self.report_error(Diagnostic::error(gen_name, "Unrecognized generic parameter"));
                    return Analysis {
                        guarantees_return: false,
                    };
                }
        
                gen_name.set_symbol(gen.as_ref().unwrap().clone());

                match self.symbol_resolver().resolve_explicit_type_or_err(type_name) {
                    Ok(ty) => {
                        node_type.replace(Some(ty.clone()));
                        let info = GenericInfo::Is(ty);
                        self
                            .context
                            .current_scope()
                            .generic_restrictions
                            .insert(gen.unwrap(), vec![info]);
                    },
                    Err(diag) => {
                        self.report_error(diag);
                        return Analysis {
                            guarantees_return: false,
                        };
                    },
                }
            }
        };

        self.check_list(body);
        self.context.pop_scope();
        
        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_while_stmt(&mut self, condition: &Expr, body: &[Stmt]) -> Analysis {
        if let Some(cond_type) = self.check_expr(condition) {
            if let Err(diag) = check::check_type_match(condition, &cond_type, &NodeType::Bool) {
                self.report_error(diag);
            }
            let _ = condition.set_type(NodeType::Bool);
        }

        self.context.push_subscope();
        let body_analysis = self.check_list(body);
        self.context.pop_scope();

        body_analysis
    }

    fn visit_for_stmt(
        &mut self,
        variable: &SymbolicToken,
        array: &Expr,
        body: &[Stmt],
    ) -> Analysis {
        let iterable_type = if let Some(t) = self.check_expr(array) {
            t
        } else {
            return Analysis {
                guarantees_return: false,
            };
        };

        let array_element_type = match iterable_type {
            NodeType::Array(of, _) => Some(NodeType::Pointer(of)),
            NodeType::Reference(to) => {
                if let NodeType::Instance(instance_symbol, spec) = to.as_ref() {
                    let metadata = self.all_symbols.type_metadata(&instance_symbol).unwrap();
                    if metadata.conforms_to(&Symbol::iterable_symbol()) {
                        if instance_symbol == &Symbol::stdlib("Vec") {
                            let object_type = spec
                                .type_for(&instance_symbol.child("T"))
                                .unwrap()
                                .specialize(&spec);
                            Some(NodeType::reference_to(object_type))
                        } else if instance_symbol == &Symbol::stdlib("Range") {
                            Some(NodeType::Int)
                        } else {
                            unimplemented!()
                        }
                    } else {
                        let message = format!(
                            "Type '{}' does not implement Iterable",
                            instance_symbol.mangled()
                        );
                        self.report_error(Diagnostic::error(array, &message));
                        return Analysis {
                            guarantees_return: false,
                        };
                    }
                } else {
                    None
                }
            }
            _ => None
        };

        if let Some(array_element_type) = array_element_type {
            self.context.push_subscope();
            self.context.define_variable(variable, &array_element_type);
            let body_analysis = self.check_list(body);
            self.context.pop_scope();

            body_analysis
        } else {
            self.report_error(Diagnostic::error(array, "Cannot iterate"));
            Analysis {
                guarantees_return: false,
            }
        }
    }

    fn visit_return_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Analysis {
        let ret_type = expr
            .as_ref()
            .and_then(|e| self.check_expr(e))
            .unwrap_or(NodeType::Void);

        let current_scope = &self.context.current_scope_immut().scope_type;
        let expected_return = if let ScopeType::InsideFunction = current_scope {
            self
                .all_symbols
                .function_metadata(&self.context.current_symbol())
                .unwrap()
                .return_type
                .clone()
        } else {
            // TopLevel
            NodeType::Void
        };

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

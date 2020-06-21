use super::metadata::*;
use super::node_type::*;
use super::symbol_table::*;
use crate::diagnostic::*;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::*;
use log::trace;
use std::collections::HashMap;
use std::rc::Rc;

type Result = DiagnosticResult<NodeType>;

struct Scope {
    id: Option<Symbol>,
    symbols: SymbolTable,
    function_metadata: Option<FunctionMetadata>,
}

impl Scope {
    fn new(id: Option<Symbol>, function_metadata: Option<FunctionMetadata>) -> Self {
        Scope {
            id,
            symbols: SymbolTable::new(),
            function_metadata,
        }
    }

    fn define_var(&mut self, name: &TypedToken, var_type: &NodeType) {
        if var_type.contains_ambiguity() {
            panic!("Should never define var as ambiguous");
        }

        let new_symbol = Symbol::new((&self.id).as_ref(), &name.token);

        trace!(target: "type_checker", "Defining {} (symbol = {}) as {}", name.span().lexeme(), new_symbol, var_type);

        self.symbols
            .insert(new_symbol.clone(), var_type.clone(), name.span().clone());

        name.set_symbol(new_symbol);
        name.set_type(var_type.clone());
    }

    fn put_in_scope(&mut self, symbol: &Symbol, var_type: &NodeType) {
        self.symbols
            .insert(symbol.clone(), var_type.clone(), Span::empty());
    }
}

pub struct TypeChecker {
    reporter: Rc<dyn Reporter>,
    lib: Rc<Lib>,
    call_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>,
    scopes: Vec<Scope>,
    is_builtin: bool,
}

pub struct Analysis {
    guarantees_return: bool,
}

impl TypeChecker {
    pub fn check(lib: Lib, reporter: Rc<dyn Reporter>) -> Lib {
        let top_level = Scope::new(None, None);
        let lib = Rc::new(lib);
        let mut checker = TypeChecker {
            reporter,
            lib: Rc::clone(&lib),
            call_map: HashMap::new(),
            scopes: vec![top_level],
            is_builtin: false,
        };

        for decl in &lib.type_decls {
            checker.visit_type_decl(decl);
        }
        for decl in &lib.function_decls {
            checker.visit_function_decl(decl);
        }

        checker.is_builtin = true;
        for builtin in &lib.builtins {
            checker.visit_function_decl(builtin);
        }
        checker.is_builtin = false;

        checker.push_scope_named("main");
        checker.check_list(&lib.other);
        checker.pop_scope();

        let call_map = std::mem::replace(&mut checker.call_map, HashMap::new());

        std::mem::drop(checker);

        let mut lib = Rc::try_unwrap(lib).ok().unwrap();
        // for (symbol, specs) in spec_map {
        //     let metadata = lib.function_metadata_mut(&symbol).unwrap();
        //     metadata.specializations = specs;
        // }

        lib.symbols.call_map = call_map;

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
        match expr.accept(self) {
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

    fn type_mismatch<T: ContainsSpan>(
        &self,
        span: &T,
        given: &NodeType,
        expected: &NodeType,
    ) -> Diagnostic {
        let message = format!("Expected {}, got {}", expected, given);
        Diagnostic::error(span, &message)
    }

    fn ensure_no_amibguity(&self, expr: &Expr, node_type: &NodeType) -> DiagnosticResult<()> {
        if node_type.contains_ambiguity() {
            Err(Diagnostic::error(expr, "Cannot infer type"))
        } else {
            Ok(())
        }
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn current_scope_immut(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn current_symbol(&self) -> Option<&Symbol> {
        self.scopes.last().unwrap().id.as_ref()
    }

    fn push_scope_named(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new_str(self.current_symbol(), name);
        let function_metadata = self.current_scope().function_metadata.clone();
        self.push_scope(symbol.clone(), function_metadata);
        symbol
    }

    fn push_scope_meta(&mut self) {
        let symbol = Symbol::meta_symbol(self.current_symbol());
        self.push_scope(symbol, None);
    }

    fn push_type_scope(&mut self, name: &TypedToken) -> (Symbol, TypeMetadata) {
        let symbol = Symbol::new(self.current_symbol(), &name.token);
        let metadata = self.lib.type_metadata(&symbol).unwrap();
        self.push_scope(symbol.clone(), None);
        (symbol, metadata)
    }

    fn push_function_scope(&mut self, name: &TypedToken) -> (Symbol, FunctionMetadata) {
        let symbol = Symbol::new(self.current_symbol(), &name.token);
        let metadata = self.lib.function_metadata(&symbol).unwrap();
        self.push_scope(symbol.clone(), Some(metadata.clone()));
        (symbol, metadata)
    }

    fn push_scope(&mut self, id: Symbol, metadata: Option<FunctionMetadata>) {
        trace!(target: "type_checker", "Pushing scope -- {}", id);
        self.scopes.push(Scope::new(Some(id), metadata));
    }

    fn pop_scope(&mut self) {
        let popped = self.scopes.pop().unwrap();
        trace!(target: "type_checker", "Popped scope {}", popped.id.unwrap());
    }

    fn resolve_var(&self, name: &str) -> Option<(Symbol, NodeType)> {
        for scope in self.scopes.iter().rev() {
            let possible_symbol = Symbol::new_str((&scope.id).as_ref(), name);
            if let Some(node_type) = scope.symbols.get_type(&possible_symbol) {
                return Some((possible_symbol, node_type.clone()));
            }
        }

        let lib_symbol = Symbol::new_str(None, name);
        if let Some(node_type) = self.lib.resolve_symbol(&lib_symbol) {
            Some((lib_symbol, node_type))
        } else {
            None
        }
    }

    fn check_type_match(
        &self,
        expr: &Expr,
        given: &NodeType,
        expected: &NodeType,
    ) -> DiagnosticResult<()> {
        if given.matches(expected) {
            Ok(())
        } else {
            Err(self.type_mismatch(expr, given, expected))
        }
    }

    fn symbolic_context(&self) -> Vec<Symbol> {
        self.scopes
            .iter()
            .flat_map(|s| s.id.as_ref().map(|id| id.clone()))
            .collect()
    }

    fn resolve_explicit_type(&mut self, explicit_type: &ExplicitType) -> Result {
        let context = self.symbolic_context();

        if let Some(deduced) = explicit_type.resolve(&self.lib, &context) {
            if let (&NodeType::Any, false) = (&deduced, self.is_builtin) {
                Err(Diagnostic::error(
                    explicit_type,
                    "Cannot have type any; must be ptr any",
                ))
            } else {
                self.confirm_fully_specialized(explicit_type, &deduced)?;
                Ok(deduced)
            }
        } else {
            Err(Diagnostic::error(explicit_type, "Undefined type"))
        }
    }

    fn confirm_fully_specialized<S: ContainsSpan>(&self, span: &S, node_type: &NodeType) -> DiagnosticResult<()> {
        match &node_type {
            NodeType::Instance(type_symbol, specialization) | NodeType::Metatype(type_symbol, specialization) => {
                let matching_type = self.lib.type_metadata(type_symbol).unwrap();
                let spec_length = specialization.node_types.len();
                if spec_length != matching_type.generics.len() {
                    let message = format!(
                        "Expected {} specializations, got {} (explicit type check)",
                        matching_type.generics.len(),
                        spec_length
                    );
                    Err(Diagnostic::error(span, &message))
                } else {
                    Ok(())
                }
            }
            _ => Ok(())
        }
    }
}

impl StmtVisitor for TypeChecker {
    type StmtResult = Analysis;

    fn visit_type_decl(&mut self, decl: &TypeDecl) -> Analysis {
        let (_, metadata) = self.push_type_scope(&decl.name);

        self.push_scope_meta();
        for meta_method in &metadata.meta_methods {
            let method_type = self.lib.resolve_symbol(&meta_method).unwrap();
            self.current_scope().put_in_scope(meta_method, &method_type);
        }
        for meta_method in &decl.meta_methods {
            self.visit_function_decl(meta_method);
        }
        self.pop_scope();

        for field in &decl.fields {
            self.visit_variable_decl(field);
        }

        for method in &metadata.methods {
            let method_type = self.lib.resolve_symbol(&method).unwrap();
            self.current_scope().put_in_scope(method, &method_type);
        }
        for method in &decl.methods {
            self.visit_function_decl(method);
        }

        self.pop_scope();

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_function_decl(&mut self, decl: &FunctionDecl) -> Analysis {
        let (func_symbol, metadata) = self.push_function_scope(&decl.name);
        decl.name.set_symbol(func_symbol.clone());

        let explicit_type_span = decl.return_type.as_ref();
        let return_type = match &metadata.return_type {
            NodeType::Array(..) => {
                self.report_error(Diagnostic::error(
                    explicit_type_span.unwrap(),
                    "Cannot return an array",
                ));
                &NodeType::Ambiguous
            }
            NodeType::Ambiguous => {
                self.report_error(Diagnostic::error(
                    explicit_type_span.unwrap(),
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

        self.pop_scope();

        if !analysis.guarantees_return && !return_type.matches(&NodeType::Void) && !self.is_builtin
        {
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
                .and_then(|k| match self.resolve_explicit_type(k) {
                    Ok(explicit_type) => Some(explicit_type),
                    Err(diagnostic) => {
                        self.report_error(diagnostic);
                        None
                    }
                });

        let implicit_type = decl.initial_value.as_ref().and_then(|v| self.check_expr(v));

        match (explicit_type, implicit_type) {
            (Some(explicit), Some(implicit)) => {
                self.current_scope().define_var(&decl.name, &explicit);

                if let Err(diag) = self.check_type_match(
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
            (Some(explicit), None) => self.current_scope().define_var(&decl.name, &explicit),
            (None, Some(implicit)) if !implicit.contains_ambiguity() => {
                self.current_scope().define_var(&decl.name, &implicit)
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
            if let Err(diag) = self.check_type_match(condition, &cond_type, &NodeType::Bool) {
                self.report_error(diag);
            }
            let _ = condition.set_type(NodeType::Bool);
        }

        let mut guarantees_return = true;

        self.push_scope_named("if");
        let body_analysis = self.check_list(body);
        if body_analysis.guarantees_return == false {
            guarantees_return = false;
        }
        self.pop_scope();

        self.push_scope_named("else");
        let else_analysis = self.check_list(else_body);
        if else_analysis.guarantees_return == false {
            guarantees_return = false;
        }
        self.pop_scope();

        Analysis { guarantees_return }
    }

    fn visit_while_stmt(&mut self, condition: &Expr, body: &[Stmt]) -> Analysis {
        if let Some(cond_type) = self.check_expr(condition) {
            if let Err(diag) = self.check_type_match(condition, &cond_type, &NodeType::Bool) {
                self.report_error(diag);
            }
            let _ = condition.set_type(NodeType::Bool);
        }

        self.push_scope_named("while");
        let body_analysis = self.check_list(body);
        self.pop_scope();

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

        self.push_scope_named("for");
        let of: &NodeType = &of;
        self.current_scope()
            .define_var(variable, &NodeType::pointer_to(of.clone()));
        let body_analysis = self.check_list(body);
        self.pop_scope();

        body_analysis
    }

    fn visit_return_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Analysis {
        let ret_type = expr
            .as_ref()
            .and_then(|e| self.check_expr(e))
            .unwrap_or(NodeType::Void);

        let expected_return = self
            .current_scope_immut()
            .function_metadata
            .as_ref()
            .map(|m| &m.return_type);
        let expected_return = expected_return.unwrap_or(&NodeType::Void);

        if let Some(expr) = expr.as_ref() {
            let _ = expr.set_type(expected_return.clone());
            if let Err(diag) = self.check_type_match(expr, &ret_type, &expected_return) {
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
                    NodeType::Int | NodeType::Bool => {
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

    fn visit_builtin_stmt(&mut self, inner: &Box<Stmt>) -> Self::StmtResult {
        self.is_builtin = true;
        let result = inner.accept(self);
        self.is_builtin = false;
        result
    }
}

impl ExprVisitor for TypeChecker {
    type ExprResult = Result;

    fn visit_assignment_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        value: &Expr,
    ) -> Self::ExprResult {
        let target_type = target.accept(self)?;
        let value_type = value.accept(self)?;
        self.check_type_match(value, &value_type, &target_type)?;
        let _ = value.set_type(target_type);
        expr.set_type(NodeType::Void)
    }

    fn visit_binary_expr(
        &mut self,
        expr: &Expr,
        lhs: &Expr,
        op: &Token,
        rhs: &Expr,
    ) -> Self::ExprResult {
        let lhs_type = lhs.accept(self)?;
        let rhs_type = rhs.accept(self)?;

        self.ensure_no_amibguity(lhs, &lhs_type)?;
        self.ensure_no_amibguity(rhs, &rhs_type)?;

        let entries: &[BinaryEntry] = match op.kind {
            TokenKind::Plus => &ADDITION_ENTRIES,
            TokenKind::Minus | TokenKind::Star | TokenKind::Slash => &MATH_ENTRIES,
            TokenKind::AmpersandAmpersand | TokenKind::BarBar => &LOGIC_ENTRIES,
            TokenKind::EqualEqual | TokenKind::BangEqual => &EQUALITY_ENTRIES,
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual => &COMPARISON_ENTRIES,
            _ => panic!(),
        };

        if let Some(matching_entry) = entries
            .iter()
            .find(|e| e.0.matches(&lhs_type) && e.1.matches(&rhs_type))
        {
            expr.set_type(matching_entry.2.clone())
        } else {
            let message = format!("Cannot {} on {} and {}", op.lexeme(), lhs_type, rhs_type);
            Err(Diagnostic::error(&Span::join(lhs, rhs), &message))
        }
    }

    fn visit_unary_expr(&mut self, expr: &Expr, op: &Token, operand: &Expr) -> Self::ExprResult {
        let operand_type = operand.accept(self)?;

        self.ensure_no_amibguity(expr, &operand_type)?;

        if let TokenKind::Ampersand = op.kind {
            let boxed_type = Box::new(operand_type.clone());
            return expr.set_type(NodeType::Pointer(boxed_type));
        }

        if let TokenKind::Star = op.kind {
            return match operand_type {
                NodeType::Pointer(inner) => expr.set_type((*inner).clone()),
                _ => {
                    let message = format!("Cannot dereference object of type {}", operand_type);
                    Err(Diagnostic::error(operand, &message))
                }
            };
        }

        let entries = match op.kind {
            TokenKind::Minus => &NEGATE_ENTRIES,
            TokenKind::Bang => &INVERT_ENTRIES,
            _ => unreachable!(),
        };

        if entries.iter().find(|e| e.matches(&operand_type)).is_some() {
            expr.set_type(operand_type.clone())
        } else {
            let message = format!("Cannot {} on {}", op.lexeme(), operand_type);
            Err(Diagnostic::error(operand, &message))
        }
    }

    fn visit_function_call_expr(
        &mut self,
        expr: &Expr,
        target: Option<&Expr>,
        function: &ResolvedToken,
        args: &[Expr],
    ) -> Self::ExprResult {
        let function_name = function.token.lexeme();

        let (func_symbol, object_specialization, function_specialization): (_, _, &[ExplicitType]) = if let Some(target) = target {
            let target_type = target.accept(self)?;
            match &target_type {
                NodeType::Instance(type_symbol, specialization) => {
                    let type_metadata = self.lib.type_metadata(&type_symbol).unwrap();
                    if let Some(method_symbol) = type_metadata.method_named(function_name) {
                        (method_symbol, Some(specialization.clone()), &function.specialization)
                    } else {
                        return Err(Diagnostic::error(
                            &Span::join(target, function),
                            &format!(
                                "Type '{}' does not have method '{}'",
                                target_type,
                                function_name
                            ),
                        ));
                    }
                },
                NodeType::Metatype(type_symbol, specialization) => {
                    let type_metadata = self.lib.type_metadata(&type_symbol).unwrap();
                    if let Some(meta_method_symbol) = type_metadata.meta_method_named(function_name) {
                        (meta_method_symbol, Some(specialization.clone()), &function.specialization)
                    } else {
                        return Err(Diagnostic::error(
                            &Span::join(target, function),
                            &format!(
                                "Type '{}' does not have meta method '{}'",
                                target_type,
                                function_name
                            ),
                        ));
                    }
                },
                _ => return Err(Diagnostic::error(
                    expr,
                    &format!("Cannot call method on a {}", target_type),
                )),
            }
        } else {
            if let Some((found_symbol, node_type)) = self.resolve_var(function_name) {
                match node_type {
                    NodeType::Function(..) => (found_symbol, None, &function.specialization),
                    _ => return Err(Diagnostic::error(
                        function,
                        &format!("Cannot call type {}", node_type),
                    ))
                }
            } else {
                let explicit_type = NodeType::deduce_from_simple_explicit(
                    function, 
                    &self.lib.symbols, 
                    &self.lib.dependencies, 
                    &self.symbolic_context()
                );
                match &explicit_type {
                    Some(NodeType::Instance(type_symbol, specs)) => {
                        self.confirm_fully_specialized(function, explicit_type.as_ref().unwrap())?;
                        let meta_symbol = Symbol::meta_symbol(Some(&type_symbol));
                        (Symbol::init_symbol(Some(&meta_symbol)), Some(specs.clone()), &[])
                    },
                    _ => {
                        return Err(Diagnostic::error(function, "Undefined function"));
                    }
                }
            }
        };

        function.set_symbol(func_symbol.clone());

        let metadata = self.lib.function_metadata(&func_symbol).unwrap();

        trace!(target: "type_checker", "Callee metadata: {}", metadata);

        let mut function_type = metadata.full_type();
        if let Some(spec) = object_specialization.as_ref() {
            function_type = function_type.specialize(self.lib.as_ref(), spec);
        }

        let arg_types: DiagnosticResult<Vec<NodeType>> =
            args.iter().map(|a| a.accept(self)).collect();
        let arg_types = arg_types?;

        if metadata.parameter_types.len() != arg_types.len() {
            return Err(Diagnostic::error(
                expr,
                &format!(
                    "Expected {} argument(s), got {}",
                    metadata.parameter_types.len(),
                    arg_types.len()
                ),
            ));
        }

        let specialization = if function_specialization.is_empty() {
            match GenericSpecialization::infer(self.lib.as_ref(), &metadata, &arg_types) {
                Ok(spec) => spec,
                Err(index) => {
                    let message = format!(
                        "Couldn't infer generic type {}",
                        metadata.generics[index].last_component()
                    );
                    return Err(Diagnostic::error(expr, &message));
                }
            }
        } else {
            if function_specialization.len() != metadata.generics.len() {
                let message = format!(
                    "Expected {} specializations, got {} (func check)",
                    metadata.generics.len(),
                    function_specialization.len()
                );
                return Err(Diagnostic::error(function, &message));
            }

            let specialization: std::result::Result<Vec<NodeType>, _> = function_specialization
                .iter()
                .map(|s| self.resolve_explicit_type(s))
                .collect();

            GenericSpecialization::new(&func_symbol, specialization?)
        };

        {
            let enclosing_func = self
                .current_scope()
                .function_metadata
                .as_ref()
                .map(|f| f.symbol.clone())
                .unwrap_or(Symbol::main_symbol());
            self.call_map
                .entry(enclosing_func)
                .or_insert(Vec::new())
                .push((func_symbol.clone(), specialization.clone()));
        }

        function_type = function_type.specialize(self.lib.as_ref(), &specialization);

        for ((index, param), arg) in function_type.parameters.into_iter().enumerate().zip(arg_types) {
            if let Err(diag) = self.ensure_no_amibguity(&args[index], &arg) {
                self.report_error(diag);
            }
            self.check_type_match(&args[index], &arg, &param)?;
        }

        expr.set_type(function_type.return_type)
    }

    fn visit_field_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        field: &ResolvedToken,
    ) -> Self::ExprResult {
        let target_type = target.accept(self)?;
        let field_name = field.token.lexeme();

        match target_type {
            NodeType::Instance(type_symbol, specialization) => {
                let type_metadata = self.lib.type_metadata(&type_symbol).unwrap();
                if let Some((field_symbol, field_type)) = type_metadata.field_named(field_name) {
                    field.set_symbol(field_symbol);
                    expr.set_type(field_type.specialize(self.lib.as_ref(), &specialization))
                } else {
                    Err(Diagnostic::error(
                        &Span::join(target, field),
                        &format!(
                            "Type '{}' does not has field '{}'",
                            type_symbol.id,
                            field.span().lexeme()
                        ),
                    ))
                }
            },
            _ => Err(Diagnostic::error(
                target,
                &format!(
                    "Cannot access property of '{}'",
                    target_type
                )
            ))
        }
    }

    fn visit_literal_expr(&mut self, expr: &Expr, token: &Token) -> Self::ExprResult {
        let node_type = match token.kind {
            TokenKind::Number => NodeType::Int,
            TokenKind::True => NodeType::Bool,
            TokenKind::False => NodeType::Bool,
            TokenKind::StringLiteral => NodeType::pointer_to(NodeType::Byte),
            _ => panic!(),
        };
        expr.set_type(node_type)
    }

    fn visit_variable_expr(&mut self, expr: &Expr, name: &ResolvedToken) -> Self::ExprResult {
        if let Some((found_symbol, node_type)) = self.resolve_var(name.span().lexeme()) {
            if !name.specialization.is_empty() {
                return Err(Diagnostic::error(expr, "Cannot specialize variable"));
            }
            name.set_symbol(found_symbol);
            expr.set_type(node_type.clone())
        } else {
            let explicit_type = NodeType::deduce_from_simple_explicit(
                name, 
                &self.lib.symbols, 
                &self.lib.dependencies, 
                &self.symbolic_context()
            );
            match &explicit_type {
                Some(NodeType::Instance(symbol, spec)) => {
                    self.confirm_fully_specialized(name, explicit_type.as_ref().unwrap())?;
                    trace!(target: "type_checker", "Treating variable as metatype of {}", symbol);
                    name.set_symbol(symbol.clone());
                    expr.set_type(NodeType::Metatype(symbol.clone(), spec.clone()))
                },
                _ => {
                    Err(Diagnostic::error(name, "Undefined variable"))
                }
            }
        }
    }

    fn visit_array_expr(&mut self, expr: &Expr, elements: &[Expr]) -> Self::ExprResult {
        if elements.is_empty() {
            let node_type = NodeType::Array(Box::new(NodeType::Ambiguous), 0);
            return expr.set_type(node_type);
        }

        let mut element_types: Vec<NodeType> = Vec::new();
        for element in elements {
            element_types.push(element.accept(self)?);
        }

        let expected_type =
            if let Some(concrete) = element_types.iter().find(|e| !e.contains_ambiguity()) {
                concrete
            } else {
                element_types.first().unwrap()
            };

        for (index, element_type) in element_types.iter().enumerate() {
            if !element_type.matches(&expected_type) {
                return Err(self.type_mismatch(&elements[index], &element_type, &expected_type));
            }
        }

        let node_type = NodeType::Array(Box::new(expected_type.clone()), elements.len());
        expr.set_type(node_type)
    }

    fn visit_subscript_expr(&mut self, expr: &Expr, target: &Expr, arg: &Expr) -> Self::ExprResult {
        let target_type = target.accept(self)?;
        let arg_type = arg.accept(self)?;

        match (target_type, arg_type) {
            (NodeType::Array(inside, _), NodeType::Int) => expr.set_type((*inside).clone()),
            (NodeType::Array(..), other) => Err(self.type_mismatch(arg, &other, &NodeType::Int)),
            _ => Err(Diagnostic::error(expr, "Can't subscript into non-array")),
        }
    }

    fn visit_cast_expr(
        &mut self,
        expr: &Expr,
        explicit_type: &ExplicitType,
        value: &Expr,
    ) -> Self::ExprResult {
        value.accept(self)?;
        expr.set_type(self.resolve_explicit_type(explicit_type)?)
    }
}

const NEGATE_ENTRIES: [NodeType; 1] = [NodeType::Int];

const INVERT_ENTRIES: [NodeType; 1] = [NodeType::Bool];

type BinaryEntry = (NodeType, NodeType, NodeType);

const ADDITION_ENTRIES: [BinaryEntry; 1] = [(NodeType::Int, NodeType::Int, NodeType::Int)];

const MATH_ENTRIES: [BinaryEntry; 1] = [(NodeType::Int, NodeType::Int, NodeType::Int)];

const LOGIC_ENTRIES: [BinaryEntry; 1] = [(NodeType::Bool, NodeType::Bool, NodeType::Bool)];

const EQUALITY_ENTRIES: [BinaryEntry; 2] = [
    (NodeType::Int, NodeType::Int, NodeType::Bool),
    (NodeType::Bool, NodeType::Bool, NodeType::Bool),
];

const COMPARISON_ENTRIES: [BinaryEntry; 1] = [(NodeType::Int, NodeType::Int, NodeType::Bool)];

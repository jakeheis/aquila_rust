use super::node_type::*;
use super::symbol_table::*;
use crate::diagnostic::*;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::*;
use log::trace;
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

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
        if let NodeType::Ambiguous = var_type {
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
    specialization_map: HashMap<Symbol, Vec<GenericSpecialization>>,
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
            specialization_map: HashMap::new(), 
            scopes: vec![top_level],
            is_builtin: false,
        };

        checker.check_list(&lib.type_decls);
        checker.check_list(&lib.function_decls);

        checker.push_scope_named("main");
        checker.check_list(&lib.other);
        checker.pop_scope();

        let spec_map = std::mem::replace(&mut checker.specialization_map, HashMap::new());

        std::mem::drop(checker);

        let mut lib = Rc::try_unwrap(lib).ok().unwrap();
        for (symbol, specs) in spec_map {
            let metadata = lib.function_metadata_mut(&symbol).unwrap();
            metadata.specializations = specs;
        }
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

    fn check_function_arguments(
        &mut self,
        expr: &Expr,
        _function_symbol: &Symbol,
        specialization: &Option<GenericSpecialization>,
        param_types: &[NodeType],
        args: &[Expr],
    ) -> DiagnosticResult<()> {
        let arg_types: DiagnosticResult<Vec<NodeType>> =
            args.iter().map(|a| a.accept(self)).collect();
        let arg_types = arg_types?;

        if param_types.len() != arg_types.len() {
            return Err(Diagnostic::error(
                expr,
                &format!(
                    "Expected {} argument(s), got {}",
                    param_types.len(),
                    arg_types.len()
                ),
            ));
        }

        for ((index, param), arg) in param_types.iter().enumerate().zip(arg_types) {
            let param_spec = param.specialize(specialization.as_ref());
            // TODO: this won't work when passing zero length arrays as arguments probably
            self.check_type_match(&args[index], &arg, &param_spec)?;
        }

        Ok(())
    }

    fn resolve_explicit_type(&mut self, explicit_type: &ExplicitType) -> Result {
        let context: Vec<Symbol> = self
            .scopes
            .iter()
            .flat_map(|s| s.id.as_ref().map(|id| id.clone()))
            .collect();

        if let Some(deduced) = explicit_type.resolve(&self.lib, &context) {
            if let (&NodeType::Any, false) = (&deduced, self.is_builtin) {
                Err(Diagnostic::error(
                    explicit_type,
                    "Cannot have type any; must be ptr any",
                ))
            } else {
                Ok(deduced)
            }
        } else {
            Err(Diagnostic::error(explicit_type, "Undefined type"))
        }
    }
}

impl StmtVisitor for TypeChecker {
    type StmtResult = Analysis;

    fn visit_type_decl(
        &mut self,
        _stmt: &Stmt,
        name: &TypedToken,
        fields: &[Stmt],
        methods: &[Stmt],
        meta_methods: &[Stmt],
    ) -> Analysis {
        let (_, metadata) = self.push_type_scope(name);

        self.push_scope_meta();
        for meta_method in &metadata.meta_methods {
            let method_type = self.lib.resolve_symbol(&meta_method).unwrap();
            self.current_scope().put_in_scope(meta_method, &method_type);
        }
        self.check_list(meta_methods);
        self.pop_scope();

        self.check_list(fields);

        for method in &metadata.methods {
            let method_type = self.lib.resolve_symbol(&method).unwrap();
            self.current_scope().put_in_scope(method, &method_type);
        }
        self.check_list(methods);

        self.pop_scope();

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_function_decl(
        &mut self,
        _stmt: &Stmt,
        name: &TypedToken,
        generics: &[TypedToken],
        params: &[Stmt],
        explicit_return_type: &Option<ExplicitType>,
        body: &[Stmt],
        _is_meta: bool,
    ) -> Analysis {
        let (func_symbol, metadata) = self.push_function_scope(name);
        name.set_symbol(func_symbol.clone());

        for (index, token) in generics.iter().enumerate() {
            let generic_type = NodeType::Generic(func_symbol.clone(), index);
            self.current_scope().define_var(token, &generic_type);
        }

        let explicit_type_span = explicit_return_type.as_ref();
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

        self.check_list(params);

        for (index, param_type) in metadata.parameters.iter().enumerate() {
            if let NodeType::Type(ty) = param_type {
                if metadata.generics.contains(&ty) {
                    self.report_error(Diagnostic::error(
                        &params[index],
                        "Can only refer to generic types behind a pointer",
                    ));
                }
            }
        }

        let analysis = self.check_list(body);

        self.pop_scope();

        if !analysis.guarantees_return && !return_type.matches(&NodeType::Void) && !self.is_builtin
        {
            let last_param = params.last().map(|p| p.span.clone());
            let span_params = Span::join_opt(name, &last_param);
            let span = Span::join_opt(&span_params, explicit_return_type);
            self.report_error(Diagnostic::error(&span, "Function may not return"));
        }

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_variable_decl(
        &mut self,
        _stmt: &Stmt,
        name: &TypedToken,
        explicit_type: &Option<ExplicitType>,
        value: &Option<Expr>,
    ) -> Analysis {
        let explicit_type =
            explicit_type
                .as_ref()
                .and_then(|k| match self.resolve_explicit_type(k) {
                    Ok(explicit_type) => Some(explicit_type),
                    Err(diagnostic) => {
                        self.report_error(diagnostic);
                        None
                    }
                });

        let implicit_type = value.as_ref().and_then(|v| self.check_expr(v));

        match (explicit_type, implicit_type) {
            (Some(explicit), Some(implicit)) => {
                self.current_scope().define_var(name, &explicit);

                if let Err(diag) =
                    self.check_type_match(&value.as_ref().unwrap(), &implicit, &explicit)
                {
                    self.report_error(diag);
                }
                let _ = value.as_ref().unwrap().set_type(explicit.clone());
            }
            (Some(explicit), None) => self.current_scope().define_var(name, &explicit),
            (None, Some(implicit)) if !implicit.contains_ambiguity() => {
                self.current_scope().define_var(name, &implicit)
            }
            _ => {
                self.report_error(Diagnostic::error(name, "Can't infer type"));
            }
        }

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_if_stmt(
        &mut self,
        _stmt: &Stmt,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) -> Analysis {
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

    fn visit_while_stmt(&mut self, _stmt: &Stmt, condition: &Expr, body: &[Stmt]) -> Analysis {
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

    fn visit_for_stmt(
        &mut self,
        _stmt: &Stmt,
        variable: &TypedToken,
        array: &Expr,
        body: &[Stmt],
    ) -> Analysis {
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

    fn visit_print_stmt(
        &mut self,
        _stmt: &Stmt,
        expr: &Option<Expr>,
        print_type: &RefCell<Option<NodeType>>,
    ) -> Analysis {
        if let Some(node_type) = expr.as_ref().and_then(|e| self.check_expr(e)) {
            match node_type {
                NodeType::Int | NodeType::Bool => {
                    print_type.replace(Some(node_type));
                }
                node_type if node_type.is_pointer_to(NodeType::Byte) => {
                    print_type.replace(Some(node_type));
                }
                _ => {
                    let message = format!("Can't print object of type {}", node_type);
                    self.report_error(Diagnostic::error(expr.as_ref().unwrap(), &message));
                }
            }
        }

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, expr: &Expr) -> Analysis {
        self.check_expr(expr);
        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_builtin_stmt(&mut self, _stmt: &Stmt, inner: &Box<Stmt>) -> Self::StmtResult {
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

        if lhs_type.contains_ambiguity() {
            return Err(Diagnostic::error(lhs, "Cannot infer type"));
        }
        if rhs_type.contains_ambiguity() {
            return Err(Diagnostic::error(lhs, "Cannot infer type"));
        }

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

        if operand_type.contains_ambiguity() {
            return Err(Diagnostic::error(operand, "Cannot infer type"));
        }

        if let TokenKind::Ampersand = op.kind {
            let boxed_type = Box::new(operand_type.clone());
            return Ok(NodeType::Pointer(boxed_type));
        }

        if let TokenKind::Star = op.kind {
            return match operand_type {
                NodeType::Pointer(inner) => Ok((*inner).clone()),
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
        specializations: &[ExplicitType],
        args: &[Expr],
    ) -> Self::ExprResult {
        let func_symbol = if let Some(target) = target {
            let target_type = target.accept(self)?;

            let object_symbol = match target_type {
                NodeType::Type(type_symbol) => Ok(type_symbol),
                NodeType::Metatype(type_symbol) => Ok(Symbol::meta_symbol(Some(&type_symbol))),
                _ => Err(Diagnostic::error(
                    expr,
                    &format!("Cannot call method on a {}", target_type),
                )),
            }?;

            let method_symbol = Symbol::new(Some(&object_symbol), &function.token);

            match self.lib.resolve_symbol(&method_symbol) {
                Some(NodeType::Function(..)) => Ok(method_symbol),
                Some(_) => Err(Diagnostic::error(
                    &Span::join(target, function),
                    &format!(
                        "'{}' is a property, not a method of {}",
                        function.span().lexeme(),
                        object_symbol.id,
                    ),
                )),
                None => Err(Diagnostic::error(
                    &Span::join(target, function),
                    &format!(
                        "Type '{}' does not have method '{}'",
                        object_symbol.id,
                        function.span().lexeme()
                    ),
                )),
            }
        } else {
            let resolved = self.resolve_var(function.span().lexeme());
            if resolved.is_none() {
                return Err(Diagnostic::error(function, "Undefined function"));
            }
            let (found_symbol, node_type) = resolved.unwrap();

            match node_type {
                NodeType::Function(..) => Ok(found_symbol),
                NodeType::Metatype(symbol) => {
                    let meta_symbol = Symbol::meta_symbol(Some(&symbol));
                    Ok(Symbol::init_symbol(Some(&meta_symbol)))
                }
                _ => Err(Diagnostic::error(
                    function,
                    &format!("Cannot call type {}", node_type),
                )),
            }
        }?;

        function.set_symbol(func_symbol.clone());

        let metadata = self.lib.function_metadata(&func_symbol).unwrap();

        let specialization: std::result::Result<Vec<NodeType>, _> = specializations
            .iter()
            .map(|s| self.resolve_explicit_type(s))
            .collect();
        let specialization = specialization?;
        let specialization = if specialization.is_empty() {
            None
        } else {
            Some(GenericSpecialization::new(&func_symbol, specialization))
        };

        if specializations.len() != metadata.generics.len() {
            let message = format!(
                "Expected {} specializations, got {}",
                metadata.generics.len(),
                specializations.len()
            );
            return Err(Diagnostic::error(function, &message));
        }

        let return_type = metadata.return_type.specialize(specialization.as_ref());

        self.check_function_arguments(
            expr,
            &function.get_symbol().unwrap(),
            &specialization,
            &metadata.parameters,
            args,
        )?;

        if let Some(specialization) = specialization {
            let vector = self.specialization_map.entry(func_symbol).or_insert(Vec::new());
            if vector.iter().position(|s| s.id == specialization.id).is_none() {
                vector.push(specialization);
            }
        }

        expr.set_type(return_type)
    }

    fn visit_field_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        field: &ResolvedToken,
    ) -> Self::ExprResult {
        let target_type = target.accept(self)?;

        let type_symbol = match target_type {
            NodeType::Type(type_symbol) => Ok(type_symbol),
            NodeType::Metatype(type_symbol) => Ok(Symbol::meta_symbol(Some(&type_symbol))),
            _ => Err(Diagnostic::error(
                target,
                &format!("Cannot access property of a {}", target_type),
            )),
        }?;

        let field_symbol = Symbol::new(Some(&type_symbol), &field.token);

        if let Some(field_type) = self.lib.resolve_symbol(&field_symbol) {
            field.set_symbol(field_symbol);
            expr.set_type(field_type.clone())
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
            name.set_symbol(found_symbol);
            expr.set_type(node_type.clone())
        } else {
            Err(Diagnostic::error(name, "Undefined variable"))
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
        _expr: &Expr,
        explicit_type: &ExplicitType,
        value: &Expr,
    ) -> Self::ExprResult {
        value.accept(self)?;
        self.resolve_explicit_type(explicit_type)
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

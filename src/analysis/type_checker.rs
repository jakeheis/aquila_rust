use super::node_type::*;
use super::symbol_table::*;
use crate::diagnostic::*;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::*;
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

type Result = DiagnosticResult<NodeType>;

struct Scope {
    id: Option<Symbol>,
    symbols: SymbolTable,
    function_return_type: Option<NodeType>,
    specialization_map: HashMap<Symbol, Symbol>
}

impl Scope {
    fn new(id: Option<Symbol>, return_type: Option<NodeType>) -> Self {
        Scope {
            id,
            symbols: SymbolTable::new(),
            function_return_type: return_type,
            specialization_map: HashMap::new(),
        }
    }

    fn define_var(&mut self, name: &TypedToken, var_type: &NodeType) {
        if let NodeType::Ambiguous = var_type {
            panic!("Should never define var as ambiguous");
        }

        let new_symbol = Symbol::new((&self.id).as_ref(), &name.token);

        self.symbols.insert(new_symbol.clone(), var_type.clone());

        name.set_symbol(new_symbol);
        name.set_type(var_type.clone());
    }
}

pub struct TypeChecker {
    reporter: Rc<dyn Reporter>,
    lib: Rc<Lib>,
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
            scopes: vec![top_level],
            is_builtin: false,
        };

        checker.check_list(&lib.type_decls);
        checker.check_list(&lib.function_decls);

        checker.push_scope_named("main");
        checker.check_list(&lib.other);
        checker.pop_scope();

        std::mem::drop(checker);

        Rc::try_unwrap(lib).ok().unwrap()
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

    fn current_symbol(&self) -> Option<&Symbol> {
        self.scopes.last().unwrap().id.as_ref()
    }

    fn push_type_scope(&mut self, name: &TypedToken) -> Symbol {
        self.push_scope_named(name.span().lexeme())
    }

    fn push_function_scope(&mut self, name: &TypedToken, ret_type: NodeType) -> Symbol {
        let symbol = Symbol::new(self.current_symbol(), &name.token);
        self.scopes
            .push(Scope::new(Some(symbol.clone()), Some(ret_type)));
        symbol
    }

    fn push_scope_named(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new_str(self.current_symbol(), name);
        let ret_type = self.current_scope().function_return_type.clone();
        self.scopes.push(Scope::new(Some(symbol.clone()), ret_type));
        symbol
    }

    fn push_scope_meta(&mut self) -> Symbol {
        let symbol = Symbol::meta_symbol(self.current_symbol());
        let ret_type = self.current_scope().function_return_type.clone();
        self.scopes.push(Scope::new(Some(symbol.clone()), ret_type));
        symbol
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
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
            let _ = expr.set_type(expected.clone());
            Ok(())
        } else {
            Err(self.type_mismatch(expr, given, expected))
        }
    }

    fn check_function_arguments(
        &mut self,
        expr: &Expr,
        function_symbol: &Symbol,
        map: &HashMap<Symbol, NodeType>,
        param_types: &[NodeType],
        args: &[Expr],
    ) -> DiagnosticResult<()> {
        println!("checking params of {}", function_symbol);
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
            let param_spec = self.specialize_type(param, &map);
            self.check_type_match(&args[index], &arg, &param_spec)?;
        }

        Ok(())
    }

    fn specialize(&self, node_type: &NodeType, generics: &[Symbol], specializations: &[NodeType]) -> NodeType {
        println!("checking if {} should be spcialized", node_type);
        match node_type {
            NodeType::Type(potential_generic) => {
                if let Some(index) = generics.iter().position(|g| g == potential_generic) {
                    println!("specialzngi {} to {}", node_type, specializations[index].clone());
                    return specializations[index].clone();
                }
            },
            NodeType::Pointer(to) => {
                return NodeType::pointer_to(self.specialize(to, generics, specializations))
            },
            NodeType::Array(of, size) => {
                let specialized = self.specialize(of, generics, specializations);
                return NodeType::Array(Box::new(specialized), *size);
            },
            _ => (),
        }

        node_type.clone()
    }

    fn specialize_type(&self, node_type: &NodeType, map: &HashMap<Symbol, NodeType>) -> NodeType {
        match node_type {
            NodeType::Type(potential_generic) => {
                if let Some(node_type) = map.get(&potential_generic) {
                    return node_type.clone();
                }
            },
            NodeType::Pointer(to) => {
                return NodeType::pointer_to(self.specialize_type(&to, map))
            },
            NodeType::Array(of, size) => {
                let specialized = self.specialize_type(&of, map);
                return NodeType::Array(Box::new(specialized), *size);
            },
            _ => (),
        }

        node_type.clone()
    }

    // fn specialize_type<T: ContainsSpan>(&self, span: &T, node_type: &NodeType, map: &HashMap<Symbol, NodeType>) -> Result {
    //     match node_type {
    //         NodeType::Type(potential_generic) => {
    //             if map.contains_key(&potential_generic) {
    //                 return Err(Diagnostic::error(span, "Can only use generics behind a pointer"))
    //             }
    //         },
    //         NodeType::Pointer(to) => {
    //             let to: &NodeType = &to;
    //             match to {
    //                 NodeType::Type(potential_generic) => {
    //                     if let Some(mapped) = map.get(&potential_generic) {
    //                         return Ok(NodeType::pointer_to(mapped.clone()));
    //                     }
    //                 },
    //                 _ => {
    //                     let new_inner = self.specialize_type(span, to, map)?;
    //                     return Ok(NodeType::pointer_to(new_inner));
    //                 }
    //             }
    //         },
    //         NodeType::Array(of, size) => {
    //             let inner = self.specialize_type(span, of, map)?;
    //             return Ok(NodeType::Array(Box::new(inner), *size));
    //         },
    //         _ => (),
    //     }

    //     Ok(node_type.clone())
    // }

    fn resolve_explicit_type(
        &mut self,
        explicit_type: &ExplicitType,
    ) -> Result {
        let context: Vec<Symbol> = self
            .scopes
            .iter()
            .flat_map(|s| s.id.as_ref().map(|id| id.clone()))
            .collect();

        if let Some(deduced) = explicit_type.resolve(&self.lib, &context) {
            if let (&NodeType::Any, false) = (&deduced, self.is_builtin) {
                Err(Diagnostic::error(explicit_type, "Cannot have type any; must be ptr any"))
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
        let symbol = self.push_type_scope(name);
        name.set_symbol(symbol);

        self.push_scope_meta();
        self.check_list(meta_methods);
        self.pop_scope();

        self.check_list(fields);

        for method in methods {
            guard!(StmtKind::FunctionDecl[name, generics, _one, _two, _three, _four] = &method.kind);
            let name = name.clone();
            let symbol = Symbol::new(self.current_symbol(), &name.token);
            let function_type = self.lib.resolve_symbol(&symbol).unwrap().clone();
            self.current_scope().define_var(&name, &function_type);
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
        let symbol = self.push_scope_named(name.token.lexeme());
        name.set_symbol(symbol.clone());

        let mut generic_symbols: Vec<Symbol> = Vec::new();

        for (index, generic) in generics.iter().enumerate() {
            let generic_symbol = Symbol::new(self.current_symbol(), &generic.token);
            self.current_scope().define_var(generic, &NodeType::Metatype(generic_symbol.clone()));
            generic_symbols.push(generic_symbol);

            // let gen_type = NodeType::Generic(self.current_symbol().unwrap().clone(), index);
            // self.current_scope().define_var(generic, &gen_type);
        }
        
        let return_type = if let Some(explicit_return_type) = explicit_return_type.as_ref() {
            match self.resolve_explicit_type(explicit_return_type) {
                Ok(NodeType::Array(..)) => {
                    self.report_error(Diagnostic::error(
                        explicit_return_type,
                        "Cannot return an array",
                    ));
                    NodeType::Ambiguous
                },
                Ok(node_type) => node_type,
                Err(diag) => {
                    self.report_error(diag);
                    NodeType::Ambiguous
                }
            }
        } else {
            NodeType::Void
        };

        self.current_scope().function_return_type = Some(return_type.clone());

        self.check_list(params);
        for param in params {
            if let StmtKind::VariableDecl(name, ..) = &param.kind {
                if let Some(NodeType::Type(ty)) = name.get_type() {
                    if generic_symbols.contains(&ty) {
                        self.report_error(Diagnostic::error(param, "Can only refer to generic types behind a pointer"));
                    }
                }
            }
        }

        let analysis = self.check_list(body);
        // self.pop_scope();
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
        let explicit_type = explicit_type.as_ref().and_then(|k| match self.resolve_explicit_type(k) {
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

        let expected_return = &self.current_scope().function_return_type;
        let expected_return = expected_return
            .as_ref()
            .map(|f| f.clone())
            .unwrap_or(NodeType::Void);

        if let Some(expr) = expr.as_ref() {
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
        function: &ResolvedToken,
        specializations: &[ExplicitType],
        args: &[Expr],
    ) -> Self::ExprResult {
        let resolved = self.resolve_var(function.span().lexeme());
        if resolved.is_none() {
            return Err(Diagnostic::error(function, "Undefined function"));
        }
        let (found_symbol, node_type) = resolved.unwrap();

        let (params, generics, return_type): (Vec<NodeType>, Vec<Symbol>, Box<NodeType>) = match node_type {
            NodeType::Function(params, generics, ret) => {
                function.set_symbol(found_symbol);
                Ok((params, generics, ret))
            }
            NodeType::Metatype(symbol) => {
                let meta_symbol = Symbol::meta_symbol(Some(&symbol));
                let init_symbol = Symbol::init_symbol(Some(&meta_symbol));

                function.set_symbol(init_symbol.clone());

                let init_type = self.lib.resolve_symbol(&init_symbol).unwrap();
                guard!(NodeType::Function[p, specializations, r] = init_type);
                Ok((p, specializations, r))
            }
            _ => Err(Diagnostic::error(
                function,
                &format!("Cannot call type {}", node_type),
            )),
        }?;

        let mut specialization_map: HashMap<Symbol, NodeType> = HashMap::new();

        let specializations: std::result::Result<Vec<NodeType>, _> = specializations.iter().map(|s| self.resolve_explicit_type(s)).collect();
        let specializations = specializations?;

        if specializations.len() != generics.len() {
            let message = format!("Expected {} specializations, got {}", generics.len(), specializations.len());
            return Err(Diagnostic::error(function, &message))
        }

        for (spec, gen) in specializations.iter().zip(generics) {
            specialization_map.insert(gen, spec.clone());
        }

        let return_type: NodeType = (*return_type).clone();
        let return_type = self.specialize_type(&return_type, &specialization_map);

        self.check_function_arguments(expr, &function.get_symbol().unwrap(), &specialization_map, &params, args)?;

        expr.set_type(return_type)
    }

    fn visit_method_call_expr(
        &mut self,
        expr: &Expr,
        object: &Expr,
        method: &ResolvedToken,
        specializations: &[ExplicitType],
        args: &[Expr],
    ) -> Self::ExprResult {
        let object_type = object.accept(self)?;

        let object_symbol = match object_type {
            NodeType::Type(type_symbol) => Ok(type_symbol),
            NodeType::Metatype(type_symbol) => Ok(Symbol::meta_symbol(Some(&type_symbol))),
            _ => Err(Diagnostic::error(
                expr,
                &format!("Cannot call method on a {}", object_type),
            )),
        }?;

        let method_symbol = Symbol::new(Some(&object_symbol), &method.token);

        match self.lib.resolve_symbol(&method_symbol) {
            Some(NodeType::Function(params, generics, ret_val)) => {
                method.set_symbol(method_symbol.clone());

                let mut specialization_map: HashMap<Symbol, NodeType> = HashMap::new();

                let specializations: std::result::Result<Vec<NodeType>, _> = specializations.iter().map(|s| self.resolve_explicit_type(s)).collect();
                let specializations = specializations?;
        
                if specializations.len() != generics.len() {
                    let message = format!("Expected {} specializations, got {}", generics.len(), specializations.len());
                    return Err(Diagnostic::error(method, &message))
                }
        
                for (spec, gen) in specializations.iter().zip(generics) {
                    specialization_map.insert(gen, spec.clone());
                }

                let ret_val = (*ret_val).clone();
                let return_type = self.specialize_type(&ret_val, &specialization_map);

                self.check_function_arguments(expr, &method_symbol, &specialization_map, &params, args)?;

                expr.set_type(return_type)
            }
            Some(_) => Err(Diagnostic::error(
                &Span::join(object, method),
                &format!(
                    "'{}' is a property, not a method of {}",
                    method.span().lexeme(),
                    object_symbol.id,
                ),
            )),
            None => Err(Diagnostic::error(
                &Span::join(object, method),
                &format!(
                    "Type '{}' does not has method '{}'",
                    object_symbol.id,
                    method.span().lexeme()
                ),
            )),
        }
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

        let node_type = NodeType::Array(
            Box::new(expected_type.clone()),
            elements.len(),
        );
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

    fn visit_cast_expr(&mut self, _expr: &Expr, explicit_type: &ExplicitType, value: &Expr) -> Self::ExprResult {
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

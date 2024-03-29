use super::check;
use super::scope::{ScopeDefinition, SymbolResolution, GenericInfo};
use super::generic_inference;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use log::trace;

pub struct ExprChecker<'a> {
    lib: Symbol,
    resolver: SymbolResolution<'a>,
    all_symbols: &'a SymbolStore,
}

impl<'a> ExprChecker<'a> {
    pub fn new(lib: Symbol, resolver: SymbolResolution<'a>, all_symbols: &'a SymbolStore) -> Self {
        ExprChecker { lib, resolver, all_symbols }
    }

    fn set_expr_type(&self, expr: &Expr, node_type: NodeType) -> DiagnosticResult<NodeType> {
        let mut expr_type = node_type.clone();
        if let NodeType::GenericInstance(type_sym) = node_type {
            for scope in self.resolver.scopes.iter().rev() {
                if let Some(restrictions) = scope.generic_restrictions.get(&type_sym) {
                    for restrict in restrictions {
                        if let GenericInfo::Is(other) = restrict {
                            expr_type = other.clone();
                            break;
                        }
                    }
                }
            }
        }

        expr.set_type(expr_type)
    }

    fn retrieve_method<S: ContainsSpan>(
        &self,
        span: &S,
        target: &NodeType,
        method: &str,
    ) -> DiagnosticResult<(Symbol, GenericSpecialization)> {
        let err = Err(Diagnostic::error(
            span,
            &format!("Cannot call method on a {}", target),
        ));

        let (target_symbol, specialization, is_meta) = match target {
            NodeType::Instance(t, s) => (t, s, false),
            NodeType::Metatype(inner) => {
                if let NodeType::Instance(t, s) = inner.as_ref() {
                    (t, s, true)
                } else {
                    return err;
                }
            },
            NodeType::GenericInstance(sym) => {
                for scope in self.resolver.scopes.iter().rev() {
                    if let Some(restrictions) = scope.generic_restrictions.get(sym) {
                        for restriction in restrictions {
                            match restriction {
                                GenericInfo::Conforms(trait_sym) => {
                                    let trait_metadata = self.all_symbols.trait_metadata_symbol(trait_sym).unwrap();
                                    if trait_metadata.function_requirements.iter().any(|m| m == method) {
                                        let sym = trait_metadata.symbol.child(method);
                                        return Ok((sym, GenericSpecialization::empty()));
                                    }
                                }
                                GenericInfo::Is(restricted_type) => {
                                    return self.retrieve_method(span, restricted_type, method);
                                }
                            }
                        }
                    }
                }

                let message = format!("Type '{}' does not have method '{}'", target, method);
                return Err(Diagnostic::error(span, &message));
            }
            NodeType::Reference(inside) => return self.retrieve_method(span, inside, method),
            _ => return err
        };

        let target_metadata = self.all_symbols.type_metadata(target_symbol).unwrap(); 
        let method_metadata = if is_meta {
            let name = target_metadata.meta_method_named(method);
            name.map(|n| target_symbol.meta_symbol().child(&n))
        } else {
            let name = target_metadata.method_named(method);
            name.map(|n| target_symbol.child(&n))
        };

        match method_metadata {
            Some(method) => Ok((method, specialization.clone())),
            None => {
                let message = format!("Type '{}' does not have method '{}'", target, method);
                Err(Diagnostic::error(span, &message))
            }
        }
    }

    fn resolve_function<S: ContainsSpan>(
        &self,
        span: &S,
        function: &SpecializedToken,
    ) -> DiagnosticResult<(&FunctionMetadata, GenericSpecialization, bool)> {
        match self.resolver.resolve_token(function) {
            Some(ScopeDefinition::Function(sym)) => {
                let metadata = self.all_symbols.function_metadata(&sym).unwrap();
                match &metadata.kind {
                    FunctionKind::Method(owner) | FunctionKind::MetaMethod(owner) => {
                        let implicit_self = self.all_symbols.type_metadata(owner).unwrap();
                        let implicit_spec = implicit_self.dummy_specialization();
                        return Ok((metadata, implicit_spec, false));
                    }
                    _ => return Ok((metadata, GenericSpecialization::empty(), false)),
                }
            }
            Some(ScopeDefinition::Variable(_, node_type)) => {
                return Err(Diagnostic::error(
                    span,
                    &format!("Cannot call object of type {}", node_type),
                ))
            }
            Some(ScopeDefinition::ExplicitType(result)) => match result {
                Ok(NodeType::Instance(type_symbol, specs)) => {
                    let init_metadata = self
                        .all_symbols
                        .function_metadata(&type_symbol.meta_symbol().init_symbol());
                    Ok((init_metadata.unwrap(), specs.clone(), true))
                }
                Ok(..) => Err(Diagnostic::error(
                    function.span(),
                    "Cannot call a primitive",
                )),
                Err(diag) => Err(diag),
            },
            None => Err(Diagnostic::error(function.span(), "Undefined function")),
        }
    }

    fn check_specialization_restrictions(
        &self,
        specialization: &GenericSpecialization,
        restrictions: &[(Symbol, Symbol)],
        span: &Span,
    ) -> DiagnosticResult<()> {
        for (generic_symbol, trait_symbol) in restrictions {
            let specialized_type = specialization.type_for(&generic_symbol).unwrap();
            match specialized_type {
                NodeType::Instance(type_sym, ..) => {
                    let metadata = self.all_symbols.type_metadata(&type_sym).unwrap();
                    if !metadata.conforms_to(trait_symbol) {
                        let message = format!(
                            "Type '{}' does not implement '{}'",
                            type_sym.name(),
                            trait_symbol.name()
                        );
                        return Err(Diagnostic::error(span, &message));
                    }
                },
                NodeType::GenericInstance(sym) => {
                    let mut implements = false;
                    for scope in self.resolver.scopes.iter().rev() {
                        let generic_restrictions = &scope.generic_restrictions;
                        let gen_infos = generic_restrictions.get(sym).map(|g| g.as_slice()).unwrap_or(&[]);
                        for gen_info in gen_infos {
                            if let GenericInfo::Conforms(trait_sym) = gen_info {
                                if trait_sym == trait_symbol {
                                    implements = true;
                                    break;
                                }
                            }
                        }
                    }

                    if !implements {
                        let message = format!(
                            "Generic '{}' is not constrained to types which implement '{}'",
                            sym.name(),
                            trait_symbol.name()
                        );
                        return Err(Diagnostic::error(span, &message));
                    }
                }
                other => {
                    let message = format!("Type '{}' does not implement '{}'", other, trait_symbol.name());
                    return Err(Diagnostic::error(span, &message));
                }
            }
        }

        Ok(())
    }
}

impl<'a> ExprVisitor for ExprChecker<'a> {
    type ExprResult = DiagnosticResult<NodeType>;

    fn visit_binary_expr(
        &mut self,
        expr: &Expr,
        lhs: &Expr,
        op: &Token,
        rhs: &Expr,
    ) -> Self::ExprResult {
        let lhs_type = lhs.accept(self)?;
        let rhs_type = rhs.accept(self)?;

        check::ensure_no_amibguity(lhs, &lhs_type)?;
        check::ensure_no_amibguity(rhs, &rhs_type)?;

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
            self.set_expr_type(expr, matching_entry.2.clone())
        } else {
            let message = format!("Cannot {} on {} and {}", op.lexeme(), lhs_type, rhs_type);
            Err(Diagnostic::error(&Span::join(lhs, rhs), &message))
        }
    }

    fn visit_unary_expr(&mut self, expr: &Expr, op: &Token, operand: &Expr) -> Self::ExprResult {
        let operand_type = operand.accept(self)?;

        check::ensure_no_amibguity(expr, &operand_type)?;

        if let TokenKind::Ampersand = op.kind {
            let expr_type = NodeType::pointer_to(operand_type.clone());
            return self.set_expr_type(expr, expr_type);
        }

        if let TokenKind::At = op.kind {
            let expr_type = NodeType::reference_to(operand_type.clone());
            return self.set_expr_type(expr, expr_type);
        }

        if let TokenKind::Star = op.kind {
            return match operand_type {
                NodeType::Pointer(inner) | NodeType::Reference(inner) => self.set_expr_type(expr, (*inner).clone()),
                _ => {
                    let message = format!("Cannot dereference object of type {}", operand_type);
                    Err(Diagnostic::error(operand, &message))
                }
            };
        }

        let entries: &[NodeType] = match op.kind {
            TokenKind::Minus => &NEGATE_ENTRIES,
            TokenKind::Bang => &INVERT_ENTRIES,
            _ => unreachable!(),
        };

        if entries.iter().find(|e| e.matches(&operand_type)).is_some() {
            self.set_expr_type(expr, operand_type.clone())
        } else {
            let message = format!("Cannot {} on {}", op.lexeme(), operand_type);
            Err(Diagnostic::error(operand, &message))
        }
    }

    fn visit_function_call_expr(&mut self, expr: &Expr, call: &FunctionCall) -> Self::ExprResult {
        let arg_types: DiagnosticResult<Vec<NodeType>> =
            call.arguments.iter().map(|a| a.accept(self)).collect();
        let arg_types = arg_types?;

        let function_name = call.name.token.lexeme();

        let (metadata, target_specialization, is_init) = if let Some(target) = &call.target {
            let target_type = target.accept(self)?;
            let (method, target_specialization) =
                self.retrieve_method(expr, &target_type, function_name)?;

            let method_metadata = self.all_symbols.function_metadata(&method).unwrap();
            (method_metadata, target_specialization, false)
        } else {
            self.resolve_function(expr, &call.name)?
        };

        if check::func_accessible(&metadata, &self.lib) == false {
            if is_init {
                return Err(Diagnostic::error(expr, "Init is private"));
            } else {
                return Err(Diagnostic::error(&call.name, "Function is private"));
            }
        }

        call.name.set_symbol(metadata.symbol.clone());

        trace!(target: "type_checker", "Callee metadata: {}", metadata);

        if metadata.parameters.len() != arg_types.len() {
            let message = format!(
                "Expected {} argument(s), got {}",
                metadata.parameters.len(),
                arg_types.len()
            );
            return Err(Diagnostic::error(expr, &message));
        }

        let function_specialization = if is_init {
            GenericSpecialization::empty()
        } else if call.name.specialization.is_empty() {
            generic_inference::infer_from_args(
                &metadata, 
                &arg_types,
                &target_specialization, 
                call.name.span(), 
                &call.arguments
            )?
        } else {
            if call.name.specialization.len() != metadata.generics.len() {
                let message = format!(
                    "Expected {} specializations, got {}",
                    metadata.generics.len(),
                    call.name.specialization.len()
                );
                return Err(Diagnostic::error(call.name.span(), &message));
            }
            let specialization: std::result::Result<Vec<NodeType>, _> = call
                .name
                .specialization
                .iter()
                .map(|s| self.resolver.resolve_explicit_type_or_err(s))
                .collect();
            GenericSpecialization::new(&metadata.symbol, &metadata.generics, specialization?)
        };

        let full_call_specialization = function_specialization.merge(&target_specialization);

        self.check_specialization_restrictions(
            &full_call_specialization,
            &metadata.generic_restrictions,
            call.name.span(),
        )?;

        call.set_specialization(full_call_specialization.clone());

        let function_type = metadata.full_type().specialize(&full_call_specialization);

        for ((index, param), arg) in function_type
            .parameters
            .into_iter()
            .enumerate()
            .zip(arg_types)
        {
            check::ensure_no_amibguity(&call.arguments[index], &arg)?;
            check::check_type_match(&call.arguments[index], &arg, &param)?;
        }

        self.set_expr_type(expr, function_type.return_type)
    }

    fn visit_field_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        field_token: &SpecializedToken,
    ) -> Self::ExprResult {
        let target_type = target.accept(self)?;
        let field_name = field_token.token.lexeme();

        let result = match &target_type {
            NodeType::Instance(type_symbol, specialization) => Some((type_symbol, specialization)),
            NodeType::Reference(to) => {
                if let NodeType::Instance(type_symbol, specialization) = to.as_ref() {
                    Some((type_symbol, specialization))   
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some((type_symbol, specialization)) = result {
            let type_metadata = self.all_symbols.type_metadata(&type_symbol).unwrap();
            if let Some(field) = type_metadata.field_named(field_name) {
                if field.public || check::symbol_accessible(&type_symbol, &self.lib) {
                    field_token.set_symbol(type_symbol.child(&field.name));
                    self.set_expr_type(expr, field.var_type.specialize(&specialization))
                } else {
                    Err(Diagnostic::error(field_token, "Field is private"))
                }
            } else {
                Err(Diagnostic::error(
                    &Span::join(target, field_token.span()),
                    &format!(
                        "Type '{}' does not has field '{}'",
                        type_symbol.name(),
                        field_token.span().lexeme()
                    ),
                ))
            }
        } else {
            Err(Diagnostic::error(
                target,
                &format!("Cannot access property of '{}'", target_type),
            ))
        }
    }

    fn visit_literal_expr(&mut self, expr: &Expr, token: &Token) -> Self::ExprResult {
        let node_type = match token.kind {
            TokenKind::Int => NodeType::Int,
            TokenKind::Double => NodeType::Double,
            TokenKind::True | TokenKind::False => NodeType::Bool,
            TokenKind::StringLiteral => NodeType::pointer_to(NodeType::Byte),
            _ => panic!(),
        };
        self.set_expr_type(expr, node_type)
    }

    fn visit_variable_expr(&mut self, expr: &Expr, name: &SpecializedToken) -> Self::ExprResult {
        match self.resolver.resolve_token(name) {
            Some(ScopeDefinition::Variable(sym, var_type)) => {
                if !name.specialization.is_empty() {
                    return Err(Diagnostic::error(expr, "Cannot specialize variable"));
                }
                name.set_symbol(sym);
                self.set_expr_type(expr, var_type.clone())
            }
            Some(ScopeDefinition::Function(..)) => {
                Err(Diagnostic::error(expr, "Cannot use function as variable"))
            }
            Some(ScopeDefinition::ExplicitType(result)) => match result {
                Ok(NodeType::Instance(symbol, spec)) => {
                    trace!(target: "type_checker", "Treating variable as metatype of {}", symbol);
                    name.set_symbol(symbol.clone());
                    let boxed = Box::new(NodeType::Instance(symbol, spec));
                    self.set_expr_type(expr, NodeType::Metatype(boxed))
                }
                Ok(NodeType::GenericInstance(sym)) => {
                    trace!(target: "type_checker", "Treating variable as metatype of {}", sym);
                    name.set_symbol(sym.clone());
                    let boxed = Box::new(NodeType::GenericInstance(sym));
                    self.set_expr_type(expr, NodeType::Metatype(boxed))
                }
                Ok(_) => Err(Diagnostic::error(expr, "Type not allowed here")),
                Err(diag) => Err(diag),
            },
            None => Err(Diagnostic::error(name.span(), "Undefined variable")),
        }
    }

    fn visit_array_expr(&mut self, expr: &Expr, elements: &[Expr]) -> Self::ExprResult {
        if elements.is_empty() {
            let node_type = NodeType::Array(Box::new(NodeType::Ambiguous), 0);
            return self.set_expr_type(expr, node_type);
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
            if let Err(diag) = check::check_type_match(&elements[index], element_type, &expected_type) {
                return Err(diag);
            }
        }

        let node_type = NodeType::Array(Box::new(expected_type.clone()), elements.len());
        self.set_expr_type(expr, node_type)
    }

    fn visit_subscript_expr(&mut self, expr: &Expr, target: &Expr, arg: &Expr) -> Self::ExprResult {
        let target_type = target.accept(self)?;
        let arg_type = arg.accept(self)?;

        check::check_type_match(arg, &arg_type, &NodeType::Int)?;

        if let NodeType::Array(inside, _) = target_type {
            self.set_expr_type(expr, (*inside).clone())
        } else {
            Err(Diagnostic::error(expr, "Can't subscript into non-array"))
        }
    }

    fn visit_cast_expr(
        &mut self,
        expr: &Expr,
        explicit_type: &ExplicitType,
        value: &Expr,
    ) -> Self::ExprResult {
        value.accept(self)?;
        self.set_expr_type(expr, self.resolver.resolve_explicit_type_or_err(explicit_type)?)
    }
}

const NEGATE_ENTRIES: [NodeType; 2] = [NodeType::Int, NodeType::Double];

const INVERT_ENTRIES: [NodeType; 1] = [NodeType::Bool];

type BinaryEntry = (NodeType, NodeType, NodeType);

const ADDITION_ENTRIES: [BinaryEntry; 4] = [
    (NodeType::Int, NodeType::Int, NodeType::Int),
    (NodeType::Double, NodeType::Int, NodeType::Double),
    (NodeType::Int, NodeType::Double, NodeType::Double),
    (NodeType::Double, NodeType::Double, NodeType::Double),
];

const MATH_ENTRIES: [BinaryEntry; 4] = [
    (NodeType::Int, NodeType::Int, NodeType::Int),
    (NodeType::Double, NodeType::Int, NodeType::Double),
    (NodeType::Int, NodeType::Double, NodeType::Double),
    (NodeType::Double, NodeType::Double, NodeType::Double),
];

const LOGIC_ENTRIES: [BinaryEntry; 1] = [(NodeType::Bool, NodeType::Bool, NodeType::Bool)];

const EQUALITY_ENTRIES: [BinaryEntry; 3] = [
    (NodeType::Int, NodeType::Int, NodeType::Bool),
    (NodeType::Double, NodeType::Double, NodeType::Bool),
    (NodeType::Bool, NodeType::Bool, NodeType::Bool),
];

const COMPARISON_ENTRIES: [BinaryEntry; 4] = [
    (NodeType::Int, NodeType::Int, NodeType::Bool),
    (NodeType::Int, NodeType::Double, NodeType::Bool),
    (NodeType::Double, NodeType::Int, NodeType::Bool),
    (NodeType::Double, NodeType::Double, NodeType::Bool),
];

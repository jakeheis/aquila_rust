use super::symbol_table::*;
use crate::diagnostic::*;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::*;
use std::cell::RefCell;
use std::rc::Rc;

type Result = DiagnosticResult<NodeType>;

struct Scope {
    id: Option<Symbol>,
    symbols: SymbolTable,
    function_return_type: Option<NodeType>,
}

impl Scope {
    fn new(id: Option<Symbol>, return_type: Option<NodeType>) -> Self {
        Scope {
            id,
            symbols: SymbolTable::new(),
            function_return_type: return_type,
        }
    }

    fn define_var(&mut self, name: &TypedToken, var_type: &NodeType) {
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

    fn resolve_var(&self, name: &str) -> Option<(Symbol, &NodeType)> {
        for scope in self.scopes.iter().rev() {
            let possible_symbol = Symbol::new_str((&scope.id).as_ref(), name);
            if let Some(node_type) = scope.symbols.get_type(&possible_symbol) {
                return Some((possible_symbol, node_type));
            }
        }

        let lib_symbol = Symbol::new_str(None, name);
        if let Some(node_type) = self.lib.resolve_symbol(&lib_symbol) {
            Some((lib_symbol, node_type))
        } else {
            None
        }
    }

    fn check_function_arguments(
        &mut self,
        expr: &Expr,
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
            if param != &arg {
                return Err(self.type_mismatch(&args[index], &arg, param));
            }
        }

        Ok(())
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
            guard!(StmtKind::FunctionDecl[name, _one, _two, _three, _four] = &method.kind);
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
        params: &[Stmt],
        return_type_expr: &Option<Expr>,
        body: &[Stmt],
        _is_meta: bool,
    ) -> Analysis {
        let return_type = return_type_expr
            .as_ref()
            .and_then(|r| self.check_expr(r))
            .unwrap_or(NodeType::Void);

        let symbol = self.push_function_scope(name, return_type.clone());
        name.set_symbol(symbol);

        self.check_list(params);
        let analysis = self.check_list(body);
        self.pop_scope();

        if !analysis.guarantees_return && return_type != NodeType::Void && !self.is_builtin {
            let last_param = params.last().map(|p| p.span.clone());
            let span_params = Span::join_opt(name, &last_param);
            let span = Span::join_opt(&span_params, return_type_expr);
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
        kind: &Option<Expr>,
        value: &Option<Expr>,
    ) -> Analysis {
        let explicit_type = kind.as_ref().and_then(|k| match k.accept(self) {
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
                if explicit != implicit {
                    let diagnostic = self.type_mismatch(
                        &value.as_ref().unwrap().span().clone(),
                        &implicit,
                        &explicit,
                    );
                    self.report_error(diagnostic);
                }
            }
            (Some(explicit), None) => self.current_scope().define_var(name, &explicit),
            (None, Some(implicit)) => self.current_scope().define_var(name, &implicit),
            (None, None) => {
                let diag = Diagnostic::error(name, "Can't infer type");
                self.report_error(diag);
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
            if cond_type != NodeType::Bool {
                self.report_error(self.type_mismatch(condition, &cond_type, &NodeType::Bool));
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

    fn visit_return_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Analysis {
        let ret_type = expr
            .as_ref()
            .and_then(|e| self.check_expr(e))
            .unwrap_or(NodeType::Void);
        if let Some(expected_return) = &self.current_scope().function_return_type {
            if &ret_type != expected_return {
                if let Some(ret_expr) = expr.as_ref() {
                    let message = format!(
                        "Cannot return value of type {}, expect type {}",
                        ret_type, expected_return
                    );
                    self.report_error(Diagnostic::error(ret_expr, &message));
                } else {
                    let message = format!("Expect function to return type {}", expected_return);
                    self.report_error(Diagnostic::error(stmt, &message));
                }
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
        if target_type != value_type {
            self.report_error(self.type_mismatch(value, &value_type, &target_type));
        }
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

        if let Some(matching_entry) = entries.iter().find(|e| e.0 == lhs_type && e.1 == rhs_type) {
            expr.set_type(matching_entry.2.clone())
        } else {
            let message = format!("Cannot {} on {} and {}", op.lexeme(), lhs_type, rhs_type);
            Err(Diagnostic::error(&Span::join(lhs, rhs), &message))
        }
    }

    fn visit_unary_expr(&mut self, expr: &Expr, op: &Token, operand: &Expr) -> Self::ExprResult {
        let operand_type = operand.accept(self)?;

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

        if entries.contains(&operand_type) {
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
        args: &[Expr],
    ) -> Self::ExprResult {
        if let Some((found_symbol, node_type)) = self.resolve_var(function.span().lexeme()) {
            let (params, return_type) = match node_type {
                NodeType::Function(params, ret) => {
                    function.set_symbol(found_symbol);
                    Ok((params, ret))
                }
                NodeType::Metatype(symbol) => {
                    let meta_symbol = Symbol::meta_symbol(Some(&symbol));
                    let init_symbol = Symbol::init_symbol(Some(&meta_symbol));

                    function.set_symbol(init_symbol.clone());

                    let init_type = self.lib.resolve_symbol(&init_symbol).unwrap();
                    guard!(NodeType::Function[p, r] = init_type);
                    Ok((p, r))
                }
                _ => Err(Diagnostic::error(
                    function,
                    &format!("Cannot call type {}", node_type),
                )),
            }?;

            let params = params.clone();
            let return_type: NodeType = (**return_type).clone();

            self.check_function_arguments(expr, &params, args)?;

            expr.set_type(return_type)
        } else {
            Err(Diagnostic::error(function, "Undefined function"))
        }
    }

    fn visit_method_call_expr(
        &mut self,
        expr: &Expr,
        object: &Expr,
        method: &ResolvedToken,
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
            Some(NodeType::Function(params, ret_val)) => {
                method.set_symbol(method_symbol);

                let params = params.clone();
                let ret_val = (**ret_val).clone();

                self.check_function_arguments(expr, &params, args)?;
                expr.set_type(ret_val)
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

        // let type_symbol = match target_type.represented_type() {
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
        let first = elements.first().unwrap().accept(self)?;

        for element in elements.iter().skip(1) {
            let element_type = element.accept(self)?;
            if element_type != first {
                return Err(self.type_mismatch(element, &element_type, &first));
            }
        }

        let node_type = NodeType::Array(Box::new(first), ArraySize::Known(elements.len()));
        expr.set_type(node_type)
    }

    fn visit_subscript_expr(&mut self, expr: &Expr, target: &Expr, arg: &Expr) -> Self::ExprResult {
        let target_type = target.accept(self)?;
        let arg_type = arg.accept(self)?;

        match (target_type, arg_type) {
            (NodeType::Array(inside, _), NodeType::Int) => {
                // println!("setting subscript type to {}", inside)
                expr.set_type((*inside).clone())
            }
            (NodeType::Array(..), other) => Err(self.type_mismatch(arg, &other, &NodeType::Int)),
            _ => Err(Diagnostic::error(expr, "Can't subscript into non-array")),
        }
    }

    fn visit_explicit_type_expr(
        &mut self,
        expr: &Expr,
        category: &ExplicitTypeCategory,
    ) -> Self::ExprResult {
        let resolved_type = match category {
            ExplicitTypeCategory::Simple(token) => {
                if let Some(primitive) = NodeType::primitive(&token.token) {
                    token.set_symbol(Symbol::new(None, &token.token));
                    Ok(primitive)
                } else if let Some((_, node_type)) = self.resolve_var(token.token.lexeme()) {
                    if let NodeType::Metatype(type_symbol) = node_type {
                        token.set_symbol(type_symbol.clone());
                        Ok(NodeType::Type(type_symbol.clone()))
                    } else {
                        Err(Diagnostic::error(token, "Not a type"))
                    }
                } else {
                    Err(Diagnostic::error(token, "Undefined type"))
                }
            }
            ExplicitTypeCategory::Array(of, count_token) => {
                let of = of.accept(self)?;
                match count_token.lexeme().parse::<usize>() {
                    Ok(count) => {
                        let size = ArraySize::Known(count);
                        Ok(NodeType::Array(Box::new(of), size))
                    }
                    Err(..) => Err(Diagnostic::error(
                        count_token,
                        "Array count must be an integer",
                    )),
                }
            }
            ExplicitTypeCategory::Pointer(to) => {
                let to = to.accept(self)?;
                Ok(NodeType::pointer_to(to))
            }
        }?;

        expr.set_type(resolved_type)
    }
}

// NodeType

#[derive(Clone, Debug, PartialEq)]
pub enum NodeType {
    Void,
    Int,
    Bool,
    Byte,
    Type(Symbol),
    Pointer(Box<NodeType>),
    Array(Box<NodeType>, ArraySize),
    Function(Vec<NodeType>, Box<NodeType>),
    Metatype(Symbol),
    Ambiguous,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArraySize {
    Known(usize),
    Unknown,
}

impl std::fmt::Display for ArraySize {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ArraySize::Known(size) => write!(f, "{}", size),
            ArraySize::Unknown => write!(f, "<unknown>"),
        }
    }
}

impl NodeType {
    pub fn primitive(token: &Token) -> Option<Self> {
        match token.lexeme() {
            "void" => Some(NodeType::Void),
            "int" => Some(NodeType::Int),
            "bool" => Some(NodeType::Bool),
            "byte" => Some(NodeType::Byte),
            _ => None,
        }
    }

    pub fn pointer_to(pointee: NodeType) -> Self {
        NodeType::Pointer(Box::new(pointee))
    }

    pub fn represented_type(&self) -> &NodeType {
        match self {
            NodeType::Pointer(inner) => inner.represented_type(),
            _ => self,
        }
    }

    pub fn is_pointer_to(&self, node_type: NodeType) -> bool {
        if let NodeType::Pointer(pointee) = self {
            let reference: &NodeType = &pointee;
            if &node_type == reference {
                return true;
            }
        }
        false
    }
}

impl std::fmt::Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let kind = match self {
            NodeType::Void => String::from("void"),
            NodeType::Int => String::from("int"),
            NodeType::Bool => String::from("bool"),
            NodeType::Byte => String::from("byte"),
            NodeType::Function(params, ret) => {
                let mut string = String::from("def(");
                if let Some(first) = params.first() {
                    string += &first.to_string()
                }
                params
                    .iter()
                    .skip(1)
                    .for_each(|p| string += &format!(", {}", p));
                string += &format!("): {}", ret);
                string
            }
            NodeType::Pointer(ty) => format!("ptr<{}>", ty),
            NodeType::Array(ty, size) => format!("Array<{}, count={}>", ty, size),
            NodeType::Type(ty) => ty.id.clone(),
            NodeType::Metatype(ty) => ty.id.clone() + "_Meta",
            NodeType::Ambiguous => String::from("<ambiguous>"),
        };
        write!(f, "{}", kind)
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

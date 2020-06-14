use super::symbol_table::*;
use crate::diagnostic::*;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::*;
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

    fn define_var(&mut self, decl: &Stmt, name: &Token, var_type: &NodeType) {
        let new_symbol = Symbol::new((&self.id).as_ref(), name);

        self.symbols.insert(new_symbol.clone(), var_type.clone());

        decl.symbol.replace(Some(new_symbol));
        decl.stmt_type.replace(Some(var_type.clone()));
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
        given: NodeType,
        expected: NodeType,
    ) -> Diagnostic {
        let message = format!("Expected {}, got {}", expected, given);
        Diagnostic::error(span, &message)
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn current_symbol(&mut self) -> Option<&Symbol> {
        (&self.current_scope().id).as_ref()
    }

    fn push_type_scope(&mut self, name: &Token) -> Symbol {
        self.push_scope_named(name.lexeme())
    }

    fn push_function_scope(&mut self, name: &Token, ret_type: NodeType) -> Symbol {
        let symbol = Symbol::new(self.current_symbol(), name);
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

    fn resolve_type(&self, type_token: &Token) -> Result {
        if let Some(primitive) = NodeType::primitive(type_token) {
            Ok(primitive)
        } else if let Some((_, node_type)) = self.resolve_var(type_token.lexeme()) {
            if let NodeType::Metatype(type_symbol) = node_type {
                let compound_type = NodeType::Type(type_symbol.clone());
                Ok(compound_type)
            } else {
                Err(Diagnostic::error(type_token, "Not a type"))
            }
        } else {
            Err(Diagnostic::error(type_token, "Undefined type"))
        }
    }
}

impl StmtVisitor for TypeChecker {
    type StmtResult = Analysis;

    fn visit_type_decl(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        fields: &[Stmt],
        methods: &[Stmt],
        meta_methods: &[Stmt],
    ) -> Analysis {
        let symbol = self.push_type_scope(name);
        stmt.symbol.replace(Some(symbol));

        self.push_scope_meta();
        self.check_list(meta_methods);
        self.pop_scope();

        self.check_list(fields);
        self.check_list(methods);

        self.pop_scope();

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_function_decl(
        &mut self,
        stmt: &Stmt,
        name: &Token,
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
        stmt.symbol.replace(Some(symbol));

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
        stmt: &Stmt,
        name: &Token,
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
                self.current_scope().define_var(&stmt, name, &explicit);
                if explicit != implicit {
                    let diagnostic = self.type_mismatch(
                        &value.as_ref().unwrap().span().clone(),
                        implicit,
                        explicit,
                    );
                    self.report_error(diagnostic);
                }
            }
            (Some(explicit), None) => self.current_scope().define_var(&stmt, name, &explicit),
            (None, Some(implicit)) => self.current_scope().define_var(&stmt, name, &implicit),
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
                self.report_error(self.type_mismatch(condition, cond_type, NodeType::Bool));
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

    fn visit_print_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Analysis {
        if let Some(node_type) = expr.as_ref().and_then(|e| self.check_expr(e)) {
            match node_type {
                NodeType::Int | NodeType::Bool => {
                    stmt.stmt_type.replace(Some(node_type));
                }
                node_type if node_type.is_pointer_to(NodeType::Byte) => {
                    stmt.stmt_type.replace(Some(node_type));
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
        _expr: &Expr,
        target: &Expr,
        value: &Expr,
    ) -> Self::ExprResult {
        let target_type = target.accept(self)?;
        let value_type = value.accept(self)?;
        if target_type != value_type {
            self.report_error(self.type_mismatch(value, value_type, target_type));
        }
        Ok(NodeType::Void)
    }

    fn visit_binary_expr(
        &mut self,
        _expr: &Expr,
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
            Ok(matching_entry.2.clone())
        } else {
            let message = format!("Cannot {} on {} and {}", op.lexeme(), lhs_type, rhs_type);
            Err(Diagnostic::error(&Span::join(lhs, rhs), &message))
        }
    }

    fn visit_unary_expr(&mut self, _expr: &Expr, op: &Token, operand: &Expr) -> Self::ExprResult {
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
            Ok(operand_type.clone())
        } else {
            let message = format!("Cannot {} on {}", op.lexeme(), operand_type);
            Err(Diagnostic::error(operand, &message))
        }
    }

    fn visit_call_expr(&mut self, expr: &Expr, target: &Expr, args: &[Expr]) -> Self::ExprResult {
        let func_type = target.accept(self)?;

        let (params, return_type) = match func_type.clone() {
            NodeType::Function(p, r) => Ok((p, *r)),
            NodeType::Metatype(symbol) => {
                let meta_symbol = Symbol::meta_symbol(Some(&symbol));
                let init_symbol = Symbol::init_symbol(Some(&meta_symbol));

                let init_type = self.lib.resolve_symbol(&init_symbol).unwrap();
                guard!(NodeType::Function[p, r] = init_type.clone());
                Ok((p, *r))
            }
            _ => Err(Diagnostic::error(
                target,
                &format!("Cannot call type {}", func_type),
            )),
        }?;

        let arg_types: DiagnosticResult<Vec<NodeType>> =
            args.iter().map(|a| a.accept(self)).collect();
        let arg_types = arg_types?;

        if params.len() != arg_types.len() {
            return Err(Diagnostic::error(
                expr,
                &format!(
                    "Expected {} argument(s), got {}",
                    params.len(),
                    arg_types.len()
                ),
            ));
        }

        for ((index, param), arg) in params.into_iter().enumerate().zip(arg_types) {
            if param != arg {
                return Err(self.type_mismatch(&args[index], arg, param));
            }
        }

        Ok(return_type)
    }

    fn visit_field_expr(&mut self, expr: &Expr, target: &Expr, field: &Token) -> Self::ExprResult {
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

        let field_symbol = Symbol::new(Some(&type_symbol), field);

        if let Some(field_type) = self.lib.resolve_symbol(&field_symbol) {
            expr.symbol.replace(Some(field_symbol));
            Ok(field_type.clone())
        } else {
            Err(Diagnostic::error(
                &Span::join(target, field),
                &format!(
                    "Type '{}' does not has field '{}'",
                    type_symbol.id,
                    field.lexeme()
                ),
            ))
        }
    }

    fn visit_literal_expr(&mut self, _expr: &Expr, token: &Token) -> Self::ExprResult {
        match token.kind {
            TokenKind::Number => Ok(NodeType::Int),
            TokenKind::True => Ok(NodeType::Bool),
            TokenKind::False => Ok(NodeType::Bool),
            TokenKind::StringLiteral => Ok(NodeType::pointer_to(NodeType::Byte)),
            _ => panic!(),
        }
    }

    fn visit_variable_expr(&mut self, expr: &Expr, name: &Token) -> Self::ExprResult {
        if let Some((symbol, node_type)) = self.resolve_var(name.lexeme()) {
            expr.symbol.replace(Some(symbol));
            Ok(node_type.clone())
        } else {
            Err(Diagnostic::error(name, "Undefined variable"))
        }
    }

    fn visit_explicit_type_expr(
        &mut self,
        _expr: &Expr,
        name: &Token,
        modifier: &Option<Token>,
    ) -> Self::ExprResult {
        let main = self.resolve_type(name)?;
        if modifier.is_some() {
            Ok(NodeType::Pointer(Box::new(main)))
        } else {
            Ok(main)
        }
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
    Function(Vec<NodeType>, Box<NodeType>),
    Metatype(Symbol),
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
            NodeType::Type(ty) => ty.id.clone(),
            NodeType::Metatype(ty) => ty.id.clone() + "_Meta",
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

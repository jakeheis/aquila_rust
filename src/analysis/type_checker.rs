use super::symbol_table::*;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::parsing::*;
use crate::source::*;
use std::rc::Rc;

pub type Result = DiagnosticResult<NodeType>;

struct Scope {
    id: Option<Symbol>,
    symbols: SymbolTable,
    function_return_type: Option<NodeType>,
}

impl Scope {
    fn new(id: Symbol, return_type: Option<NodeType>) -> Self {
        Scope {
            id: Some(id),
            symbols: SymbolTable::new(),
            function_return_type: return_type,
        }
    }

    fn global(symbols: SymbolTable) -> Self {
        Scope {
            id: None,
            symbols,
            function_return_type: None,
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
    scopes: Vec<Scope>,
    errored: bool,
}

pub struct Analysis {
    guarantees_return: bool,
}

impl TypeChecker {
    pub fn check(program: &ParsedProgram, reporter: Rc<dyn Reporter>) -> (SymbolTable, bool) {
        let table = SymbolTableBuilder::build(program);
        let global_scope = Scope::global(table);

        let mut checker = TypeChecker {
            reporter,
            scopes: vec![global_scope],
            errored: false,
        };
        checker.check_list(&program.statements);

        (checker.scopes.remove(0).symbols, !checker.errored)
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
                self.report(diag);
                None
            }
        }
    }

    fn report(&mut self, diag: Diagnostic) {
        self.reporter.report(diag);
        self.errored = true;
    }

    fn type_mismatch<T: ContainsSpan>(
        &self,
        span: &T,
        given: NodeType,
        expected: NodeType,
    ) -> Diagnostic {
        let message = format!("Expected {:#?}, got {:#?}", expected, given);
        Diagnostic::error(span, &message)
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn global_scope(&mut self) -> &mut Scope {
        self.scopes.first_mut().unwrap()
    }

    fn push_scope(&mut self, name: &Token) -> Symbol {
        self.push_scope_named(name.lexeme())
    }

    fn push_function_scope(&mut self, name: &Token, ret_type: NodeType) -> Symbol {
        let symbol = Symbol::new((&self.current_scope().id).as_ref(), name);
        self.scopes.push(Scope::new(symbol.clone(), Some(ret_type)));
        symbol
    }

    fn push_scope_named(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new_str((&self.current_scope().id).as_ref(), name);
        let ret_type = self.current_scope().function_return_type.clone();
        self.scopes.push(Scope::new(symbol.clone(), ret_type));
        symbol
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_var(&self, name: &str) -> Option<(&Scope, &Symbol)> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.symbol_named(name) {
                return Some((scope, symbol));
            }
        }
        None
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
    ) -> Analysis {
        let symbol = self.push_scope(name);
        stmt.symbol.replace(Some(symbol));

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
        return_type_token: &Option<Token>,
        body: &[Stmt],
    ) -> Analysis {
        let return_type = return_type_token
            .as_ref()
            .map(|r| NodeType::from(r))
            .unwrap_or(NodeType::Void);

        let symbol = self.push_function_scope(name, return_type.clone());
        stmt.symbol.replace(Some(symbol));

        self.check_list(params);
        let analysis = self.check_list(body);
        self.pop_scope();

        if analysis.guarantees_return == false {
            let message = format!("Function may not return {}", return_type);
            let last_param = params.last().map(|p| p.span.clone());
            let span_params = Span::join_opt(name, &last_param);
            let span = Span::join_opt(&span_params, return_type_token);
            self.report(Diagnostic::error(&span, &message));
        }

        Analysis {
            guarantees_return: false,
        }
    }

    fn visit_variable_decl(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        kind: &Option<Token>,
        value: &Option<Expr>,
    ) -> Analysis {
        let explicit_type = kind.as_ref().map(|k| NodeType::from(k));
        if let Some(explicit_type) = explicit_type.as_ref() {
            self.current_scope().define_var(&stmt, name, explicit_type);

            if let NodeType::Type(compound_type) = explicit_type {
                match self
                    .global_scope()
                    .symbols
                    .symbol_and_type_named(&compound_type)
                {
                    Some((_, symbol_type)) if !symbol_type.is_metatype() => self
                        .reporter
                        .report(Diagnostic::error(kind.as_ref().unwrap(), "Not a type")),
                    None => self
                        .reporter
                        .report(Diagnostic::error(kind.as_ref().unwrap(), "Undefined type")),
                    _ => (),
                }
            }
        }

        if let Some(value) = value.as_ref() {
            if let Some(value_type) = self.check_expr(value) {
                if let Some(explicit_type) = explicit_type {
                    if explicit_type != value_type {
                        let diagnostic = self.type_mismatch(value, value_type, explicit_type);
                        self.report(diagnostic);
                    }
                } else {
                    self.current_scope().define_var(&stmt, name, &value_type);
                }
            }
        } else if explicit_type.is_none() {
            let diag = Diagnostic::error(name, "Can't infer type");
            self.report(diag);
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
                self.report(self.type_mismatch(condition, cond_type, NodeType::Bool));
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
                    self.report(Diagnostic::error(ret_expr, &message));
                } else {
                    let message = format!("Expect function to return type {}", expected_return);
                    self.report(Diagnostic::error(stmt, &message));
                }
            }
        }

        Analysis {
            guarantees_return: true,
        }
    }

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, expr: &Expr) -> Analysis {
        self.check_expr(expr);
        Analysis {
            guarantees_return: false,
        }
    }
}

impl ExprVisitor for TypeChecker {
    type ExprResult = Result;

    fn visit_assignment_expr(
        &mut self,
        _expr: &Expr,
        _target: &Expr,
        _value: &Expr,
    ) -> Self::ExprResult {
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

        let entries: &[NodeType] = match op.kind {
            TokenKind::Minus => &NEGATE_ENTRIES,
            TokenKind::Bang => &INVERT_ENTRIES,
            _ => panic!(),
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
        if let NodeType::Function(params, ret) = func_type.clone() {
            let arg_types: DiagnosticResult<Vec<NodeType>> =
                args.iter().map(|a| a.accept(self)).collect();
            let arg_types = arg_types?;
            if params.len() != arg_types.len() {
                return Err(Diagnostic::error(
                    target,
                    &format!("Cannot call type {}", func_type),
                ));
            }
            for ((index, param), arg) in params.into_iter().enumerate().zip(arg_types) {
                if param != arg {
                    return Err(self.type_mismatch(&args[index], arg, param));
                }
            }
            Ok(*ret.clone())
        } else {
            Err(Diagnostic::error(
                target,
                &format!("Cannot call type {}", func_type),
            ))
        }
    }

    fn visit_field_expr(&mut self, expr: &Expr, target: &Expr, field: &Token) -> Self::ExprResult {
        let target_type = target.accept(self)?;

        if let NodeType::Type(type_name) = target_type {
            let type_symbol = Symbol::new_str(None, &type_name);
            let field_symbol = Symbol::new(Some(&type_symbol), field);

            if let Some(field_type) = self.global_scope().symbols.get_type(&field_symbol) {
                expr.symbol.replace(Some(field_symbol));
                Ok(field_type.clone())
            } else {
                Err(Diagnostic::error(
                    &Span::join(target, field),
                    &format!(
                        "Type '{}' does not has field '{}'",
                        type_name,
                        field.lexeme()
                    ),
                ))
            }
        } else {
            Err(Diagnostic::error(
                target,
                &format!("Cannot access property of type {}", target_type),
            ))
        }
    }

    fn visit_literal_expr(&mut self, _expr: &Expr, token: &Token) -> Self::ExprResult {
        match token.kind {
            TokenKind::Number => Ok(NodeType::Int),
            TokenKind::True => Ok(NodeType::Bool),
            TokenKind::False => Ok(NodeType::Bool),
            _ => panic!(),
        }
    }

    fn visit_variable_expr(&mut self, expr: &Expr, name: &Token) -> Self::ExprResult {
        if let Some((scope, symbol)) = self.resolve_var(name.lexeme()) {
            expr.symbol.replace(Some(symbol.clone()));
            Ok(scope.symbols.get_type(symbol).unwrap().clone())
        } else {
            Err(Diagnostic::error(name, "Undefined variable"))
        }
    }
}

// NodeType

#[derive(Clone, Debug, PartialEq)]
pub enum NodeType {
    Void,
    Int,
    Bool,
    Type(String),
    Function(Vec<NodeType>, Box<NodeType>),
    Metatype(String),
}

impl NodeType {
    pub fn from(token: &Token) -> Self {
        match token.lexeme() {
            "void" => NodeType::Void,
            "int" => NodeType::Int,
            "bool" => NodeType::Bool,
            other => NodeType::Type(other.to_string()),
        }
    }

    pub fn from_opt(token: &Option<Token>) -> Self {
        token
            .as_ref()
            .map(|t| NodeType::from(t))
            .unwrap_or(NodeType::Void)
    }

    fn is_metatype(&self) -> bool {
        match self {
            NodeType::Metatype(_) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let kind = match self {
            NodeType::Void => String::from("void"),
            NodeType::Int => String::from("int"),
            NodeType::Bool => String::from("bool"),
            NodeType::Function(params, ret) => {
                let mut string = String::from("(");
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
            NodeType::Type(ty) => ty.clone(),
            NodeType::Metatype(ty) => ty.clone() + "_Meta",
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

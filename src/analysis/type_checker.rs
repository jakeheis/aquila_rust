use super::symbol_table::*;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::parsing::*;
use crate::program::*;
use crate::source::*;
use std::collections::HashMap;

pub type Result = DiagnosticResult<NodeType>;

struct Scope {
    id: Option<Symbol>,
    symbols: SymbolTable,
}

impl Scope {
    fn new(id: Symbol, symbols: SymbolTable) -> Self {
        Scope {
            id: Some(id),
            symbols,
        }
    }

    fn global(symbols: SymbolTable) -> Self {
        // let mut variables = HashMap::with_capacity(symbols.count());
        // for (symbol, _) in &symbols.type_map {
        //     variables.insert(symbol.id.clone(), symbol.clone());
        // }
        Scope {
            id: None,
            symbols,
        }
    }

    fn define_var(&mut self, name: &Token, var_type: &NodeType) {
        let new_symbol = Symbol::new((&self.id).as_ref(), name);
        self.symbols.insert(new_symbol.clone(), var_type.clone());
        // self.variables.insert(name.lexeme().to_string(), new_symbol);
    }
}

pub struct TypeChecker {
    scopes: Vec<Scope>,
    errored: bool,
}

impl TypeChecker {
    pub fn check(program: ParsedProgram) -> bool {
        let table = SymbolTableBuilder::build(&program);
        println!("{}", table);
        let global_scope = Scope::global(table);

        let mut checker = TypeChecker {
            scopes: vec![global_scope],
            errored: false,
        };
        checker.check_list(&program.statements);

        checker.errored
    }

    fn check_list(&mut self, stmt_list: &[Stmt]) {
        for stmt in stmt_list {
            stmt.accept(self);
        }
    }


    fn check_expr(&mut self, expr: &Expr) -> Option<NodeType> {
        match expr.accept(self) {
            Ok(t) => Some(t),
            Err(diag) => {
                DefaultReporter::new().report(diag);
                None
            },
        }
    }

    // fn require(&self, expr: &Expr, given: ExprType, expected: ExprType) -> Result {
    //     if given == expected {
    //         Ok(given)
    //     } else  {
    //         let message = format!("Expected {:#?}, got {:#?}", expected, given);
    //         Err(Diagnostic::error_expr(expr, &message))
    //     }
    // }

    fn type_mismatch<T: ContainsSpan>(&self, span: &T, given: NodeType, expected: NodeType) -> Diagnostic {
        let message = format!("Expected {:#?}, got {:#?}", expected, given);
        Diagnostic::error(span, &message)
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn global_scope(&mut self) -> &mut Scope {
        self.scopes.first_mut().unwrap()
    }

    fn push_scope(&mut self, name: &Token) {
        let symbol = Symbol::new((&self.current_scope().id).as_ref(), name);
        self.scopes.push(Scope::new(symbol, SymbolTable::new()));
    }

    fn push_scope_named(&mut self, name: &str) {
        let symbol = Symbol::new_str((&self.current_scope().id).as_ref(), name);
        self.scopes.push(Scope::new(symbol, SymbolTable::new()));
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_var(&self, name: &str) -> Option<(&Scope, &Symbol)> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.symbol_named(name) {
                return Some((scope, symbol))
            }
        }
        None
    }

    // fn resolve_symbol(&self, name: &Symbol) -> Option<NodeType> {
    //     for scope in self.scopes.iter().rev() {
    //         if let Some(symbol) = scope.symbols.get_type(name) {
    //             return Some(symbol.clone())
    //         }
    //     }
    //     None
    // }
}

impl StmtVisitor for TypeChecker {
    type StmtResult = ();

    fn visit_type_decl(&mut self, _stmt: &Stmt, name: &Token, fields: &[Stmt], methods: &[Stmt]) {
        self.push_scope(name);
        self.check_list(fields);
        self.check_list(methods);
        self.pop_scope();
    }

    fn visit_function_decl(
        &mut self,
        _stmt: &Stmt, 
        name: &Token,
        params: &[Stmt],
        return_type: &Option<Token>,
        body: &[Stmt],
    ) {
        self.push_scope(name);
        self.check_list(params);
        self.check_list(body);
        self.pop_scope();
    }

    fn visit_variable_decl(&mut self, _stmt: &Stmt, name: &Token, kind: &Option<Token>, value: &Option<Expr>) {
        let explicit_type = kind.as_ref().map(|k| NodeType::from(k));
        if let Some(explicit_type) = explicit_type.as_ref() {
            if let NodeType::Type(compound_type) = explicit_type {
                if let None = self.global_scope().symbols.symbol_named(&compound_type) {
                    DefaultReporter::new().report(Diagnostic::error(kind.as_ref().unwrap(), "Undefined type"))
                }
            }
            self.current_scope().define_var(name, explicit_type);
        }

        if let Some(value) = value.as_ref() {
            if let Some(value_type) = self.check_expr(value) {
                if let Some(explicit_type) = explicit_type {
                    if explicit_type != value_type {
                        let diagnostic = self.type_mismatch(value, value_type, explicit_type);
                        DefaultReporter::new().report(diagnostic);
                    }
                } else {
                    self.current_scope().define_var(name, &value_type);
                }
            }
        } else if explicit_type.is_none() {
            let diag = Diagnostic::error(name, "Couldn't infer type");
            DefaultReporter::new().report(diag);
        }
    }

    fn visit_if_stmt(&mut self, _stmt: &Stmt, condition: &Expr, body: &[Stmt], else_body: &[Stmt]) {
        if let Some(cond_type) = self.check_expr(condition) {
            if cond_type != NodeType::Bool {
                DefaultReporter::new().report(self.type_mismatch(condition, cond_type, NodeType::Bool));
            }
        }
       
        self.push_scope_named("if");
        self.check_list(body);
        self.pop_scope();

        self.push_scope_named("else");
        self.check_list(else_body);
        self.pop_scope();
    }

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, expr: &Expr) {
        self.check_expr(expr);
    }
}

impl ExprVisitor for TypeChecker {
    type ExprResult = Result;

    fn visit_assignment_expr(&mut self, _target: &Expr, _value: &Expr) -> Self::ExprResult {
        Ok(NodeType::Void)
    }

    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::ExprResult {
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

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Self::ExprResult {
        let operand_type = expr.accept(self)?;

        let entries: &[NodeType] = match op.kind {
            TokenKind::Minus => &NEGATE_ENTRIES,
            TokenKind::Bang => &INVERT_ENTRIES,
            _ => panic!(),
        };

        if entries.contains(&operand_type) {
            Ok(operand_type.clone())
        } else {
            let message = format!("Cannot {} on {}", op.lexeme(), operand_type);
            Err(Diagnostic::error(expr, &message))
        }
    }

    fn visit_call_expr(&mut self, target: &Expr, args: &[Expr]) -> Self::ExprResult {
        let func_type = target.accept(self)?;
        if let NodeType::Function(params, ret) = func_type.clone() {
            let arg_types: DiagnosticResult<Vec<NodeType>> = args.iter().map(|a| a.accept(self)).collect();
            let arg_types = arg_types?;
            if params.len() != arg_types.len() {
                return Err(Diagnostic::error(target, &format!("Cannot call type {}", func_type)));
            }
            for ((index, param), arg) in params.into_iter().enumerate().zip(arg_types) {
                if param != arg {
                    return Err(self.type_mismatch(&args[index], arg, param));
                }
            }
            Ok(*ret.clone())
        } else {
            Err(Diagnostic::error(target, &format!("Cannot call type {}", func_type)))
        }
    }

    fn visit_field_expr(&mut self, target: &Expr, field: &Token) -> Self::ExprResult {
        let target_type = target.accept(self)?;

        if let NodeType::Type(type_name) = target_type {
            let type_symbol = Symbol::new_str(None, &type_name);
            let field_symbol = Symbol::new(Some(&type_symbol), field);
            if let Some(field_type) = self.global_scope().symbols.get_type(&field_symbol) {
                Ok(field_type.clone())
            } else {
                Err(Diagnostic::error(&Span::join(target, field), &format!("Type '{}' does not has field '{}'", type_name, field.lexeme())))
            }
        } else {
            Err(Diagnostic::error(target, &format!("Cannot access property of type {}", target_type)))
        }
    }

    fn visit_literal_expr(&mut self, token: &Token) -> Self::ExprResult {
        match token.kind {
            TokenKind::Number => Ok(NodeType::Int),
            TokenKind::True => Ok(NodeType::Bool),
            TokenKind::False => Ok(NodeType::Bool),
            _ => panic!(),
        }
    }

    fn visit_variable_expr(&mut self, name: &Token, _var_type: &Option<Token>) -> Self::ExprResult {
        if let Some((scope, symbol)) = self.resolve_var(name.lexeme()) {
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
    Function(Vec<NodeType>, Box<NodeType>),
    Type(String),
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

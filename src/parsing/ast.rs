use super::Expr;
use crate::lexing::Token;
use crate::library::*;
use crate::source::*;
use std::cell::RefCell;

#[derive(Debug)]
pub struct TypeDecl {
    pub name: SymbolicToken,
    pub generics: Vec<Token>,
    pub fields: Vec<StructuralVariableDecl>,
    pub methods: Vec<FunctionDecl>,
    pub meta_methods: Vec<FunctionDecl>,
    pub is_public: bool,
}

impl TypeDecl {
    pub fn new(
        name: Token,
        generics: Vec<Token>,
        fields: Vec<StructuralVariableDecl>,
        methods: Vec<FunctionDecl>,
        meta_methods: Vec<FunctionDecl>,
        public: bool,
    ) -> Self {
        TypeDecl {
            name: SymbolicToken::new(name),
            generics,
            fields,
            methods,
            meta_methods,
            is_public: public,
        }
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: SymbolicToken,
    pub generics: Vec<Token>,
    pub parameters: Vec<StructuralVariableDecl>,
    pub return_type: Option<ExplicitType>,
    pub body: Vec<Stmt>,
    pub is_meta: bool,
    pub is_public: bool,
    pub is_builtin: bool,
    pub include_caller: bool,
}

impl FunctionDecl {
    pub fn new(
        name: Token,
        generics: Vec<Token>,
        parameters: Vec<StructuralVariableDecl>,
        return_type: Option<ExplicitType>,
        body: Vec<Stmt>,
        is_meta: bool,
        is_builtin: bool,
        is_public: bool,
        include_caller: bool,
    ) -> Self {
        FunctionDecl {
            name: SymbolicToken::new(name),
            generics,
            parameters,
            return_type,
            body,
            is_meta,
            is_public,
            is_builtin,
            include_caller,
        }
    }
}

#[derive(Debug)]
pub struct StructuralVariableDecl {
    pub name: Token,
    pub explicit_type: ExplicitType,
    pub span: Span,
    pub is_public: bool,
}

impl StructuralVariableDecl {
    pub fn new(name: Token, explicit_type: ExplicitType, public: bool) -> Self {
        let span = Span::join(&name, &explicit_type);
        StructuralVariableDecl {
            name: name,
            explicit_type,
            span: span.clone(),
            is_public: public,
        }
    }
}

impl ContainsSpan for StructuralVariableDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
pub struct TraitDecl {
    pub name: Token,
    pub requirements: Vec<FunctionDecl>,
}

impl TraitDecl {
    pub fn new(name: Token, requirements: Vec<FunctionDecl>) -> Self {
        TraitDecl { name, requirements }
    }
}

#[derive(Debug)]
pub struct ConformanceDecl {
    pub target: SymbolicToken,
    pub trait_name: Token,
    pub implementations: Vec<FunctionDecl>,
}

impl ConformanceDecl {
    pub fn new(target: Token, trait_name: Token, impls: Vec<FunctionDecl>) -> ConformanceDecl {
        ConformanceDecl {
            target: SymbolicToken::new(target),
            trait_name: trait_name,
            implementations: impls,
        }
    }
}

pub enum ASTNode {
    TypeDecl(TypeDecl),
    FunctionDecl(FunctionDecl),
    TraitDecl(TraitDecl),
    ConformanceDecl(ConformanceDecl),
    Stmt(Stmt),
}

#[derive(Debug)]
pub struct LocalVariableDecl {
    pub name: SymbolicToken,
    pub explicit_type: Option<ExplicitType>,
    pub initial_value: Option<Expr>,
    pub var_type: RefCell<Option<NodeType>>,
    pub span: Span,
}

impl LocalVariableDecl {
    pub fn get_type(&self) -> Option<NodeType> {
        self.var_type.borrow().as_ref().map(|s| s.clone())
    }

    pub fn set_type(&self, var_type: NodeType) {
        self.var_type.replace(Some(var_type));
    }
}

impl ContainsSpan for LocalVariableDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
pub enum StmtKind {
    LocalVariableDecl(LocalVariableDecl),
    Assignment(Box<Expr>, Box<Expr>),
    IfStmt(Expr, Vec<Stmt>, Vec<Stmt>),
    WhileStmt(Expr, Vec<Stmt>),
    ForStmt(SymbolicToken, Expr, Vec<Stmt>),
    ReturnStmt(Option<Expr>),
    ExpressionStmt(Expr),
    BreakStmt,
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Stmt { kind, span }
    }

    pub fn accept<V: StmtVisitor>(&self, visitor: &mut V) -> V::StmtResult {
        match &self.kind {
            StmtKind::LocalVariableDecl(decl) => visitor.visit_local_variable_decl(decl),
            StmtKind::Assignment(target, value) => visitor.visit_assignment_stmt(target, value),
            StmtKind::IfStmt(condition, body, else_body) => {
                visitor.visit_if_stmt(&condition, &body, &else_body)
            }
            StmtKind::WhileStmt(condition, body) => visitor.visit_while_stmt(condition, &body),
            StmtKind::ForStmt(variable, array, body) => {
                visitor.visit_for_stmt(variable, array, &body)
            }
            StmtKind::ReturnStmt(expr) => visitor.visit_return_stmt(&self, expr),
            StmtKind::ExpressionStmt(expr) => visitor.visit_expression_stmt(expr),
            StmtKind::BreakStmt => visitor.visit_break_stmt(),
        }
    }

    pub fn local_variable_decl(
        name: Token,
        explicit_type: Option<ExplicitType>,
        value: Option<Expr>,
    ) -> Self {
        let end_span: &Span = if let Some(value) = &value {
            value.span()
        } else if let Some(explicit_type) = &explicit_type {
            explicit_type.span()
        } else {
            name.span()
        };
        let span = Span::join(&name, end_span);
        let decl = LocalVariableDecl {
            name: SymbolicToken::new(name),
            explicit_type,
            initial_value: value,
            var_type: RefCell::new(None),
            span: span.clone(),
        };
        Stmt::new(StmtKind::LocalVariableDecl(decl), span)
    }

    pub fn if_stmt(
        if_span: Span,
        condition: Expr,
        body: Vec<Stmt>,
        else_body: Vec<Stmt>,
        end_brace_span: Span,
    ) -> Self {
        let span = Span::join(&if_span, &end_brace_span);
        Stmt::new(StmtKind::IfStmt(condition, body, else_body), span)
    }

    pub fn while_stmt(
        while_span: Span,
        condition: Expr,
        body: Vec<Stmt>,
        end_brace_span: &Span,
    ) -> Self {
        let span = Span::join(&while_span, end_brace_span);
        Stmt::new(StmtKind::WhileStmt(condition, body), span)
    }

    pub fn for_stmt(
        for_span: Span,
        new_var: Token,
        array: Expr,
        body: Vec<Stmt>,
        end_brace_span: &Span,
    ) -> Self {
        let span = Span::join(&for_span, end_brace_span);
        Stmt::new(
            StmtKind::ForStmt(SymbolicToken::new(new_var), array, body),
            span,
        )
    }

    pub fn return_stmt(return_keyword: Span, expr: Option<Expr>) -> Self {
        let span = Span::join_opt(&return_keyword, &expr);
        Stmt::new(StmtKind::ReturnStmt(expr), span)
    }

    pub fn assign(target: Box<Expr>, value: Box<Expr>) -> Self {
        let span = Span::join(target.as_ref(), value.as_ref());
        Stmt::new(StmtKind::Assignment(target, value), span)
    }

    pub fn expression(expr: Expr) -> Self {
        let span = expr.span.clone();
        Stmt::new(StmtKind::ExpressionStmt(expr), span)
    }
}

impl ContainsSpan for Stmt {
    fn span(&self) -> &Span {
        &self.span
    }
}

pub trait StmtVisitor {
    type StmtResult;

    fn visit_local_variable_decl(&mut self, decl: &LocalVariableDecl) -> Self::StmtResult;

    fn visit_assignment_stmt(&mut self, target: &Expr, value: &Expr) -> Self::StmtResult;

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_while_stmt(&mut self, condition: &Expr, body: &[Stmt]) -> Self::StmtResult;

    fn visit_for_stmt(
        &mut self,
        variable: &SymbolicToken,
        array: &Expr,
        body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_return_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult;

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Self::StmtResult;

    fn visit_break_stmt(&mut self) -> Self::StmtResult;
}

// Tokens

#[derive(Debug)]
pub struct SymbolicToken {
    pub token: Token,
    symbol: RefCell<Option<Symbol>>,
}

impl SymbolicToken {
    pub fn new(token: Token) -> Self {
        SymbolicToken {
            token,
            symbol: RefCell::new(None),
        }
    }

    pub fn set_symbol(&self, symbol: Symbol) {
        self.symbol.replace(Some(symbol));
    }

    pub fn get_symbol(&self) -> Option<Symbol> {
        self.symbol.borrow().as_ref().map(|s| s.clone())
    }
}

impl ContainsSpan for SymbolicToken {
    fn span(&self) -> &Span {
        &self.token.span
    }
}

#[derive(Debug)]
pub struct SpecializedToken {
    pub token: Token,
    symbol: RefCell<Option<Symbol>>,
    pub specialization: Vec<ExplicitType>,
    span: Span,
}

impl SpecializedToken {
    pub fn new(token: Token, specialization: Vec<ExplicitType>) -> Self {
        let span = if let Some(last) = specialization.last() {
            Span::join(&token.span, last)
        } else {
            token.span.clone()
        };
        SpecializedToken {
            token,
            symbol: RefCell::new(None),
            specialization,
            span,
        }
    }

    pub fn set_symbol(&self, symbol: Symbol) {
        self.symbol.replace(Some(symbol));
    }

    pub fn get_symbol(&self) -> Option<Symbol> {
        self.symbol.borrow().as_ref().map(|s| s.clone())
    }
}

impl ContainsSpan for SpecializedToken {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
pub enum ExplicitTypeKind {
    Simple(SpecializedToken),
    Array(Box<ExplicitType>, Token),
    Pointer(Box<ExplicitType>),
}

#[derive(Debug)]
pub struct ExplicitType {
    pub kind: ExplicitTypeKind,
    pub span: Span,
    pub cached_type: RefCell<Option<NodeType>>,
}

impl ExplicitType {
    pub fn new(start: Span, kind: ExplicitTypeKind, end: &Span) -> Self {
        let span = Span::join(&start, end);
        ExplicitType {
            kind,
            span,
            cached_type: RefCell::new(None),
        }
    }

    pub fn guarantee_resolved(&self) -> NodeType {
        self.cached_type.borrow().as_ref().unwrap().clone()
    }
}

impl ContainsSpan for ExplicitType {
    fn span(&self) -> &Span {
        &self.span
    }
}

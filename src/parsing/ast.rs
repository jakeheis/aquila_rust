use super::Expr;
use crate::lexing::Token;
use crate::library::*;
use crate::source::*;
use std::cell::RefCell;

#[derive(Debug)]
pub struct TypeDecl {
    pub name: ResolvedToken,
    pub generics: Vec<ResolvedToken>,
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
        public: bool
    ) -> Self {
        let generics: Vec<_> = generics
            .into_iter()
            .map(|g| ResolvedToken::new_non_specialized(g))
            .collect();
        TypeDecl {
            name: ResolvedToken::new_non_specialized(name),
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
    pub name: ResolvedToken,
    pub generics: Vec<ResolvedToken>,
    pub parameters: Vec<StructuralVariableDecl>,
    pub return_type: Option<ExplicitType>,
    pub body: Vec<Stmt>,
    pub is_meta: bool,
    pub is_public: bool,
    pub is_builtin: bool,
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
        is_public: bool
    ) -> Self {
        let generics: Vec<_> = generics
            .into_iter()
            .map(|g| ResolvedToken::new_non_specialized(g))
            .collect();
        FunctionDecl {
            name: ResolvedToken::new_non_specialized(name),
            generics,
            parameters,
            return_type,
            body,
            is_meta,
            is_public,
            is_builtin,
        }
    }
}

#[derive(Debug)]
pub struct StructuralVariableDecl {
    pub name: TypedToken,
    pub explicit_type: ExplicitType,
    pub span: Span,
    pub is_public: bool,
}

impl StructuralVariableDecl {
    pub fn new(name: Token, explicit_type: ExplicitType, public: bool) -> Self {
        let span = Span::join(&name, &explicit_type);
        StructuralVariableDecl {
            name: TypedToken::new(name),
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
    pub name: ResolvedToken,
    pub requirements: Vec<FunctionDecl>,
}

impl TraitDecl {
    pub fn new(name: Token, requirements: Vec<FunctionDecl>) -> Self {
        TraitDecl {
            name: ResolvedToken::new(name, Vec::new()),
            requirements,
        }
    }
}

#[derive(Debug)]
pub struct ConformanceDecl {
    pub target: ResolvedToken,
    pub trait_name: ResolvedToken,
    pub implementations: Vec<FunctionDecl>,
}

impl ConformanceDecl {
    pub fn new(target: Token, trait_name: Token, impls: Vec<FunctionDecl>) -> ConformanceDecl {
        ConformanceDecl {
            target: ResolvedToken::new_non_specialized(target),
            trait_name: ResolvedToken::new_non_specialized(trait_name),
            implementations: impls,
        }
    }
}

pub enum ASTNode {
    TypeDecl(TypeDecl),
    FunctionDecl(FunctionDecl),
    TraitDecl(TraitDecl),
    ConformanceDecl(ConformanceDecl),
    Stmt(Stmt)
}

#[derive(Debug)]
pub struct LocalVariableDecl {
    pub name: TypedToken,
    pub explicit_type: Option<ExplicitType>,
    pub initial_value: Option<Expr>,
    pub span: Span,
}

impl ContainsSpan for LocalVariableDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
pub enum StmtKind {
    LocalVariableDecl(LocalVariableDecl),
    IfStmt(Expr, Vec<Stmt>, Vec<Stmt>),
    WhileStmt(Expr, Vec<Stmt>),
    ForStmt(TypedToken, Expr, Vec<Stmt>),
    ReturnStmt(Option<Expr>),
    PrintStmt(Option<Expr>),
    ExpressionStmt(Expr),
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
            StmtKind::IfStmt(condition, body, else_body) => {
                visitor.visit_if_stmt(&condition, &body, &else_body)
            }
            StmtKind::WhileStmt(condition, body) => visitor.visit_while_stmt(condition, &body),
            StmtKind::ForStmt(variable, array, body) => {
                visitor.visit_for_stmt(variable, array, &body)
            }
            StmtKind::ReturnStmt(expr) => visitor.visit_return_stmt(&self, expr),
            StmtKind::PrintStmt(expr) => visitor.visit_print_stmt(expr),
            StmtKind::ExpressionStmt(expr) => visitor.visit_expression_stmt(expr),
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
            name: TypedToken::new(name),
            explicit_type,
            initial_value: value,
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
            StmtKind::ForStmt(TypedToken::new(new_var), array, body),
            span,
        )
    }

    pub fn return_stmt(return_keyword: Span, expr: Option<Expr>) -> Self {
        let span = Span::join_opt(&return_keyword, &expr);
        Stmt::new(StmtKind::ReturnStmt(expr), span)
    }

    pub fn print_stmt(print_keyword: Span, expr: Option<Expr>) -> Self {
        let span = Span::join_opt(&print_keyword, &expr);
        Stmt::new(StmtKind::PrintStmt(expr), span)
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

    fn visit_type_decl(&mut self, decl: &TypeDecl) -> Self::StmtResult;

    fn visit_function_decl(&mut self, decl: &FunctionDecl) -> Self::StmtResult;

    fn visit_local_variable_decl(&mut self, decl: &LocalVariableDecl) -> Self::StmtResult;

    fn visit_trait_decl(&mut self, decl: &TraitDecl) -> Self::StmtResult;

    fn visit_conformance_decl(&mut self, decl: &ConformanceDecl) -> Self::StmtResult;

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_while_stmt(&mut self, condition: &Expr, body: &[Stmt]) -> Self::StmtResult;

    fn visit_for_stmt(
        &mut self,
        variable: &TypedToken,
        array: &Expr,
        body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_return_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult;

    fn visit_print_stmt(&mut self, expr: &Option<Expr>) -> Self::StmtResult;

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Self::StmtResult;
}

// Tokens

#[derive(Debug)]
pub struct TypedToken {
    pub token: Token,
    symbol: RefCell<Option<Symbol>>,
    token_type: RefCell<Option<NodeType>>,
}

impl TypedToken {
    fn new(token: Token) -> Self {
        TypedToken {
            token,
            symbol: RefCell::new(None),
            token_type: RefCell::new(None),
        }
    }

    pub fn set_symbol(&self, symbol: Symbol) {
        self.symbol.replace(Some(symbol));
    }

    pub fn get_symbol(&self) -> Option<Symbol> {
        self.symbol.borrow().as_ref().map(|s| s.clone())
    }

    pub fn set_type(&self, token_type: NodeType) {
        self.token_type.replace(Some(token_type));
    }

    pub fn get_type(&self) -> Option<NodeType> {
        self.token_type.borrow().as_ref().map(|s| s.clone())
    }
}

impl ContainsSpan for TypedToken {
    fn span(&self) -> &Span {
        &self.token.span
    }
}

#[derive(Debug)]
pub struct ResolvedToken {
    pub token: Token,
    symbol: RefCell<Option<Symbol>>,
    pub specialization: Vec<ExplicitType>,
    span: Span,
}

impl ResolvedToken {
    pub fn new_non_specialized(token: Token) -> Self {
        let span = token.span.clone();
        ResolvedToken {
            token,
            symbol: RefCell::new(None),
            specialization: Vec::new(),
            span,
        }
    }

    pub fn new(token: Token, specialization: Vec<ExplicitType>) -> Self {
        let span = if let Some(last) = specialization.last() {
            Span::join(&token.span, last)
        } else {
            token.span.clone()
        };
        ResolvedToken {
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

impl ContainsSpan for ResolvedToken {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
pub enum ExplicitTypeKind {
    Simple(ResolvedToken),
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

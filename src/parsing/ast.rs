use crate::analysis::{NodeType, Symbol};
use crate::diagnostic::*;
use crate::lexing::*;
use crate::library::Lib;
use crate::source::*;
use std::cell::RefCell;

pub struct TypeDecl {
    pub name: TypedToken,
    pub generics: Vec<TypedToken>,
    pub fields: Vec<Stmt>,
    pub methods: Vec<Stmt>,
    pub meta_methods: Vec<Stmt>
}

pub enum StmtKind {
    TypeDecl(TypeDecl),
    FunctionDecl(
        TypedToken,
        Vec<TypedToken>,
        Vec<Stmt>,
        Option<ExplicitType>,
        Vec<Stmt>,
        bool,
    ),
    VariableDecl(TypedToken, Option<ExplicitType>, Option<Expr>),
    TraitDecl(TypedToken, Vec<Stmt>),
    IfStmt(Expr, Vec<Stmt>, Vec<Stmt>),
    WhileStmt(Expr, Vec<Stmt>),
    ForStmt(TypedToken, Expr, Vec<Stmt>),
    ReturnStmt(Option<Expr>),
    PrintStmt(Option<Expr>),
    ExpressionStmt(Expr),
    Builtin(Box<Stmt>),
}

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
            StmtKind::TypeDecl(decl) => {
                visitor.visit_type_decl(&decl)
            }
            StmtKind::FunctionDecl(name, generics, params, return_type, body, is_meta) => visitor
                .visit_function_decl(
                    &name,
                    &generics,
                    &params,
                    &return_type,
                    &body,
                    *is_meta,
                ),
            StmtKind::VariableDecl(name, kind, value) => {
                visitor.visit_variable_decl(&name, &kind, &value)
            }
            StmtKind::TraitDecl(name, requirements) => {
                visitor.visit_trait_decl(name, &requirements)
            }
            StmtKind::IfStmt(condition, body, else_body) => {
                visitor.visit_if_stmt(&condition, &body, &else_body)
            }
            StmtKind::WhileStmt(condition, body) => {
                visitor.visit_while_stmt(condition, &body)
            }
            StmtKind::ForStmt(variable, array, body) => {
                visitor.visit_for_stmt(variable, array, &body)
            }
            StmtKind::ReturnStmt(expr) => visitor.visit_return_stmt(&self, expr),
            StmtKind::PrintStmt(expr) => visitor.visit_print_stmt(expr),
            StmtKind::ExpressionStmt(expr) => visitor.visit_expression_stmt(expr),
            StmtKind::Builtin(stmt) => visitor.visit_builtin_stmt(&stmt),
        }
    }

    pub fn type_decl(
        type_span: Span,
        name: Token,
        generics: Vec<Token>,
        fields: Vec<Stmt>,
        methods: Vec<Stmt>,
        meta_methods: Vec<Stmt>,
        right_brace: &Token,
    ) -> Self {
        let span = Span::join(&type_span, right_brace);
        let generics: Vec<_> = generics.into_iter().map(|g| TypedToken::new(g)).collect();
        let decl = TypeDecl {
            name: TypedToken::new(name),
            generics,
            fields,
            methods,
            meta_methods
        };
        Stmt::new(
            StmtKind::TypeDecl(decl),
            span,
        )
    }

    pub fn function_decl(
        start_span: Span,
        name: Token,
        generics: Vec<Token>,
        params: Vec<Stmt>,
        return_type: Option<ExplicitType>,
        body: Vec<Stmt>,
        right_brace_span: &Span,
        is_meta: bool,
    ) -> Self {
        let span = Span::join(&start_span, right_brace_span);
        let generics: Vec<_> = generics.into_iter().map(|g| TypedToken::new(g)).collect();
        Stmt::new(
            StmtKind::FunctionDecl(
                TypedToken::new(name),
                generics,
                params,
                return_type,
                body,
                is_meta,
            ),
            span,
        )
    }

    pub fn variable_decl(
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
        Stmt::new(
            StmtKind::VariableDecl(TypedToken::new(name), explicit_type, value),
            span,
        )
    }

    pub fn trait_decl(trait_span: Span, name: Token, requirements: Vec<Stmt>, end: &Span) -> Self {
        let span = Span::join(&trait_span, end);
        Stmt::new(StmtKind::TraitDecl(TypedToken::new(name), requirements), span)
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

    pub fn builtin(stmt: Stmt) -> Self {
        let span = stmt.span().clone();
        Stmt::new(StmtKind::Builtin(Box::new(stmt)), span)
    }
}

pub trait StmtVisitor {
    type StmtResult;

    fn visit_type_decl(
        &mut self,
        decl: &TypeDecl,
    ) -> Self::StmtResult;

    fn visit_function_decl(
        &mut self,
        name: &TypedToken,
        generics: &[TypedToken],
        params: &[Stmt],
        return_type: &Option<ExplicitType>,
        body: &[Stmt],
        is_meta: bool,
    ) -> Self::StmtResult;

    fn visit_variable_decl(
        &mut self,
        name: &TypedToken,
        explicit_type: &Option<ExplicitType>,
        value: &Option<Expr>,
    ) -> Self::StmtResult;

    fn visit_trait_decl(
        &mut self,
        name: &TypedToken,
        requirements: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_while_stmt(
        &mut self,
        condition: &Expr,
        body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_for_stmt(
        &mut self,
        variable: &TypedToken,
        array: &Expr,
        body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_return_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult;

    fn visit_print_stmt(&mut self, expr: &Option<Expr>) -> Self::StmtResult;

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Self::StmtResult;

    fn visit_builtin_stmt(&mut self, inner: &Box<Stmt>) -> Self::StmtResult;
}

// Expr

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

pub struct ResolvedToken {
    pub token: Token,
    symbol: RefCell<Option<Symbol>>,
}

impl ResolvedToken {
    pub fn new(token: Token) -> Self {
        ResolvedToken {
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

impl ContainsSpan for ResolvedToken {
    fn span(&self) -> &Span {
        &self.token.span
    }
}

pub enum ExplicitTypeKind {
    Simple(ResolvedToken),
    Array(Box<ExplicitType>, Token),
    Pointer(Box<ExplicitType>),
}

pub struct ExplicitType {
    pub kind: ExplicitTypeKind,
    span: Span,
    cached_type: RefCell<Option<NodeType>>,
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

    pub fn resolve(&self, lib: &Lib, context: &[Symbol]) -> Option<NodeType> {
        let result = NodeType::deduce_from_lib(self, lib, context);
        self.cached_type.replace(result.clone());
        result
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

pub enum ExprKind {
    Assignment(Box<Expr>, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    FunctionCall(
        Option<Box<Expr>>,
        ResolvedToken,
        Vec<ExplicitType>,
        Vec<Expr>,
    ),
    Field(Box<Expr>, ResolvedToken),
    Literal(Token),
    Variable(ResolvedToken),
    Array(Vec<Expr>),
    Subscript(Box<Expr>, Box<Expr>),
    Cast(Box<ExplicitType>, Box<Expr>),
}

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub node_type: RefCell<Option<NodeType>>,
}

impl Expr {
    pub fn set_type(&self, node_type: NodeType) -> DiagnosticResult<NodeType> {
        self.node_type.replace(Some(node_type.clone()));
        Ok(node_type)
    }

    pub fn get_type(&self) -> Option<NodeType> {
        self.node_type.borrow().as_ref().map(|t| t.clone())
    }

    pub fn new(kind: ExprKind, span: Span) -> Self {
        Expr {
            kind,
            span,
            node_type: RefCell::new(None),
        }
    }

    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::ExprResult {
        match &self.kind {
            ExprKind::Assignment(target, value) => {
                visitor.visit_assignment_expr(&self, &target, &value)
            }
            ExprKind::Binary(lhs, op, rhs) => visitor.visit_binary_expr(&self, &lhs, &op, &rhs),
            ExprKind::Unary(op, expr) => visitor.visit_unary_expr(&self, &op, &expr),
            ExprKind::FunctionCall(target, function, generics, args) => {
                let target: Option<&Expr> = target.as_ref().map(|t| t.as_ref());
                visitor.visit_function_call_expr(&self, target, &function, &generics, &args)
            }
            ExprKind::Field(target, field) => visitor.visit_field_expr(&self, &target, &field),
            ExprKind::Literal(token) => visitor.visit_literal_expr(&self, &token),
            ExprKind::Variable(name) => visitor.visit_variable_expr(&self, &name),
            ExprKind::Array(elements) => visitor.visit_array_expr(&self, elements),
            ExprKind::Subscript(target, arg) => visitor.visit_subscript_expr(&self, target, arg),
            ExprKind::Cast(exp_type, value) => visitor.visit_cast_expr(&self, &exp_type, &value),
        }
    }

    pub fn assignment(target: Expr, value: Expr) -> Self {
        let span = Span::join(&target.span, &value.span);
        Expr::new(
            ExprKind::Assignment(Box::new(target), Box::new(value)),
            span,
        )
    }

    pub fn binary(lhs: Expr, operator: Token, rhs: Expr) -> Self {
        let span = Span::join(&lhs.span, &rhs.span);
        Expr::new(
            ExprKind::Binary(Box::new(lhs), operator, Box::new(rhs)),
            span,
        )
    }

    pub fn unary(op: Token, expr: Expr) -> Self {
        let span = Span::join(&op.span, &expr.span);
        Expr::new(ExprKind::Unary(op, Box::new(expr)), span)
    }

    pub fn function_call(
        target: Option<Box<Expr>>,
        function: ResolvedToken,
        generics: Vec<ExplicitType>,
        args: Vec<Expr>,
        right_paren: &Token,
    ) -> Self {
        let span = Span::join(&function, right_paren);
        Expr::new(
            ExprKind::FunctionCall(target, function, generics, args),
            span,
        )
    }

    pub fn field(target: Expr, name: &Token) -> Self {
        let span = Span::join(&target, name);
        Expr::new(
            ExprKind::Field(Box::new(target), ResolvedToken::new(name.clone())),
            span,
        )
    }

    pub fn literal(token: &Token) -> Self {
        Expr::new(ExprKind::Literal(token.clone()), token.span.clone())
    }

    pub fn variable(name: Token) -> Self {
        let span = name.span().clone();
        Expr::new(ExprKind::Variable(ResolvedToken::new(name)), span)
    }

    pub fn array(left_bracket: Span, elements: Vec<Expr>, right_bracket: &Token) -> Self {
        let span = Span::join(&left_bracket, right_bracket);
        Expr::new(ExprKind::Array(elements), span)
    }

    pub fn subscript(target: Expr, index: Expr, right_bracket: &Token) -> Self {
        let span = Span::join(&target, right_bracket);
        Expr::new(ExprKind::Subscript(Box::new(target), Box::new(index)), span)
    }

    pub fn cast(
        cast_span: Span,
        explicit_type: ExplicitType,
        value: Expr,
        right_paren: &Token,
    ) -> Self {
        let span = Span::join(&cast_span, right_paren);
        Expr::new(
            ExprKind::Cast(Box::new(explicit_type), Box::new(value)),
            span,
        )
    }

    pub fn lexeme(&self) -> &str {
        self.span.lexeme()
    }
}

pub trait ExprVisitor {
    type ExprResult;

    fn visit_assignment_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        value: &Expr,
    ) -> Self::ExprResult;
    fn visit_binary_expr(
        &mut self,
        expr: &Expr,
        lhs: &Expr,
        op: &Token,
        rhs: &Expr,
    ) -> Self::ExprResult;
    fn visit_unary_expr(&mut self, expr: &Expr, op: &Token, operand: &Expr) -> Self::ExprResult;
    fn visit_function_call_expr(
        &mut self,
        expr: &Expr,
        target: Option<&Expr>,
        function: &ResolvedToken,
        specializations: &[ExplicitType],
        args: &[Expr],
    ) -> Self::ExprResult;
    fn visit_field_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        field: &ResolvedToken,
    ) -> Self::ExprResult;
    fn visit_literal_expr(&mut self, expr: &Expr, token: &Token) -> Self::ExprResult;
    fn visit_variable_expr(&mut self, expr: &Expr, name: &ResolvedToken) -> Self::ExprResult;
    fn visit_array_expr(&mut self, expr: &Expr, elements: &[Expr]) -> Self::ExprResult;
    fn visit_subscript_expr(&mut self, expr: &Expr, target: &Expr, arg: &Expr) -> Self::ExprResult;
    fn visit_cast_expr(
        &mut self,
        expr: &Expr,
        explicit_type: &ExplicitType,
        value: &Expr,
    ) -> Self::ExprResult;
}

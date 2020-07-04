use super::{ExplicitType, ResolvedToken};
use crate::diagnostic::*;
use crate::lexing::Token;
use crate::library::{NodeType, GenericSpecialization};
use std::cell::RefCell;

#[derive(Debug)]
pub struct FunctionCall {
    pub target: Option<Box<Expr>>,
    pub name: ResolvedToken,
    pub specialization: RefCell<Option<GenericSpecialization>>,
    pub arguments: Vec<Expr>,
}

impl FunctionCall {
    pub fn get_specialization(&self) -> Option<GenericSpecialization> {
        self.specialization.borrow().clone()
    }

    pub fn set_specialization(&self, spec: GenericSpecialization) {
        self.specialization.borrow_mut().replace(spec);
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Assignment(Box<Expr>, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    FunctionCall(FunctionCall),
    Field(Box<Expr>, ResolvedToken),
    Literal(Token),
    Variable(ResolvedToken),
    Array(Vec<Expr>),
    Subscript(Box<Expr>, Box<Expr>),
    Cast(Box<ExplicitType>, Box<Expr>),
}

#[derive(Debug)]
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
            ExprKind::FunctionCall(call) => {
                visitor.visit_function_call_expr(&self, call)
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
        args: Vec<Expr>,
        right_paren: &Token,
    ) -> Self {
        let span = Span::join(function.span(), right_paren);
        let function_call = FunctionCall {
            target,
            name: function,
            specialization: RefCell::new(None),
            arguments: args,
        };
        Expr::new(ExprKind::FunctionCall(function_call), span)
    }

    pub fn field(target: Expr, name: Token, specialization: Vec<ExplicitType>) -> Self {
        let span = Span::join(&target, &name);
        Expr::new(
            ExprKind::Field(Box::new(target), ResolvedToken::new(name, specialization)),
            span,
        )
    }

    pub fn literal(token: &Token) -> Self {
        Expr::new(ExprKind::Literal(token.clone()), token.span.clone())
    }

    pub fn variable(name: Token, specialization: Vec<ExplicitType>) -> Self {
        let span = name.span().clone();
        Expr::new(
            ExprKind::Variable(ResolvedToken::new(name, specialization)),
            span,
        )
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

impl ContainsSpan for Expr {
    fn span(&self) -> &Span {
        &self.span
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
        call: &FunctionCall,
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

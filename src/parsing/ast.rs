use crate::lexing::*;
use crate::source::*;

pub struct Expr {
    kind: ExprKind,
    span: Span,
}

impl Expr {
    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::Result {
        match &self.kind {
            ExprKind::Literal(token) => visitor.visit_literal_expr(&token),
            ExprKind::Binary(lhs, op, rhs) => visitor.visit_binary_expr(&lhs, &op, &rhs),
        }
    }

    pub fn literal(token: &Token) -> Self {
        Expr {
            kind: ExprKind::Literal(token.clone()),
            span: token.span.clone(),
        }
    }

    pub fn binary(lhs: Expr, operator: Token, rhs: Expr) -> Self {
        let span = Span::span(&lhs.span, &rhs.span);
        Expr {
            kind: ExprKind::Binary(Box::new(lhs), operator, Box::new(rhs)),
            span,
        }
    }

    pub fn lexeme(&self) -> &str {
        self.span.lexeme()
    }
}

pub enum ExprKind {
    Literal(Token),
    Binary(Box<Expr>, Token, Box<Expr>),
}

pub trait ExprVisitor {
    type Result;

    fn visit_literal_expr(&mut self, token: &Token) -> Self::Result;
    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Result;
}

pub struct ExprPrinter {
    indent: i32,
}

impl ExprPrinter {
    pub fn new() -> ExprPrinter {
        ExprPrinter { indent: 0 }
    }

    pub fn write(&self, token: &Token) {
        let indent = (0..self.indent).map(|_| "|--").collect::<String>();
        println!("{}{}", indent, token)
    }
}

impl ExprPrinter {
    fn indent<T>(&mut self, block: T)
    where
        T: Fn(&mut ExprPrinter) -> (),
    {
        self.indent += 1;
        block(self);
        self.indent -= 1;
    }
}

impl ExprVisitor for ExprPrinter {
    type Result = ();

    fn visit_literal_expr(&mut self, token: &Token) {
        self.write(token);
    }

    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) {
        self.write(op);
        self.indent(|visitor| {
            lhs.accept(visitor);
            rhs.accept(visitor);
        })
    }
}

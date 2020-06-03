use crate::lexing::*;
use crate::source::*;

pub struct Stmt {
    pub kind: StmtKind,
}

impl Stmt {
    pub fn accept<V: StmtVisitor>(&self, visitor: &mut V) -> V::StmtResult {
        match &self.kind {
            StmtKind::Expression(expr) => visitor.visit_expression_stmt(expr),
        }
    }

    pub fn expression(expr: Expr) -> Self {
        Stmt {
            kind: StmtKind::Expression(expr),
        }
    }
}

pub enum StmtKind {
    Expression(Expr),
}

pub trait StmtVisitor {
    type StmtResult;

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Self::StmtResult;
}

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::ExprResult {
        match &self.kind {
            ExprKind::Binary(lhs, op, rhs) => visitor.visit_binary_expr(&lhs, &op, &rhs),
            ExprKind::Unary(op, expr) => visitor.visit_unary_expr(&op, &expr),
            ExprKind::Literal(token) => visitor.visit_literal_expr(&token),
        }
    }

    pub fn binary(lhs: Expr, operator: Token, rhs: Expr) -> Self {
        let span = Span::join(&lhs.span, &rhs.span);
        Expr {
            kind: ExprKind::Binary(Box::new(lhs), operator, Box::new(rhs)),
            span,
        }
    }

    pub fn unary(op: Token, expr: Expr) -> Self {
        let span = Span::join(&op.span, &expr.span);
        Expr {
            kind: ExprKind::Unary(op, Box::new(expr)),
            span,
        }
    }

    pub fn literal(token: &Token) -> Self {
        Expr {
            kind: ExprKind::Literal(token.clone()),
            span: token.span.clone(),
        }
    }

    pub fn lexeme(&self) -> &str {
        self.span.lexeme()
    }
}

pub enum ExprKind {
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Literal(Token),
}

pub trait ExprVisitor {
    type ExprResult;

    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::ExprResult;
    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Self::ExprResult;
    fn visit_literal_expr(&mut self, token: &Token) -> Self::ExprResult;
}

pub struct ASTPrinter {
    indent: i32,
}

impl ASTPrinter {
    pub fn new() -> ASTPrinter {
        ASTPrinter { indent: 0 }
    }

    pub fn write(&self, token: &Token) {
        let indent = (0..self.indent).map(|_| "|--").collect::<String>();
        println!("{}{}", indent, token)
    }

    fn indent<T>(&mut self, block: T)
    where
        T: Fn(&mut ASTPrinter) -> (),
    {
        self.indent += 1;
        block(self);
        self.indent -= 1;
    }
}

impl StmtVisitor for ASTPrinter {
    type StmtResult = ();

    fn visit_expression_stmt(&mut self, expr: &Expr) {
        expr.accept(self);
    }
}

impl ExprVisitor for ASTPrinter {
    type ExprResult = ();

    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) {
        self.write(op);
        self.indent(|visitor| {
            lhs.accept(visitor);
            rhs.accept(visitor);
        })
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) {
        self.write(op);
        self.indent(|visitor| {
            expr.accept(visitor);
        })
    }

    fn visit_literal_expr(&mut self, token: &Token) {
        self.write(token);
    }

}

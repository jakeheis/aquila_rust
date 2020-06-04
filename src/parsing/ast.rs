use crate::lexing::*;
use crate::source::*;

pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn accept<V: StmtVisitor>(&self, visitor: &mut V) -> V::StmtResult {
        match &self.kind {
            StmtKind::VariableDecl(name, value) => visitor.visit_variable_decl(&name, &value),
            StmtKind::Expression(expr) => visitor.visit_expression_stmt(expr),
        }
    }

    pub fn variable_decl(let_span: Span, name: Token, value: Expr) -> Stmt {
        let span = Span::join(&let_span, &value.span);
        Stmt {
            kind: StmtKind::VariableDecl(name, value),
            span,
        }
    }

    pub fn expression(expr: Expr) -> Self {
        let span = expr.span.clone();
        Stmt {
            kind: StmtKind::Expression(expr),
            span,
        }
    }
}

pub enum StmtKind {
    VariableDecl(Token, Expr),
    Expression(Expr),
}

pub trait StmtVisitor {
    type StmtResult;

    fn visit_variable_decl(&mut self, name: &Token, value: &Expr) -> Self::StmtResult;
    fn visit_expression_stmt(&mut self, expr: &Expr) -> Self::StmtResult;
}

// Expr

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::ExprResult {
        match &self.kind {
            ExprKind::Assignment(target, value) => visitor.visit_assignment_expr(&target, &value),
            ExprKind::Binary(lhs, op, rhs) => visitor.visit_binary_expr(&lhs, &op, &rhs),
            ExprKind::Unary(op, expr) => visitor.visit_unary_expr(&op, &expr),
            ExprKind::Literal(token) => visitor.visit_literal_expr(&token),
            ExprKind::Variable(token) => visitor.visit_variable_expr(&token),
        }
    }

    pub fn assignment(target: Expr, value: Expr) -> Self {
        let span = Span::join(&target.span, &value.span);
        Expr {
            kind: ExprKind::Assignment(Box::new(target), Box::new(value)),
            span,
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

    pub fn variable(token: &Token) -> Self {
        Expr {
            kind: ExprKind::Variable(token.clone()),
            span: token.span.clone(),
        }
    }

    pub fn lexeme(&self) -> &str {
        self.span.lexeme()
    }
}

pub enum ExprKind {
    Assignment(Box<Expr>, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Literal(Token),
    Variable(Token),
}

pub trait ExprVisitor {
    type ExprResult;

    fn visit_assignment_expr(&mut self, target: &Expr, value: &Expr) -> Self::ExprResult;
    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::ExprResult;
    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Self::ExprResult;
    fn visit_literal_expr(&mut self, token: &Token) -> Self::ExprResult;
    fn visit_variable_expr(&mut self, token: &Token) -> Self::ExprResult;
}

// ASTPrinter

pub struct ASTPrinter {
    indent: i32,
}

impl ASTPrinter {
    pub fn new() -> ASTPrinter {
        ASTPrinter { indent: 0 }
    }

    pub fn write_ln(&self, token: &str) {
        let indent = (0..self.indent).map(|_| "|  ").collect::<String>();
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

    fn visit_variable_decl(&mut self, name: &Token, value: &Expr) {
        self.write_ln(&format!("VariableDecl(name: {})", name.lexeme()));
        self.indent(|visitor| {
            value.accept(visitor);
        })
    }

    fn visit_expression_stmt(&mut self, expr: &Expr) {
        self.write_ln("ExpressionStmt");
        self.indent(|visitor| {
            expr.accept(visitor);
        })
    }
}

impl ExprVisitor for ASTPrinter {
    type ExprResult = ();

    fn visit_assignment_expr(&mut self, target: &Expr, value: &Expr) {
        self.write_ln("Assign");
        self.indent(|visitor| {
            target.accept(visitor);
            value.accept(visitor);
        })
    }

    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) {
        self.write_ln(&format!("Binary({})", op.lexeme()));
        self.indent(|visitor| {
            lhs.accept(visitor);
            rhs.accept(visitor);
        })
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) {
        self.write_ln(&format!("Unary({})", op.lexeme()));
        self.indent(|visitor| {
            expr.accept(visitor);
        })
    }

    fn visit_literal_expr(&mut self, token: &Token) {
        self.write_ln(&format!("Literal({})", token.lexeme()));
    }

    fn visit_variable_expr(&mut self, token: &Token) {
        self.write_ln(&format!("Variable(name: {})", token.lexeme()))
    }
}

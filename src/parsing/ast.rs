use crate::lexer::Token;

pub struct Expr {
    pub kind: ExprKind,
}

impl Expr {
    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::Result {
        match &self.kind {
            ExprKind::Literal(token) => visitor.visit_literal_expr(&token),
            ExprKind::Binary(lhs, op, rhs) => visitor.visit_binary_expr(&lhs, &op, &rhs),
        }
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

impl ExprVisitor for ExprPrinter {
    type Result = ();

    fn visit_literal_expr(&mut self, token: &Token) {
        self.write(token);
    }

    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) {
        self.write(op);
        self.indent += 1;
        lhs.accept(self);
        rhs.accept(self);
        self.indent -= 1;
    }
}

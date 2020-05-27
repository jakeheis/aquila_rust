use super::lexer::*;

struct Expr {
    kind: ExprKind
}

impl Expr {
    fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::Result {
        match &self.kind {
            ExprKind::Literal(token) => visitor.visit_literal_expr(&token),
            ExprKind::Binary(lhs, op, rhs) => visitor.visit_binary_expr(&lhs, &op, &rhs)
        }
    }
}

enum ExprKind {
    Literal(Token),
    Binary(Box<Expr>, Token, Box<Expr>)
}

trait ExprVisitor {
    type Result;

    fn visit_literal_expr(&mut self, token: &Token) -> Self::Result;
    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Result;
}

struct ExprPrinter {
    indent: i32
}

impl ExprPrinter {
    fn new() -> ExprPrinter { 
        ExprPrinter { indent: 0 }
    }

    fn write(&self, token: &Token) {
        let indent = (0..self.indent).map(|_| "  ").collect::<String>();
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

pub struct Parser {
    tokens: Vec<Token>
}

impl Parser {

    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens }
    }

    pub fn parse(&mut self) {
        let expr = self.parse_precedence(Precedence::TERM);
        let mut printer = ExprPrinter::new();
        expr.accept(&mut printer);
    }

    fn parse_precedence(&mut self, prec: Precedence) -> Expr {
        let mut lhs = self.number();

        while let Some(next) = self.peek() {
            if self.operator_precedence(next) >= prec {
                let operator = self.advance();
                let rhs = self.parse_precedence(prec.next());
                let kind = ExprKind::Binary(Box::new(lhs), operator, Box::new(rhs));
                lhs = Expr { kind }
            } else {
                break
            }
        }
        
        lhs
    }

    fn number(&mut self) -> Expr {
        let literal = self.consume(TokenKind::NUMBER);
        let kind = ExprKind::Literal(literal);
        Expr { kind }
    }

    fn operator_precedence(&self, kind: TokenKind) -> Precedence {
        match kind {
            TokenKind::PLUS | TokenKind::MINUS => Precedence::TERM,
            TokenKind::STAR | TokenKind::SLASH => Precedence::FACTOR,
            _ => panic!()
        }
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.tokens.first().map(|t| t.kind)
    }

    fn consume(&mut self, kind: TokenKind) -> Token {
        let first = self.advance();
        if first.kind == kind {
            first
        } else {
            panic!();
        }
    }

    fn advance(&mut self) -> Token {
        self.tokens.remove(0)
    }

}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    TERM,
    FACTOR,
    NUMBER,
    MAX
}

impl Precedence {
    fn next(&self) -> Precedence {
        match self {
            Precedence::TERM => Precedence::FACTOR,
            Precedence::FACTOR => Precedence::NUMBER,
            Precedence::NUMBER => Precedence::MAX,
            Precedence::MAX => panic!()
        }
    }
}
use super::lexer::*;

struct Expr {
    kind: ExprKind,
}

impl Expr {
    fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::Result {
        match &self.kind {
            ExprKind::Literal(token) => visitor.visit_literal_expr(&token),
            ExprKind::Binary(lhs, op, rhs) => visitor.visit_binary_expr(&lhs, &op, &rhs),
        }
    }
}

enum ExprKind {
    Literal(Token),
    Binary(Box<Expr>, Token, Box<Expr>),
}

trait ExprVisitor {
    type Result;

    fn visit_literal_expr(&mut self, token: &Token) -> Self::Result;
    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Result;
}

struct ExprPrinter {
    indent: i32,
}

impl ExprPrinter {
    fn new() -> ExprPrinter {
        ExprPrinter { indent: 0 }
    }

    fn write(&self, token: &Token) {
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

struct ParseTableEntry {
    kind: TokenKind,
    prefix: Option<fn(&mut Parser) -> Expr>,
    infix: Option<(fn(&mut Parser, lhs: Expr) -> Expr, Precedence)>,
}

const PARSE_TABLE: [ParseTableEntry; 5] = [
    ParseTableEntry {
        kind: TokenKind::NUMBER,
        prefix: Some(Parser::number),
        infix: None,
    },
    ParseTableEntry {
        kind: TokenKind::PLUS,
        prefix: None,
        infix: Some((Parser::binary, Precedence::TERM)),
    },
    ParseTableEntry {
        kind: TokenKind::MINUS,
        prefix: None,
        infix: Some((Parser::binary, Precedence::TERM)),
    },
    ParseTableEntry {
        kind: TokenKind::STAR,
        prefix: None,
        infix: Some((Parser::binary, Precedence::FACTOR)),
    },
    ParseTableEntry {
        kind: TokenKind::SLASH,
        prefix: None,
        infix: Some((Parser::binary, Precedence::FACTOR)),
    },
];

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, index: 0 }
    }

    pub fn parse(&mut self) {
        let expr = self.parse_precedence(Precedence::TERM);
        let mut printer = ExprPrinter::new();
        expr.accept(&mut printer);
    }

    fn parse_precedence(&mut self, prec: Precedence) -> Expr {
        let prefix_entry = PARSE_TABLE
            .iter()
            .find(|t| t.kind == self.advance().kind)
            .unwrap();

        let mut lhs = prefix_entry.prefix.unwrap()(self);

        while let Some(next) = self.peek() {
            let infix_entry = PARSE_TABLE
                .iter()
                .find(|t| t.kind == next)
                .unwrap()
                .infix
                .as_ref()
                .unwrap();
            if infix_entry.1 >= prec {
                lhs = infix_entry.0(self, lhs);
            } else {
                break;
            }
        }

        lhs
    }

    fn binary(&mut self, lhs: Expr) -> Expr {
        let operator = self.advance();
        let rhs = self.parse_precedence(Precedence::TERM.next());
        let kind = ExprKind::Binary(Box::new(lhs), operator.clone(), Box::new(rhs));
        Expr { kind }
    }

    fn number(&mut self) -> Expr {
        let kind = ExprKind::Literal(self.previous().clone());
        Expr { kind }
    }

    fn peek(&mut self) -> Option<TokenKind> {
        if self.index < self.tokens.len() {
            Some(self.tokens[self.index].kind)
        } else {
            None
        }
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
        self.index += 1;
        return self.previous();
    }

    fn previous(&self) -> Token {
        self.tokens[self.index - 1].clone()
    }

    fn current(&self) -> Token {
        self.tokens[self.index].clone()
    }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    TERM,
    FACTOR,
    NUMBER,
    MAX,
}

impl Precedence {
    fn next(&self) -> Precedence {
        match self {
            Precedence::TERM => Precedence::FACTOR,
            Precedence::FACTOR => Precedence::NUMBER,
            Precedence::NUMBER => Precedence::MAX,
            Precedence::MAX => panic!(),
        }
    }
}

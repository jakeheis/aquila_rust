use super::ast::*;
use crate::lexer::{Token, TokenKind};

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
        let prefix = PARSE_TABLE.prefix(self.advance().kind).unwrap();

        let mut lhs = prefix(self);

        while let Some(next) = self.peek() {
            let infix_entry = PARSE_TABLE.infix(next).unwrap();
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

// ParseTable

type PrefixFn = fn(&mut Parser) -> Expr;
type InfixFn = fn(&mut Parser, lhs: Expr) -> Expr;

struct ParseTable {
    entries: [ParseTableEntry; 5],
}

impl ParseTable {    
    fn entry(&self, kind: TokenKind) -> Option<&ParseTableEntry> {
        self.entries.iter().find(|t| t.kind == kind)
    }

    fn prefix(&self, kind: TokenKind) -> Option<&PrefixFn> {
        self.entry(kind).and_then(|e| e.prefix.as_ref())
    }

    fn infix(&self, kind: TokenKind) -> Option<&(InfixFn, Precedence)> {
        self.entry(kind).and_then(|e| e.infix.as_ref())
    }
}

struct ParseTableEntry {
    kind: TokenKind,
    prefix: Option<PrefixFn>,
    infix: Option<(InfixFn, Precedence)>,
}

const PARSE_TABLE: ParseTable = ParseTable {
    entries: [
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
    ],
};

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

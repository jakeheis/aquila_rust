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
        let expr = self.parse_precedence(Precedence::Logic);
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
        let next_prec = PARSE_TABLE.precedence_for(operator.kind).next();
        let rhs = self.parse_precedence(next_prec);
        let kind = ExprKind::Binary(Box::new(lhs), operator.clone(), Box::new(rhs));
        Expr { kind }
    }

    fn literal(&mut self) -> Expr {
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
    entries: [ParseTableEntry; 14],
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

    fn precedence_for(&self, kind: TokenKind) -> Precedence {
        self.infix(kind).unwrap().1
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
            kind: TokenKind::Number,
            prefix: Some(Parser::literal),
            infix: None,
        },
        ParseTableEntry {
            kind: TokenKind::Plus,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Term)),
        },
        ParseTableEntry {
            kind: TokenKind::Minus,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Term)),
        },
        ParseTableEntry {
            kind: TokenKind::Star,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Factor)),
        },
        ParseTableEntry {
            kind: TokenKind::Slash,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Factor)),
        },
        ParseTableEntry {
            kind: TokenKind::AmpersandAmpersand,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Logic)),
        },
        ParseTableEntry {
            kind: TokenKind::BarBar,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Logic)),
        },
        ParseTableEntry {
            kind: TokenKind::EqualEqual,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Equality)),
        },
        ParseTableEntry {
            kind: TokenKind::Greater,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Comparison)),
        },
        ParseTableEntry {
            kind: TokenKind::GreaterEqual,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Comparison)),
        },
        ParseTableEntry {
            kind: TokenKind::Less,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Comparison)),
        },
        ParseTableEntry {
            kind: TokenKind::LessEqual,
            prefix: None,
            infix: Some((Parser::binary, Precedence::Comparison)),
        },
        ParseTableEntry {
            kind: TokenKind::True,
            prefix: Some(Parser::literal),
            infix: None,
        },
        ParseTableEntry {
            kind: TokenKind::False,
            prefix: Some(Parser::literal),
            infix: None,
        },
    ],
};

#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    Logic,
    Equality,
    Comparison,
    Term,
    Factor,
    Atom,
}

impl Precedence {
    fn next(&self) -> Precedence {
        match self {
            Precedence::Logic => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Atom,
            Precedence::Atom => panic!(),
        }
    }
}

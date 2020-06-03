use super::ast::*;
use crate::lexing::*;
use crate::diagnostic::*;
use std::rc::Rc;

type Result<T> = std::result::Result<T, Diagnostic>;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    reporter: Rc<dyn Reporter>,
}

impl Parser {

    pub fn new(tokens: Vec<Token>, reporter: Rc<dyn Reporter>) -> Self {
        Parser { tokens, index: 0, reporter }
    }

    pub fn parse(&mut self) {
        let statements = self.program();

        for stmt in statements {
            let mut printer = ASTPrinter::new();
            stmt.accept(&mut printer);
        }
    }

    fn program(&mut self) -> Vec<Stmt> {
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            match self.statement() {
                Ok(stmt) => stmts.push(stmt),
                Err(diagnostic) => self.reporter.report(diagnostic)
            }
        }
        stmts
    }

    fn statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenKind::Semicolon, "Expect semicolon after statement")?;

        Ok(Stmt::expression(expr))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.parse_precedence(Precedence::Logic)
    }

    fn parse_precedence(&mut self, prec: Precedence) -> Result<Expr> {
        let prefix = PARSE_TABLE.prefix(self.advance().kind).ok_or_else(|| {
            Diagnostic::error_token(self.previous(), "Expected atom")
        })?;

        let mut lhs = prefix(self);

        while !self.is_at_end() && self.peek() != TokenKind::Semicolon {
            let next = self.peek();
            let infix_entry = PARSE_TABLE.infix(next).ok_or_else(|| {
                Diagnostic::error_token(self.current(), "Expected expression")
             })?;
             
            if infix_entry.1 >= prec {
                self.advance();
                lhs = infix_entry.0(self, lhs)?;
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn binary(&mut self, lhs: Expr) -> Result<Expr> {
        let operator = self.previous().clone();
        let next_prec = PARSE_TABLE.precedence_for(operator.kind).next();
        let rhs = self.parse_precedence(next_prec)?;
        Ok(Expr::binary(lhs, operator, rhs))
    }

    fn literal(&mut self) -> Expr {
        Expr::literal(self.previous())
    }

    fn peek(&self) -> TokenKind {
        self.tokens[self.index].kind
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<&Token> {
        if self.is_at_end() {
            return Err(Diagnostic::error_token(self.previous(), message));
        }

        let consumed = self.advance();
        if consumed.kind == kind {
            Ok(consumed)
        } else {
            Err(Diagnostic::error_token(consumed, message))
        }
    }

    fn advance(&mut self) -> &Token {
        self.index += 1;
        return self.previous();
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.index - 1]
    }

    fn current(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn is_at_end(&self) -> bool {
        self.peek() == TokenKind::EOF
    }
}

// ParseTable

type PrefixFn = fn(&mut Parser) -> Expr;
type InfixFn = fn(&mut Parser, lhs: Expr) -> Result<Expr>;

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

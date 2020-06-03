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
        let prefix = self.advance().kind.prefix().ok_or_else(|| {
            Diagnostic::error_token(self.previous(), "Expected atom")
        })?;

        let mut lhs = prefix(self)?;

        while !self.is_at_end() && self.peek() != TokenKind::Semicolon {
            let next = self.peek();
            let infix_entry = next.infix().ok_or_else(|| {
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
        let next_prec = Precedence::for_kind(operator.kind).next();
        let rhs = self.parse_precedence(next_prec)?;
        Ok(Expr::binary(lhs, operator, rhs))
    }

    fn unary(&mut self) -> Result<Expr> {
        let operator = self.previous().clone();
        let expr = self.parse_precedence(Precedence::Unary.next())?;
        Ok(Expr::unary(operator, expr))
    }

    fn literal(&mut self) -> Result<Expr> {
        Ok(Expr::literal(self.previous()))
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

type PrefixFn = fn(&mut Parser) -> Result<Expr>;
type InfixFn = fn(&mut Parser, lhs: Expr) -> Result<Expr>;

impl TokenKind {
    fn prefix(&self) -> Option<PrefixFn> {
        match self {
            TokenKind::Minus | TokenKind::Bang => Some(Parser::unary),
            TokenKind::True | TokenKind::False | TokenKind::Number => Some(Parser::literal),
            _ => None
        }
    }

    fn infix(&self) -> Option<(InfixFn, Precedence)> {
        match self {
            TokenKind::Plus | TokenKind::Minus => Some((Parser::binary, Precedence::Term)),
            TokenKind::Star | TokenKind::Slash => Some((Parser::binary, Precedence::Factor)),
            TokenKind::AmpersandAmpersand | TokenKind::BarBar => Some((Parser::binary, Precedence::Logic)),
            TokenKind::EqualEqual | TokenKind::BangEqual => Some((Parser::binary, Precedence::Equality)),
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual => Some((Parser::binary, Precedence::Comparison)),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    Logic,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Atom,
}

impl Precedence {
    fn for_kind(kind: TokenKind) -> Precedence {
        kind.infix().unwrap().1
    }

    fn next(&self) -> Precedence {
        match self {
            Precedence::Logic => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Atom,
            Precedence::Atom => panic!(),
        }
    }
}

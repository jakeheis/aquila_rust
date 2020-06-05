use super::ast::*;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::source::*;
use std::rc::Rc;

type Result<T> = std::result::Result<T, Diagnostic>;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    reporter: Rc<dyn Reporter>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, reporter: Rc<dyn Reporter>) -> Self {
        Parser {
            tokens,
            index: 0,
            reporter,
        }
    }

    pub fn parse(&mut self) {
        let statements = self.stmt_list(None);

        for stmt in statements {
            let mut printer = ASTPrinter::new();
            stmt.accept(&mut printer);
        }
    }

    fn stmt_list(&mut self, end: Option<TokenKind>) -> Vec<Stmt> {
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.is_at_end() && Some(self.peek()) != end {
            match self.statement() {
                Ok(stmt) => stmts.push(stmt),
                Err(diagnostic) => {
                    self.reporter.report(diagnostic);
                    self.synchronize();
                }
            }
        }
        stmts
    }

    fn block(&mut self) -> Vec<Stmt> {
        self.stmt_list(Some(TokenKind::RightBrace))
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.matches(TokenKind::Let) {
            let decl = self.variable_decl()?;
            self.consume(
                TokenKind::Semicolon,
                "Expected semicolon after variable declaration",
            )?;
            Ok(decl)
        } else if self.matches(TokenKind::If) {
            self.if_stmt()
        } else {
            let stmt = Stmt::expression(self.parse_precedence(Precedence::Assignment)?);
            self.consume(TokenKind::Semicolon, "Expected semicolon after expression")?;
            Ok(stmt)
        }
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() && self.previous().kind != TokenKind::Semicolon {
            let done = match self.peek() {
                TokenKind::Let | TokenKind::If => true,
                TokenKind::Semicolon => {
                    self.advance();
                    true
                }
                _ => {
                    self.advance();
                    false
                }
            };
            if done {
                break;
            }
        }
    }

    fn variable_decl(&mut self) -> Result<Stmt> {
        let let_span = self.previous().span.clone();
        self.advance();
        let (name, kind) = self.parse_var(true, false)?;
        self.consume(TokenKind::Equal, "Expect '=' after variable declaration")?;
        let value = self.expression()?;
        Ok(Stmt::variable_decl(let_span, name, kind, value))
    }

    fn if_stmt(&mut self) -> Result<Stmt> {
        let if_span = self.previous().span.clone();

        let condition = self.expression()?;

        self.consume(TokenKind::LeftBrace, "Expect '{' after condition")?;
        let body = self.block();

        let mut end_brace_span = self
            .consume(TokenKind::RightBrace, "Expect '}' after if body")?
            .span
            .clone();

        let else_body = if self.matches(TokenKind::Else) {
            self.consume(TokenKind::LeftBrace, "Expect '{' after else")?;
            let block = self.block();
            end_brace_span = self
                .consume(TokenKind::RightBrace, "Expect '}' after else body")?
                .span
                .clone();
            block
        } else {
            Vec::new()
        };

        Ok(Stmt::if_stmt(
            if_span,
            condition,
            body,
            else_body,
            end_brace_span,
        ))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.parse_precedence(Precedence::Logic)
    }

    fn parse_precedence(&mut self, prec: Precedence) -> Result<Expr> {
        if self.is_at_end() {
            return Err(Diagnostic::error_token(
                self.previous(),
                "Expected expression",
            ));
        }

        let prefix = self
            .advance()
            .kind
            .prefix()
            .ok_or_else(|| Diagnostic::error_token(self.previous(), "Expected expression"))?;

        let can_assign = prec <= Precedence::Assignment;

        let mut lhs = prefix(self, can_assign)?;

        while !self.is_at_end() {
            if let Some(infix_entry) = self.peek().infix_entry() {
                if infix_entry.1 >= prec {
                    self.advance();
                    lhs = infix_entry.0(self, lhs, can_assign)?;
                    continue;
                }
            }

            break;
        }

        if can_assign && self.matches(TokenKind::Equal) {
            return Err(Diagnostic::error_expr(&lhs, "Invalid assignment target"));
        }

        Ok(lhs)
    }

    fn assignment(&mut self, lhs: Expr) -> Result<Expr> {
        let value = self.parse_precedence(Precedence::Equality.next())?;
        Ok(Expr::assignment(lhs, value))
    }

    fn binary(&mut self, lhs: Expr, _can_assign: bool) -> Result<Expr> {
        let operator = self.previous().clone();
        let next_prec = Precedence::for_kind(operator.kind).next();
        let rhs = self.parse_precedence(next_prec)?;
        Ok(Expr::binary(lhs, operator, rhs))
    }

    fn unary(&mut self, _can_assign: bool) -> Result<Expr> {
        let operator = self.previous().clone();
        let expr = self.parse_precedence(Precedence::Unary.next())?;
        Ok(Expr::unary(operator, expr))
    }

    fn literal(&mut self, _can_assign: bool) -> Result<Expr> {
        Ok(Expr::literal(self.previous()))
    }

    fn variable(&mut self, can_assign: bool) -> Result<Expr> {
        let (name, kind) = self.parse_var(false, false)?;
        let variable = Expr::variable(name, kind);
        if can_assign && self.matches(TokenKind::Equal) {
            self.assignment(variable)
        } else {
            Ok(variable)
        }
    }

    fn parse_var(
        &mut self,
        allow_type: bool,
        require_type: bool,
    ) -> Result<(Token, Option<Token>)> {
        let name = self.previous().clone();
        let mut var_type: Option<Token> = None;

        if self.matches(TokenKind::Colon) {
            if allow_type {
                var_type = Some(
                    self.consume(TokenKind::Identifier, "Expect variable type")?
                        .clone(),
                );
            } else {
                return Err(Diagnostic::error_token(
                    self.previous(),
                    "Variable type not allowed here",
                ));
            }
        } else {
            if require_type {
                return Err(Diagnostic::error_token(
                    self.previous(),
                    "Variable type required here",
                ));
            }
        }

        Ok((name, var_type))
    }

    fn peek(&self) -> TokenKind {
        self.current().kind
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

    fn matches(&mut self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() == kind {
            self.advance();
            true
        } else {
            false
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

type PrefixFn = fn(&mut Parser, can_assign: bool) -> Result<Expr>;
type InfixFn = fn(&mut Parser, lhs: Expr, can_assign: bool) -> Result<Expr>;

impl TokenKind {
    fn prefix(&self) -> Option<PrefixFn> {
        match self {
            TokenKind::Minus | TokenKind::Bang => Some(Parser::unary),
            TokenKind::True | TokenKind::False | TokenKind::Number => Some(Parser::literal),
            TokenKind::Identifier => Some(Parser::variable),
            _ => None,
        }
    }

    fn infix_entry(&self) -> Option<(InfixFn, Precedence)> {
        match self {
            TokenKind::Plus | TokenKind::Minus => Some((Parser::binary, Precedence::Term)),
            TokenKind::Star | TokenKind::Slash => Some((Parser::binary, Precedence::Factor)),
            TokenKind::AmpersandAmpersand | TokenKind::BarBar => {
                Some((Parser::binary, Precedence::Logic))
            }
            TokenKind::EqualEqual | TokenKind::BangEqual => {
                Some((Parser::binary, Precedence::Equality))
            }
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual => Some((Parser::binary, Precedence::Comparison)),
            // TokenKind::Equal => Some((Parser::assignment, Precedence::Assignment)),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    Assignment,
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
        kind.infix_entry().unwrap().1
    }

    fn next(&self) -> Precedence {
        match self {
            Precedence::Assignment => Precedence::Logic,
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

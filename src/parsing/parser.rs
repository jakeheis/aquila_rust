use super::ast::*;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::program::*;
use crate::source::*;
use std::rc::Rc;

type Result<T> = DiagnosticResult<T>;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    reporter: Rc<dyn Reporter>,
}

#[derive(Clone, Copy, PartialEq)]
enum Context {
    TopLevel,
    InsideType,
    InsideFunction,
}

impl Parser {
    pub fn new(program: LexedProgram, reporter: Rc<dyn Reporter>) -> Self {
        Parser {
            tokens: program.tokens,
            index: 0,
            reporter,
        }
    }

    pub fn parse(&mut self) -> ParsedProgram {
        let statements = self.stmt_list(Context::TopLevel, None);

        for stmt in &statements {
            let mut printer = ASTPrinter::new();
            stmt.accept(&mut printer);
        }

        ParsedProgram {
            source: Rc::clone(&self.tokens[0].span().source),
            statements,
        }
    }

    fn stmt_list(&mut self, context: Context, end: Option<TokenKind>) -> Vec<Stmt> {
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.is_at_end() && Some(self.peek()) != end {
            match self.statement(context) {
                Ok(stmt) => stmts.push(stmt),
                Err(diagnostic) => {
                    self.reporter.report(diagnostic);
                    self.synchronize();
                }
            }
        }
        stmts
    }

    fn block(&mut self, context: Context) -> Vec<Stmt> {
        self.stmt_list(context, Some(TokenKind::RightBrace))
    }

    fn statement(&mut self, context: Context) -> Result<Stmt> {
        if self.matches(TokenKind::Type) {
            if context == Context::TopLevel {
                self.type_decl()
            } else {
                Err(Diagnostic::error(
                    self.previous(),
                    "Type declaration not allowed",
                ))
            }
        } else if self.matches(TokenKind::Def) {
            if context == Context::TopLevel || context == Context::InsideType {
                self.function_decl()
            } else {
                Err(Diagnostic::error(
                    self.previous(),
                    "Function declaration not allowed",
                ))
            }
        } else if self.matches(TokenKind::Let) {
            let allow_init = context != Context::InsideType;
            let decl = self.variable_decl(allow_init)?;
            self.consume(
                TokenKind::Semicolon,
                "Expected semicolon after variable declaration",
            )
            .replace_span(&decl)?;
            Ok(decl)
        } else if self.matches(TokenKind::If) {
            if context == Context::InsideFunction {
                self.if_stmt()
            } else {
                Err(Diagnostic::error(
                    self.previous(),
                    "If statement not allowed",
                ))
            }
        } else {
            if context == Context::TopLevel || context == Context::InsideFunction {
                let stmt = Stmt::expression(self.parse_precedence(Precedence::Assignment)?);
                self.consume(TokenKind::Semicolon, "Expected semicolon after expression")
                    .replace_span(&stmt)?;
                Ok(stmt)
            } else {
                Err(Diagnostic::error(
                    self.previous(),
                    "Expression not allowed",
                ))
            }
        }
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() && self.previous().kind != TokenKind::Semicolon {
            let done = match self.peek() {
                TokenKind::Type
                | TokenKind::Def
                | TokenKind::Let
                | TokenKind::If
                | TokenKind::RightBrace => true,
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

    fn type_decl(&mut self) -> Result<Stmt> {
        let type_span = self.previous().span.clone();
        let name = self
            .consume(TokenKind::Identifier, "Expect type name")?
            .clone();
        self.consume(TokenKind::LeftBrace, "Expect '{' after type name")?;

        let mut fields: Vec<Stmt> = Vec::new();
        let mut methods: Vec<Stmt> = Vec::new();

        for stmt in self.block(Context::InsideType) {
            match stmt.kind {
                StmtKind::VariableDecl(_, _, _) => fields.push(stmt),
                StmtKind::FunctionDecl(_, _, _, _) => methods.push(stmt),
                _ => panic!(),
            }
        }

        let right_brace = self.consume(TokenKind::RightBrace, "Expect '}' after type body")?;

        Ok(Stmt::type_decl(
            type_span,
            name,
            fields,
            methods,
            right_brace,
        ))
    }

    fn function_decl(&mut self) -> Result<Stmt> {
        let def_span = self.previous().span.clone();
        let name = self
            .consume(TokenKind::Identifier, "Expect function name")?
            .clone();

        let mut params: Vec<Stmt> = Vec::new();
        self.consume(TokenKind::LeftParen, "Expect '(' after function name")?;
        while !self.is_at_end() && self.peek() != TokenKind::RightParen {
            params.push(self.variable_decl(false)?);
            if self.peek() != TokenKind::RightParen {
                self.consume(TokenKind::Comma, "Expect ',' separating parameters")?;
            }
        }
        self.consume(TokenKind::RightParen, "Expect closing ')'")?;

        let return_type = if self.matches(TokenKind::Colon) {
            Some(
                self.consume(TokenKind::Identifier, "Expect return type after ':'")?
                    .clone(),
            )
        } else {
            None
        };

        self.consume(
            TokenKind::LeftBrace,
            "Expect '{' after function declaration",
        )?;
        let body = self.block(Context::InsideFunction);
        let right_brace = self.consume(TokenKind::RightBrace, "Expect '}' after function body")?;

        Ok(Stmt::function_decl(
            def_span,
            name,
            params,
            return_type,
            body,
            right_brace.span().clone(),
        ))
    }

    fn variable_decl(&mut self, allow_initializer: bool) -> Result<Stmt> {
        self.consume(TokenKind::Identifier, "Expected variable name")?;

        let (name, kind) = self.parse_var(true, !allow_initializer)?;

        if self.matches(TokenKind::Equal) {
            if allow_initializer {
                let value = self.expression()?;
                Ok(Stmt::variable_decl(name, kind, Some(value)))
            } else {
                Err(Diagnostic::error(
                    self.previous(),
                    "Variable cannot be initialized",
                ))
            }
        } else {
            Ok(Stmt::variable_decl(name, kind, None))
        }
    }

    fn if_stmt(&mut self) -> Result<Stmt> {
        let if_span = self.previous().span.clone();

        let condition = self.expression()?;

        self.consume(TokenKind::LeftBrace, "Expect '{' after condition")?;
        let body = self.block(Context::InsideFunction);

        let mut end_brace_span = self
            .consume(TokenKind::RightBrace, "Expect '}' after if body")?
            .span
            .clone();

        let else_body = if self.matches(TokenKind::Else) {
            self.consume(TokenKind::LeftBrace, "Expect '{' after else")?;
            let block = self.block(Context::InsideFunction);
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
            return Err(Diagnostic::error(
                self.previous(),
                "Expected expression",
            ));
        }

        let prefix = self
            .advance()
            .kind
            .prefix()
            .ok_or_else(|| Diagnostic::error(self.previous(), "Expected expression"))?;

        let can_assign = prec <= Precedence::Assignment;

        let mut lhs = prefix(self, can_assign)?;

        while !self.is_at_end() {
            if let Some((infix_func, infix_prec)) = self.peek().infix_entry() {
                if infix_prec >= prec {
                    self.advance();
                    lhs = infix_func(self, lhs, can_assign)?;
                    continue;
                }
            }

            break;
        }

        if can_assign && self.matches(TokenKind::Equal) {
            return Err(Diagnostic::error(&lhs, "Invalid assignment target"));
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

    fn call(&mut self, lhs: Expr, _can_assign: bool) -> Result<Expr> {
        let mut args: Vec<Expr> = Vec::new();
        while !self.is_at_end() && self.peek() != TokenKind::RightParen {
            let arg = self.expression()?;
            args.push(arg);
            if self.peek() != TokenKind::RightParen {
                self.consume(TokenKind::Comma, "Expect ',' between arguments")?;
            }
        }
        let right_paren = self.consume(TokenKind::RightParen, "Expect ')' after arguments")?;
        Ok(Expr::call(lhs, args, right_paren))
    }

    fn field(&mut self, lhs: Expr, _can_assign: bool) -> Result<Expr> {
        let field = self.consume(TokenKind::Identifier, "Expect field name after '.'")?;
        Ok(Expr::field(lhs, field))
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
                    self.consume(TokenKind::Identifier, "Expected variable type")?
                        .clone(),
                );
            } else {
                return Err(Diagnostic::error(
                    self.previous(),
                    "Variable type not allowed",
                ));
            }
        } else {
            if require_type {
                return Err(Diagnostic::error(
                    self.previous(),
                    "Variable type required",
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
            return Err(Diagnostic::error(self.previous(), message));
        }

        if self.matches(kind) {
            Ok(self.previous())
        } else {
            Err(Diagnostic::error(self.current(), message))
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

    fn _print_state(&self) {
        println!("prev {} cur {}", self.previous(), self.current());
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
            TokenKind::LeftParen => Some((Parser::call, Precedence::Call)),
            TokenKind::Period => Some((Parser::field, Precedence::Call)),
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
    Call,
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
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Atom,
            Precedence::Atom => panic!(),
        }
    }
}

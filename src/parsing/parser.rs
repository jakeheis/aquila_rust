use super::ast::*;
use crate::diagnostic::*;
use crate::lexing::*;
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
    pub fn new(tokens: Vec<Token>, reporter: Rc<dyn Reporter>) -> Self {
        Parser {
            tokens,
            index: 0,
            reporter,
        }
    }

    pub fn parse(mut self) -> Vec<Stmt> {
        self.stmt_list(Context::TopLevel, None)
    }

    fn stmt_list(&mut self, context: Context, end: Option<TokenKind>) -> Vec<Stmt> {
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.is_at_end() && Some(self.peek()) != end {
            let is_builtin = self.matches(TokenKind::Builtin);
            match self.statement(context) {
                Ok(stmt) => {
                    let stmt = if is_builtin {
                        Stmt::builtin(stmt)
                    } else {
                        stmt
                    };
                    stmts.push(stmt)
                }
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
                self.function_decl(false)
            } else {
                Err(Diagnostic::error(
                    self.previous(),
                    "Function declaration not allowed",
                ))
            }
        } else if self.matches(TokenKind::Meta) {
            if context == Context::InsideType {
                self.function_decl(true)
            } else {
                Err(Diagnostic::error(
                    self.previous(),
                    "Meta function declaration not allowed",
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
            if context == Context::TopLevel || context == Context::InsideFunction {
                self.if_stmt()
            } else {
                Err(Diagnostic::error(
                    self.previous(),
                    "If statement not allowed",
                ))
            }
        } else if self.matches(TokenKind::While) {
            if context == Context::TopLevel || context == Context::InsideFunction {
                self.while_stmt()
            } else {
                Err(Diagnostic::error(
                    self.previous(),
                    "While statement not allowed",
                ))
            }
        } else if self.matches(TokenKind::For) {
            if context == Context::TopLevel || context == Context::InsideFunction {
                self.for_stmt()
            } else {
                Err(Diagnostic::error(
                    self.previous(),
                    "For statement not allowed",
                ))
            }
        } else if self.matches(TokenKind::Return) {
            let stmt = self.return_stmt();
            self.consume(
                TokenKind::Semicolon,
                "Expected semicolon after return statement",
            )?;
            if context == Context::InsideFunction {
                stmt
            } else {
                let span = stmt
                    .map(|s| s.span.clone())
                    .unwrap_or(self.previous().span.clone());
                Err(Diagnostic::error(
                    &span,
                    "Return statement only allowed inside functions",
                ))
            }
        } else if self.matches(TokenKind::Print) {
            let stmt = self.print_stmt();
            self.consume(
                TokenKind::Semicolon,
                "Expected semicolon after print statement",
            )?;
            stmt
        } else {
            if context == Context::TopLevel || context == Context::InsideFunction {
                let stmt = Stmt::expression(self.parse_precedence(Precedence::Assignment)?);
                self.consume(TokenKind::Semicolon, "Expected semicolon after expression")
                    .replace_span(&stmt)?;
                Ok(stmt)
            } else {
                Err(Diagnostic::error(self.previous(), "Expression not allowed"))
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
                | TokenKind::Return
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
        let mut meta_methods: Vec<Stmt> = Vec::new();

        for stmt in self.block(Context::InsideType) {
            match stmt.kind {
                StmtKind::VariableDecl(..) => fields.push(stmt),
                StmtKind::FunctionDecl(.., is_meta) => {
                    if is_meta {
                        meta_methods.push(stmt)
                    } else {
                        methods.push(stmt)
                    }
                }
                _ => panic!(),
            }
        }

        let right_brace = self.consume(TokenKind::RightBrace, "Expect '}' after type body")?;

        Ok(Stmt::type_decl(
            type_span,
            name,
            fields,
            methods,
            meta_methods,
            right_brace,
        ))
    }

    fn function_decl(&mut self, meta: bool) -> Result<Stmt> {
        let start_span = self.previous().span.clone();

        if meta {
            self.consume(TokenKind::Def, "Expect def after meta")?;
        }

        let name = self
            .consume(TokenKind::Identifier, "Expect function name")?
            .clone();

        let mut generics: Vec<Token> = Vec::new();
        if self.matches(TokenKind::Bar) {
            while !self.is_at_end() && self.peek() != TokenKind::Bar {
                let generic_type = self.consume(TokenKind::Identifier, "Expect generic type identifier")?.clone();
                generics.push(generic_type);
                if self.peek() != TokenKind::Bar {
                    self.consume(TokenKind::Comma, "Expect ',' separating parameters")?;
                }
            }
            self.consume(TokenKind::Bar, "Expect '|' after generic parameters")?;
        }

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
            Some(self.parse_explicit_type()?)
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
            start_span,
            name,
            generics,
            params,
            return_type,
            body,
            right_brace.span().clone(),
            meta,
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

    fn while_stmt(&mut self) -> Result<Stmt> {
        let while_span = self.previous().span.clone();

        let condition = self.expression()?;
        self.consume(TokenKind::LeftBrace, "Expect '{' after condition")?;
        let body = self.block(Context::InsideFunction);

        let end_brace_span = &self
            .consume(TokenKind::RightBrace, "Expect '}' after while body")?
            .span;

        Ok(Stmt::while_stmt(
            while_span,
            condition,
            body,
            end_brace_span,
        ))
    }

    fn for_stmt(&mut self) -> Result<Stmt> {
        let for_span = self.previous().span.clone();

        let var_name = self
            .consume(TokenKind::Identifier, "Expect variable name after for")?
            .clone();
        self.consume(TokenKind::In, "Expect 'in' after variable name")?;
        let array = self.expression()?;

        self.consume(TokenKind::LeftBrace, "Expect '{' after array")?;
        let body = self.block(Context::InsideFunction);

        let end_brace_span = &self
            .consume(TokenKind::RightBrace, "Expect '}' after for body")?
            .span;

        Ok(Stmt::for_stmt(
            for_span,
            var_name,
            array,
            body,
            end_brace_span,
        ))
    }

    fn return_stmt(&mut self) -> Result<Stmt> {
        let ret_keyword = self.previous().span.clone();
        if self.peek() != TokenKind::Semicolon {
            let value = self.expression()?;
            Ok(Stmt::return_stmt(ret_keyword, Some(value)))
        } else {
            Ok(Stmt::return_stmt(ret_keyword, None))
        }
    }

    fn print_stmt(&mut self) -> Result<Stmt> {
        let print_keyword = self.previous().span.clone();
        if self.peek() != TokenKind::Semicolon {
            let expr = self.expression()?;
            Ok(Stmt::print_stmt(print_keyword, Some(expr)))
        } else {
            Ok(Stmt::print_stmt(print_keyword, None))
        }
    }

    fn expression(&mut self) -> Result<Expr> {
        self.parse_precedence(Precedence::Logic)
    }

    fn parse_precedence(&mut self, prec: Precedence) -> Result<Expr> {
        if self.is_at_end() {
            return Err(Diagnostic::error(self.previous(), "Expected expression"));
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
        let specialization = if self.previous().kind == TokenKind::Bar {
            let result = self.parse_generic_specialization()?;
            self.consume(TokenKind::LeftParen, "Expect '(' before function arguments")?;
            result
        } else {
            Vec::new()
        };

        let mut args: Vec<Expr> = Vec::new();
        while !self.is_at_end() && self.peek() != TokenKind::RightParen {
            let arg = self.expression()?;
            args.push(arg);
            if self.peek() != TokenKind::RightParen {
                self.consume(TokenKind::Comma, "Expect ',' between arguments")?;
            }
        }

        let right_paren = self.consume(TokenKind::RightParen, "Expect ')' after arguments")?;

        match lhs.kind {
            ExprKind::Field(object, field) => {
                Ok(Expr::method_call(object, field, specialization, args, right_paren))
            }
            ExprKind::Variable(name) => Ok(Expr::function_call(name, specialization, args, right_paren)),
            _ => {
                let span = Span::join(&lhs, right_paren);
                Err(Diagnostic::error(&span, "Cannot call this"))
            }
        }
    }

    fn field(&mut self, lhs: Expr, can_assign: bool) -> Result<Expr> {
        let field_name = self.consume(TokenKind::Identifier, "Expect field name after '.'")?;

        let target_is_call = match &lhs.kind {
            ExprKind::FunctionCall(..) | ExprKind::MethodCall(..) => true,
            _ => false,
        };

        let field = Expr::field(lhs, field_name);

        if can_assign && !target_is_call && self.matches(TokenKind::Equal) {
            self.assignment(field)
        } else {
            Ok(field)
        }
    }

    fn subscript(&mut self, lhs: Expr, _can_assign: bool) -> Result<Expr> {
        let index = self.expression()?;
        let right_bracket =
            self.consume(TokenKind::RightBracket, "Expect ']' after subscript index")?;
        Ok(Expr::subscript(lhs, index, &right_bracket))
    }

    fn literal(&mut self, _can_assign: bool) -> Result<Expr> {
        Ok(Expr::literal(self.previous()))
    }

    fn variable(&mut self, can_assign: bool) -> Result<Expr> {
        let (name, _) = self.parse_var(false, false)?;
        let variable = Expr::variable(name);
        if can_assign && self.matches(TokenKind::Equal) {
            self.assignment(variable)
        } else {
            Ok(variable)
        }
    }

    fn array(&mut self, _can_assign: bool) -> Result<Expr> {
        let left_bracket = self.previous().span.clone();
        let mut elements: Vec<Expr> = Vec::new();
        while !self.is_at_end() && self.peek() != TokenKind::RightBracket {
            elements.push(self.expression()?);
            if self.peek() != TokenKind::RightBracket {
                self.consume(TokenKind::Comma, "Expect ',' between array elements")?;
            }
        }
        let right_bracket =
            self.consume(TokenKind::RightBracket, "Expect ']' after array elements")?;
        Ok(Expr::array(left_bracket, elements, right_bracket))
    }

    fn grouping(&mut self, _can_assign: bool) -> Result<Expr> {
        let expr = self.expression()?;
        self.consume(TokenKind::RightParen, "Expect matching ')'")?;
        Ok(expr)
    }

    fn cast(&mut self, _can_assign: bool) -> Result<Expr> {
        let cast_span = self.previous().span.clone();

        self.consume(TokenKind::Bar, "Expect |cast type|")?;
        
        let mut specialization = self.parse_generic_specialization()?;
        if specialization.len() != 1 {
            return Err(Diagnostic::error(&cast_span, "Expect single cast type"));
        }

        self.consume(TokenKind::LeftParen, "Expect '(' after cast type")?;
        let value = self.expression()?;
        let right_paren = self.consume(TokenKind::RightParen, "Expect matching ')'")?;

        Ok(Expr::cast(cast_span, specialization.remove(0), value, right_paren))
    }

    fn parse_generic_specialization(&mut self) -> Result<Vec<ExplicitType>> {
        let mut specialized_types = Vec::new();
        while !self.is_at_end() && self.peek() != TokenKind::Bar {
            let specialized_type = self.parse_explicit_type()?;
            specialized_types.push(specialized_type);
            if self.peek() != TokenKind::Bar {
                self.consume(TokenKind::Comma, "Expect ',' between generic types")?;
            }
        }
        self.consume(TokenKind::Bar, "Expect '|' after generic specialization")?;
        Ok(specialized_types)
    }

    fn parse_var(&mut self, allow_type: bool, require_type: bool) -> Result<(Token, Option<ExplicitType>)> {
        let name = self.previous().clone();
        let mut explicit_type: Option<ExplicitType> = None;

        if self.matches(TokenKind::Colon) {
            if allow_type {
                explicit_type = Some(self.parse_explicit_type()?);
            } else {
                return Err(Diagnostic::error(
                    self.previous(),
                    "Variable type not allowed",
                ));
            }
        } else {
            if require_type {
                return Err(Diagnostic::error(self.previous(), "Variable type required"));
            }
        }

        Ok((name, explicit_type))
    }

    fn parse_explicit_type(&mut self) -> Result<ExplicitType> {
        let start = self.current().span.clone();

        let category = if self.matches(TokenKind::Ptr) {
            let inside = self.parse_explicit_type()?;
            ExplicitTypeKind::Pointer(Box::new(inside))
        } else if self.matches(TokenKind::LeftBracket) {
            let inside = self.parse_explicit_type()?;
            self.consume(
                TokenKind::Semicolon,
                "Expect ';' after array type before count",
            )?;
            let count = self
                .consume(TokenKind::Number, "Expect array count after ';'")?
                .clone();
            self.consume(TokenKind::RightBracket, "Expect ']' after array count")?;
            ExplicitTypeKind::Array(Box::new(inside), count)
        } else {
            let name = self
                .consume(TokenKind::Identifier, "Expected variable type")?
                .clone();
                ExplicitTypeKind::Simple(ResolvedToken::new(name))
        };

        let end = &self.previous().span;

        Ok(ExplicitType::new(start, category, end))
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
            TokenKind::Minus | TokenKind::Bang | TokenKind::Ampersand | TokenKind::Star => {
                Some(Parser::unary)
            }
            TokenKind::True | TokenKind::False | TokenKind::Number | TokenKind::StringLiteral => {
                Some(Parser::literal)
            }
            TokenKind::Identifier => Some(Parser::variable),
            TokenKind::LeftBracket => Some(Parser::array),
            TokenKind::LeftParen => Some(Parser::grouping),
            TokenKind::Cast => Some(Parser::cast),
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
            TokenKind::LeftParen | TokenKind::Bar => Some((Parser::call, Precedence::Call)),
            TokenKind::Period => Some((Parser::field, Precedence::Call)),
            TokenKind::LeftBracket => Some((Parser::subscript, Precedence::Call)),
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

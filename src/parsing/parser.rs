use super::ast::*;
use super::ast_printer::ASTPrinter;
use super::expr::*;
use crate::diagnostic::*;
use crate::lexing::*;
use crate::library::Lib;
use log::trace;
use std::rc::Rc;

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

    pub fn parse(mut self, name: &str) -> Lib {
        let mut lib = Lib::new(name);

        while !self.is_at_end() {
            match self.top_level(false) {
                Ok(node) => {
                    ASTPrinter::trace().visit(&node);

                    match node {
                        ASTNode::TypeDecl(decl) => lib.type_decls.push(decl),
                        ASTNode::FunctionDecl(decl) => lib.function_decls.push(decl),
                        ASTNode::TraitDecl(decl) => lib.trait_decls.push(decl),
                        ASTNode::ConformanceDecl(decl) => lib.conformance_decls.push(decl),
                        ASTNode::Stmt(decl) => lib.main.push(decl),
                    }
                }
                Err(diagnostic) => {
                    self.reporter.report(diagnostic);
                    self.synchronize();
                }
            }
        }

        lib
    }

    fn top_level(&mut self, public: bool) -> DiagnosticResult<ASTNode> {
        if self.should_parse_function() {
            let decl = self.parse_function(public)?;
            if decl.is_meta {
                Err(Diagnostic::error(&decl.name, "Meta function not allowed"))
            } else {
                Ok(ASTNode::FunctionDecl(decl))
            }
        } else if self.matches(TokenKind::Type) {
            let decl = self.type_decl(public)?;
            Ok(ASTNode::TypeDecl(decl))
        } else if public {
            Err(Diagnostic::error(
                self.previous(),
                "Pub can only modify types declarations, function declarations, or type fields",
            ))
        } else if self.matches(TokenKind::Pub) {
            self.top_level(true)
        } else if self.matches(TokenKind::Trait) {
            let decl = self.trait_decl()?;
            Ok(ASTNode::TraitDecl(decl))
        } else if self.matches(TokenKind::Impl) {
            let decl = self.trait_conformance()?;
            Ok(ASTNode::ConformanceDecl(decl))
        } else {
            let stmt = self.single_stmt(true)?;
            Ok(ASTNode::Stmt(stmt))
        }
    }

    fn should_parse_function(&mut self) -> bool {
        match self.peek() {
            TokenKind::Builtin | TokenKind::Meta | TokenKind::Def => true,
            _ => false,
        }
    }

    fn parse_function(&mut self, public: bool) -> DiagnosticResult<FunctionDecl> {
        if self.matches(TokenKind::Builtin) {
            let mut decl = if self.matches(TokenKind::Meta) {
                self.function_decl(true, false, true, public)?
            } else {
                self.consume(TokenKind::Def, "Expect 'def' after 'builtin'")?;
                self.function_decl(false, false, true, public)?
            };
            decl.is_builtin = true;
            Ok(decl)
        } else if self.matches(TokenKind::Def) {
            self.function_decl(false, true, false, public)
        } else if self.matches(TokenKind::Meta) {
            self.function_decl(true, true, false, public)
        } else {
            panic!()
        }
    }

    fn block(&mut self) -> Vec<Stmt> {
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.is_at_end() && self.peek() != TokenKind::RightBrace {
            let result = self.single_stmt(false);
            if let Some(stmt) = self.success_or_report_and_sync(result) {
                stmts.push(stmt)
            }
        }
        stmts
    }

    fn single_stmt(&mut self, is_top_level: bool) -> DiagnosticResult<Stmt> {
        if self.matches(TokenKind::Let) {
            let decl = self.local_variable_decl()?;
            self.consume(
                TokenKind::Semicolon,
                "Expected semicolon after variable declaration",
            )
            .replace_span(&decl)?;
            Ok(decl)
        } else if self.matches(TokenKind::If) {
            self.if_stmt()
        } else if self.matches(TokenKind::CompileTimeIf) {
            self.conformance_condition()
        } else if self.matches(TokenKind::While) {
            self.while_stmt()
        } else if self.matches(TokenKind::For) {
            self.for_stmt()
        } else if self.matches(TokenKind::Return) {
            let stmt = self.return_stmt();
            self.consume(
                TokenKind::Semicolon,
                "Expected semicolon after return statement",
            )?;
            if !is_top_level {
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
        } else if self.matches(TokenKind::Break) {
            // TODO: error if not in loop
            self.consume(
                TokenKind::Semicolon,
                "Expected semicolon after break statement",
            )?;
            Ok(Stmt::new(StmtKind::BreakStmt, self.previous().span.clone()))
        } else {
            let expr = self.parse_precedence(Precedence::Assignment)?;
            self.consume(TokenKind::Semicolon, "Expected semicolon after expression")
                .replace_span(&expr)?;
            if let ExprKind::Assignment(target, value) = expr.kind {
                return Ok(Stmt::assign(target, value));
            }
            Ok(Stmt::expression(expr))
        }
    }

    fn synchronize(&mut self) {
        trace!(target: "parser", "Synchronizing after {}", self.previous());
        self.advance();
        while !self.is_at_end() && self.previous().kind != TokenKind::Semicolon {
            let done = match self.peek() {
                TokenKind::Type
                | TokenKind::Def
                | TokenKind::Let
                | TokenKind::If
                | TokenKind::Return
                | TokenKind::RightBrace => true,
                TokenKind::Semicolon | TokenKind::LeftBrace => {
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

    fn type_decl(&mut self, public: bool) -> DiagnosticResult<TypeDecl> {
        let name = self
            .consume(TokenKind::Identifier, "Expect type name")?
            .clone();

        let generics = if self.matches(TokenKind::LeftBracket) {
            self.parse_generic_list()?
        } else {
            Vec::new()
        };

        self.consume(TokenKind::LeftBrace, "Expect '{' after type name")?;

        let mut fields: Vec<StructuralVariableDecl> = Vec::new();
        let mut methods: Vec<FunctionDecl> = Vec::new();
        let mut meta_methods: Vec<FunctionDecl> = Vec::new();

        while !self.is_at_end() && self.peek() != TokenKind::RightBrace {
            let public = self.matches(TokenKind::Pub);

            if self.should_parse_function() {
                let result = self.parse_function(public);
                if let Some(function) = self.success_or_report_and_sync(result) {
                    if function.is_meta {
                        meta_methods.push(function);
                    } else {
                        methods.push(function);
                    }
                }
            } else if self.matches(TokenKind::Let) {
                let result = self.structural_variable_decl(public);
                if let Some(field) = self.success_or_report_and_sync(result) {
                    let semicolon =
                        self.consume(TokenKind::Semicolon, "Expect ';' after field declaration");
                    if let Err(diag) = semicolon {
                        self.reporter.report(diag);
                    }
                    fields.push(field);
                }
            } else {
                self.reporter
                    .report(Diagnostic::error(self.current(), "Expression not allowed"));
                self.synchronize();
            }
        }

        self.consume(TokenKind::RightBrace, "Expect '}' after type body")?;

        Ok(TypeDecl::new(
            name,
            generics,
            fields,
            methods,
            meta_methods,
            public,
        ))
    }

    fn function_decl(
        &mut self,
        meta: bool,
        parse_body: bool,
        builtin: bool,
        public: bool,
    ) -> DiagnosticResult<FunctionDecl> {
        if meta {
            self.consume(TokenKind::Def, "Expect def after meta")?;
        }

        let name = self
            .consume(TokenKind::Identifier, "Expect function name")?
            .clone();

        let generics = if self.matches(TokenKind::LeftBracket) {
            self.parse_generic_list()?
        } else {
            Vec::new()
        };

        let mut params: Vec<StructuralVariableDecl> = Vec::new();
        let mut include_caller = false;

        self.consume(TokenKind::LeftParen, "Expect '(' after function name")?;
        while !self.is_at_end() && self.peek() != TokenKind::RightParen {
            if self.matches(TokenKind::Caller) {
                include_caller = true;
            } else {
                params.push(self.structural_variable_decl(false)?);
            }
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

        let generic_restrictions = if self.matches(TokenKind::Where) {
            self.generic_restriction_list()?
        } else {
            Vec::new()
        };

        let body = if parse_body {
            self.consume(
                TokenKind::LeftBrace,
                "Expect '{' after function declaration",
            )?;
            let body = self.block();
            self.consume(TokenKind::RightBrace, "Expect '}' after function body")?;
            body
        } else {
            if self.matches(TokenKind::LeftBrace) {
                return Err(Diagnostic::error(
                    self.previous(),
                    "Function body not allowed here",
                ));
            }

            self.consume(TokenKind::Semicolon, "Expect ';' after function prototype")?;
            Vec::new()
        };

        Ok(FunctionDecl::new(
            name,
            generics,
            params,
            return_type,
            generic_restrictions,
            body,
            meta,
            builtin,
            public,
            include_caller,
        ))
    }

    fn generic_restriction_list(&mut self) -> DiagnosticResult<Vec<GenericRestriction>> {
        let mut restrictions: Vec<GenericRestriction> = Vec::new();

        while !self.is_at_end() && self.peek() != TokenKind::LeftBrace {
            let generic = self
                .consume(TokenKind::Identifier, "Expect generic type")?
                .clone();
            self.consume(TokenKind::Colon, "Expect ':' after generic type")?;
            let trait_name = self
                .consume(TokenKind::Identifier, "Expect trait name")?
                .clone();
            restrictions.push(GenericRestriction {
                generic: SymbolicToken::new(generic),
                trait_name: SymbolicToken::new(trait_name),
            });
        }

        Ok(restrictions)
    }

    fn structural_variable_decl(
        &mut self,
        public: bool,
    ) -> DiagnosticResult<StructuralVariableDecl> {
        let var_name = self
            .consume(TokenKind::Identifier, "Expected variable name")?
            .clone();
        self.consume(TokenKind::Colon, "Exepct ':' after variable name")?;
        let explicit_type = self.parse_explicit_type()?;

        Ok(StructuralVariableDecl::new(var_name, explicit_type, public))
    }

    fn local_variable_decl(&mut self) -> DiagnosticResult<Stmt> {
        let name = self
            .consume(TokenKind::Identifier, "Expected variable name")?
            .clone();

        let explicit_type = if self.matches(TokenKind::Colon) {
            Some(self.parse_explicit_type()?)
        } else {
            None
        };

        let value = if self.matches(TokenKind::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        Ok(Stmt::local_variable_decl(name, explicit_type, value))
    }

    fn trait_decl(&mut self) -> DiagnosticResult<TraitDecl> {
        let name = self
            .consume(TokenKind::Identifier, "Expect trait name")?
            .clone();
        self.consume(TokenKind::LeftBrace, "Expect '{' after trait name")?;

        let mut requirements = Vec::new();
        while !self.is_at_end() && self.peek() != TokenKind::RightBrace {
            let meta = if self.matches(TokenKind::Meta) {
                true
            } else if self.matches(TokenKind::Def) {
                false
            } else {
                return Err(Diagnostic::error(
                    self.current(),
                    "Traits can only require functions",
                ));
            };
            requirements.push(self.function_decl(meta, false, false, false)?);
        }

        self.consume(TokenKind::RightBrace, "Expect '}' after trait body")?;

        Ok(TraitDecl::new(name, requirements))
    }

    fn trait_conformance(&mut self) -> DiagnosticResult<ConformanceDecl> {
        let target = self
            .consume(TokenKind::Identifier, "Expect type name")?
            .clone();
        self.consume(TokenKind::Colon, "Expect ':' after type name")?;
        let trait_name = self
            .consume(TokenKind::Identifier, "Expect trait name")?
            .clone();

        self.consume(TokenKind::LeftBrace, "Expect '{' after trait name")?;

        let mut impls = Vec::new();
        while !self.is_at_end() && self.peek() != TokenKind::RightBrace {
            let meta = if self.matches(TokenKind::Meta) {
                true
            } else if self.matches(TokenKind::Def) {
                false
            } else {
                return Err(Diagnostic::error(
                    self.current(),
                    "Trait conformances can only implement functions",
                ));
            };
            impls.push(self.function_decl(meta, true, false, false)?);
        }

        self.consume(TokenKind::RightBrace, "Expect '}' after impl body")?;

        Ok(ConformanceDecl::new(target, trait_name, impls))
    }

    fn if_stmt(&mut self) -> DiagnosticResult<Stmt> {
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

    fn conformance_condition(&mut self) -> DiagnosticResult<Stmt> {
        let if_span = self.previous().span.clone();

        self.consume(TokenKind::LeftParen, "Expect '(' after #if")?;

        let type_name = self
            .consume(TokenKind::Identifier, "Expect type name")?
            .clone();
        self.consume(TokenKind::Colon, "Expect ':' after type name")?;
        let trait_name = self
            .consume(TokenKind::Identifier, "Expect trait name")?
            .clone();

        self.consume(TokenKind::RightParen, "Expect ')' after condition")?;

        self.consume(TokenKind::LeftBrace, "Expect '{' after condition")?;
        let body = self.block();
        let end_brace = self.consume(TokenKind::RightBrace, "Expect '}' after if body")?;

        Ok(Stmt::conformance_condition(
            if_span, type_name, trait_name, body, end_brace,
        ))
    }

    fn while_stmt(&mut self) -> DiagnosticResult<Stmt> {
        let while_span = self.previous().span.clone();

        let condition = self.expression()?;
        self.consume(TokenKind::LeftBrace, "Expect '{' after condition")?;
        let body = self.block();

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

    fn for_stmt(&mut self) -> DiagnosticResult<Stmt> {
        let for_span = self.previous().span.clone();

        let var_name = self
            .consume(TokenKind::Identifier, "Expect variable name after for")?
            .clone();
        self.consume(TokenKind::In, "Expect 'in' after variable name")?;
        let array = self.expression()?;

        self.consume(TokenKind::LeftBrace, "Expect '{' after array")?;
        let body = self.block();

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

    fn return_stmt(&mut self) -> DiagnosticResult<Stmt> {
        let ret_keyword = self.previous().span.clone();
        if self.peek() != TokenKind::Semicolon {
            let value = self.expression()?;
            Ok(Stmt::return_stmt(ret_keyword, Some(value)))
        } else {
            Ok(Stmt::return_stmt(ret_keyword, None))
        }
    }

    fn expression(&mut self) -> DiagnosticResult<Expr> {
        self.parse_precedence(Precedence::Logic)
    }

    fn parse_precedence(&mut self, prec: Precedence) -> DiagnosticResult<Expr> {
        if self.is_at_end() {
            return Err(Diagnostic::error(self.previous(), "Expected expression"));
        }

        trace!(target: "parser", "Starting expr with {:?} {}", self.current().kind, self.current().lexeme());

        let prefix = self
            .advance()
            .kind
            .prefix()
            .ok_or_else(|| Diagnostic::error(self.previous(), "Expected expression"))?;

        let can_assign = prec <= Precedence::Assignment;

        let mut lhs = prefix(self, can_assign)?;

        trace!(target: "parser", "Parsed prefix {}", lhs.span().lexeme());

        while !self.is_at_end() {
            if let Some((infix_func, infix_prec)) = self.peek().infix_entry() {
                if infix_prec >= prec {
                    self.advance();
                    lhs = infix_func(self, lhs, can_assign)?;
                    trace!(target: "parser", "Parsed infix {}", lhs.span().lexeme());
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

    fn assignment(&mut self, lhs: Expr) -> DiagnosticResult<Expr> {
        let value = self.parse_precedence(Precedence::Equality.next())?;
        Ok(Expr::assignment(lhs, value))
    }

    fn binary(&mut self, lhs: Expr, _can_assign: bool) -> DiagnosticResult<Expr> {
        let operator = self.previous().clone();
        let next_prec = Precedence::for_kind(operator.kind).next();
        let rhs = self.parse_precedence(next_prec)?;
        Ok(Expr::binary(lhs, operator, rhs))
    }

    fn unary(&mut self, _can_assign: bool) -> DiagnosticResult<Expr> {
        let operator = self.previous().clone();
        let expr = self.parse_precedence(Precedence::Unary.next())?;
        Ok(Expr::unary(operator, expr))
    }

    fn call(&mut self, lhs: Expr, _can_assign: bool) -> DiagnosticResult<Expr> {
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
                Ok(Expr::function_call(Some(object), field, args, right_paren))
            }
            ExprKind::Variable(name) => Ok(Expr::function_call(None, name, args, right_paren)),
            _ => {
                let span = Span::join(&lhs, right_paren);
                Err(Diagnostic::error(&span, "Cannot call non-function"))
            }
        }
    }

    fn field(&mut self, lhs: Expr, can_assign: bool) -> DiagnosticResult<Expr> {
        let field_name = self
            .consume(TokenKind::Identifier, "Expect field name after '.'")?
            .clone();

        let specialization = self.parse_possible_specialization()?;

        let target_is_call = match &lhs.kind {
            ExprKind::FunctionCall(..) => true,
            _ => false,
        };

        let field = Expr::field(lhs, field_name, specialization);

        if can_assign && !target_is_call && self.matches(TokenKind::Equal) {
            self.assignment(field)
        } else {
            Ok(field)
        }
    }

    fn subscript(&mut self, lhs: Expr, _can_assign: bool) -> DiagnosticResult<Expr> {
        let index = self.expression()?;
        let right_bracket =
            self.consume(TokenKind::RightBracket, "Expect ']' after subscript index")?;
        Ok(Expr::subscript(lhs, index, &right_bracket))
    }

    fn literal(&mut self, _can_assign: bool) -> DiagnosticResult<Expr> {
        Ok(Expr::literal(self.previous()))
    }

    fn variable(&mut self, can_assign: bool) -> DiagnosticResult<Expr> {
        let name = self.previous().clone();

        let specialization = self.parse_possible_specialization()?;

        let variable = Expr::variable(name, specialization);
        if can_assign && self.matches(TokenKind::Equal) {
            self.assignment(variable)
        } else {
            Ok(variable)
        }
    }

    fn parse_possible_specialization(&mut self) -> DiagnosticResult<Vec<ExplicitType>> {
        if self.peek() == TokenKind::LeftBracket {
            match self.peek_next() {
                Some(TokenKind::Int) => Ok(Vec::new()), // Do nothing, it's a subscript
                _ => {
                    self.consume(TokenKind::LeftBracket, "")?;
                    self.parse_specialization()
                }
            }
        } else {
            Ok(Vec::new())
        }
    }

    fn array(&mut self, _can_assign: bool) -> DiagnosticResult<Expr> {
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

    fn grouping(&mut self, _can_assign: bool) -> DiagnosticResult<Expr> {
        let expr = self.expression()?;
        self.consume(TokenKind::RightParen, "Expect matching ')'")?;
        Ok(expr)
    }

    fn cast(&mut self, _can_assign: bool) -> DiagnosticResult<Expr> {
        let cast_span = self.previous().span.clone();

        self.consume(TokenKind::LeftBracket, "Expect [cast type]")?;

        let mut specialization = self.parse_specialization()?;
        if specialization.len() != 1 {
            return Err(Diagnostic::error(&cast_span, "Expect single cast type"));
        }

        self.consume(TokenKind::LeftParen, "Expect '(' after cast type")?;
        let value = self.expression()?;
        let right_paren = self.consume(TokenKind::RightParen, "Expect matching ')'")?;

        Ok(Expr::cast(
            cast_span,
            specialization.remove(0),
            value,
            right_paren,
        ))
    }

    fn parse_generic_list(&mut self) -> DiagnosticResult<Vec<Token>> {
        let mut generics = Vec::new();
        while !self.is_at_end() && self.peek() != TokenKind::RightBracket {
            let generic_type = self
                .consume(TokenKind::Identifier, "Expect generic type identifier")?
                .clone();
            generics.push(generic_type);
            if self.peek() != TokenKind::RightBracket {
                self.consume(TokenKind::Comma, "Expect ',' separating generic types")?;
            }
        }
        self.consume(TokenKind::RightBracket, "Expect '|' after generic types")?;
        Ok(generics)
    }

    fn parse_specialization(&mut self) -> DiagnosticResult<Vec<ExplicitType>> {
        let mut specialized_types = Vec::new();
        while !self.is_at_end() && self.peek() != TokenKind::RightBracket {
            let specialized_type = self.parse_explicit_type()?;
            specialized_types.push(specialized_type);
            if self.peek() != TokenKind::RightBracket {
                self.consume(TokenKind::Comma, "Expect ',' between generic types")?;
            }
        }
        let message = format!("Expect ']' after generic types");
        self.consume(TokenKind::RightBracket, &message)?;
        Ok(specialized_types)
    }

    fn parse_explicit_type(&mut self) -> DiagnosticResult<ExplicitType> {
        let start = self.current().span.clone();

        let category = if self.matches(TokenKind::Ptr) {
            let inside = self.parse_explicit_type()?;
            ExplicitTypeKind::Pointer(Box::new(inside))
        } else if self.matches(TokenKind::Ref) {
            let inside = self.parse_explicit_type()?;
            ExplicitTypeKind::Reference(Box::new(inside))
        } else if self.matches(TokenKind::LeftBracket) {
            let inside = self.parse_explicit_type()?;
            self.consume(
                TokenKind::Semicolon,
                "Expect ';' after array type before count",
            )?;
            let count = self
                .consume(TokenKind::Int, "Expect array count after ';'")?
                .clone();
            self.consume(TokenKind::RightBracket, "Expect ']' after array count")?;
            ExplicitTypeKind::Array(Box::new(inside), count)
        } else {
            let name = self
                .consume(TokenKind::Identifier, "Expected variable type")?
                .clone();
            let specialization = if self.matches(TokenKind::LeftBracket) {
                self.parse_specialization()?
            } else {
                Vec::new()
            };
            ExplicitTypeKind::Simple(SpecializedToken::new(name, specialization))
        };

        let end = &self.previous().span;

        Ok(ExplicitType::new(start, category, end))
    }

    fn peek(&self) -> TokenKind {
        self.current().kind
    }

    fn peek_next(&self) -> Option<&TokenKind> {
        if self.index + 1 < self.tokens.len() {
            Some(&self.tokens[self.index + 1].kind)
        } else {
            None
        }
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> DiagnosticResult<&Token> {
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

    fn success_or_report_and_sync<T>(&mut self, result: DiagnosticResult<T>) -> Option<T> {
        match result {
            Ok(value) => Some(value),
            Err(diag) => {
                self.reporter.report(diag);
                self.synchronize();
                None
            }
        }
    }
}

// ParseTable

type PrefixFn = fn(&mut Parser, can_assign: bool) -> DiagnosticResult<Expr>;
type InfixFn = fn(&mut Parser, lhs: Expr, can_assign: bool) -> DiagnosticResult<Expr>;

impl TokenKind {
    fn prefix(&self) -> Option<PrefixFn> {
        match self {
            TokenKind::Minus | TokenKind::Bang | TokenKind::Ampersand | TokenKind::Star | TokenKind::At => {
                Some(Parser::unary)
            }
            TokenKind::True
            | TokenKind::False
            | TokenKind::Int
            | TokenKind::Double
            | TokenKind::StringLiteral => Some(Parser::literal),
            TokenKind::Identifier | TokenKind::SelfKeyword => Some(Parser::variable),
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
            TokenKind::PeriodLeftBracket => Some((Parser::subscript, Precedence::Call)),
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

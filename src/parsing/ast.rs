use crate::analysis::{NodeType, Symbol};
use crate::lexing::*;
use crate::source::*;
use std::cell::RefCell;

pub enum StmtKind {
    TypeDecl(Token, Vec<Stmt>, Vec<Stmt>),
    FunctionDecl(Token, Vec<Stmt>, Option<Token>, Vec<Stmt>),
    VariableDecl(Token, Option<Token>, Option<Expr>),
    IfStmt(Expr, Vec<Stmt>, Vec<Stmt>),
    ReturnStmt(Option<Expr>),
    PrintStmt(Option<Expr>),
    ExpressionStmt(Expr),
}

pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
    pub symbol: RefCell<Option<Symbol>>,
    pub stmt_type: RefCell<Option<NodeType>>,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Stmt {
            kind,
            span,
            symbol: RefCell::new(None),
            stmt_type: RefCell::new(None),
        }
    }

    pub fn accept<V: StmtVisitor>(&self, visitor: &mut V) -> V::StmtResult {
        match &self.kind {
            StmtKind::TypeDecl(name, fields, methods) => {
                visitor.visit_type_decl(&self, &name, &fields, &methods)
            }
            StmtKind::FunctionDecl(name, params, return_type, body) => {
                visitor.visit_function_decl(&self, &name, &params, &return_type, &body)
            }
            StmtKind::VariableDecl(name, kind, value) => {
                visitor.visit_variable_decl(&self, &name, &kind, &value)
            }
            StmtKind::IfStmt(condition, body, else_body) => {
                visitor.visit_if_stmt(&self, &condition, &body, &else_body)
            }
            StmtKind::ReturnStmt(expr) => visitor.visit_return_stmt(&self, expr),
            StmtKind::PrintStmt(expr) => visitor.visit_print_stmt(&self, expr),
            StmtKind::ExpressionStmt(expr) => visitor.visit_expression_stmt(&self, expr),
        }
    }

    pub fn type_decl(
        type_span: Span,
        name: Token,
        fields: Vec<Stmt>,
        methods: Vec<Stmt>,
        right_brace: &Token,
    ) -> Self {
        let span = Span::join(&type_span, right_brace);
        Stmt::new(StmtKind::TypeDecl(name, fields, methods), span)
    }

    pub fn function_decl(
        def_span: Span,
        name: Token,
        params: Vec<Stmt>,
        return_type: Option<Token>,
        body: Vec<Stmt>,
        right_brace_span: Span,
    ) -> Self {
        let span = Span::join(&def_span, &right_brace_span);
        Stmt::new(
            StmtKind::FunctionDecl(name, params, return_type, body),
            span,
        )
    }

    pub fn variable_decl(name: Token, var_type: Option<Token>, value: Option<Expr>) -> Self {
        let end_span: &Span = if let Some(value) = &value {
            value.span()
        } else if let Some(var_type) = &var_type {
            var_type.span()
        } else {
            name.span()
        };
        let span = Span::join(&name, end_span);
        Stmt::new(StmtKind::VariableDecl(name, var_type, value), span)
    }

    pub fn if_stmt(
        if_span: Span,
        condition: Expr,
        body: Vec<Stmt>,
        else_body: Vec<Stmt>,
        end_brace_span: Span,
    ) -> Self {
        let span = Span::join(&if_span, &end_brace_span);
        Stmt::new(StmtKind::IfStmt(condition, body, else_body), span)
    }

    pub fn return_stmt(return_keyword: Span, expr: Option<Expr>) -> Self {
        let span = Span::join_opt(&return_keyword, &expr);
        Stmt::new(StmtKind::ReturnStmt(expr), span)
    }

    pub fn print_stmt(print_keyword: Span, expr: Option<Expr>) -> Self {
        let span = Span::join_opt(&print_keyword, &expr);
        Stmt::new(StmtKind::PrintStmt(expr), span)
    }

    pub fn expression(expr: Expr) -> Self {
        let span = expr.span.clone();
        Stmt::new(StmtKind::ExpressionStmt(expr), span)
    }
}

pub trait StmtVisitor {
    type StmtResult;

    fn visit_type_decl(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        fields: &[Stmt],
        methods: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_function_decl(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        params: &[Stmt],
        return_type: &Option<Token>,
        body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_variable_decl(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        kind: &Option<Token>,
        value: &Option<Expr>,
    ) -> Self::StmtResult;

    fn visit_if_stmt(
        &mut self,
        stmt: &Stmt,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_return_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult;

    fn visit_print_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult;

    fn visit_expression_stmt(&mut self, stmt: &Stmt, expr: &Expr) -> Self::StmtResult;
}

// Expr

pub enum ExprKind {
    Assignment(Box<Expr>, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Field(Box<Expr>, Token),
    Literal(Token),
    Variable(Token),
}

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub symbol: RefCell<Option<Symbol>>,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Expr {
            kind,
            span,
            symbol: RefCell::new(None),
        }
    }

    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::ExprResult {
        match &self.kind {
            ExprKind::Assignment(target, value) => {
                visitor.visit_assignment_expr(&self, &target, &value)
            }
            ExprKind::Binary(lhs, op, rhs) => visitor.visit_binary_expr(&self, &lhs, &op, &rhs),
            ExprKind::Unary(op, expr) => visitor.visit_unary_expr(&self, &op, &expr),
            ExprKind::Call(target, args) => visitor.visit_call_expr(&self, &target, &args),
            ExprKind::Field(target, field) => visitor.visit_field_expr(&self, &target, &field),
            ExprKind::Literal(token) => visitor.visit_literal_expr(&self, &token),
            ExprKind::Variable(name) => visitor.visit_variable_expr(&self, &name),
        }
    }

    pub fn assignment(target: Expr, value: Expr) -> Self {
        let span = Span::join(&target.span, &value.span);
        Expr::new(
            ExprKind::Assignment(Box::new(target), Box::new(value)),
            span,
        )
    }

    pub fn binary(lhs: Expr, operator: Token, rhs: Expr) -> Self {
        let span = Span::join(&lhs.span, &rhs.span);
        Expr::new(
            ExprKind::Binary(Box::new(lhs), operator, Box::new(rhs)),
            span,
        )
    }

    pub fn unary(op: Token, expr: Expr) -> Self {
        let span = Span::join(&op.span, &expr.span);
        Expr::new(ExprKind::Unary(op, Box::new(expr)), span)
    }

    pub fn call(target: Expr, args: Vec<Expr>, right_paren: &Token) -> Self {
        let span = Span::join(&target, right_paren);
        Expr::new(ExprKind::Call(Box::new(target), args), span)
    }

    pub fn field(target: Expr, name: &Token) -> Self {
        let span = Span::join(&target, name);
        Expr::new(ExprKind::Field(Box::new(target), name.clone()), span)
    }

    pub fn literal(token: &Token) -> Self {
        Expr::new(ExprKind::Literal(token.clone()), token.span.clone())
    }

    pub fn variable(name: Token) -> Self {
        let span = name.span().clone();
        Expr::new(ExprKind::Variable(name), span)
    }

    pub fn lexeme(&self) -> &str {
        self.span.lexeme()
    }
}

pub trait ExprVisitor {
    type ExprResult;

    fn visit_assignment_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        value: &Expr,
    ) -> Self::ExprResult;
    fn visit_binary_expr(
        &mut self,
        expr: &Expr,
        lhs: &Expr,
        op: &Token,
        rhs: &Expr,
    ) -> Self::ExprResult;
    fn visit_unary_expr(&mut self, expr: &Expr, op: &Token, operand: &Expr) -> Self::ExprResult;
    fn visit_call_expr(&mut self, expr: &Expr, target: &Expr, args: &[Expr]) -> Self::ExprResult;
    fn visit_field_expr(&mut self, expr: &Expr, target: &Expr, field: &Token) -> Self::ExprResult;
    fn visit_literal_expr(&mut self, expr: &Expr, token: &Token) -> Self::ExprResult;
    fn visit_variable_expr(&mut self, expr: &Expr, name: &Token) -> Self::ExprResult;
}

// ASTPrinter

enum ASTPrinterMode {
    Stdout,
    Collect(Vec<String>),
}

pub struct ASTPrinter {
    indent: i32,
    mode: ASTPrinterMode,
}

impl ASTPrinter {
    pub fn new() -> ASTPrinter {
        ASTPrinter {
            indent: 0,
            mode: ASTPrinterMode::Stdout,
        }
    }

    pub fn collect() -> ASTPrinter {
        ASTPrinter {
            indent: 0,
            mode: ASTPrinterMode::Collect(Vec::new()),
        }
    }

    pub fn print(&mut self, program: &super::ParsedProgram) {
        program.type_decls.iter().for_each(|s| s.accept(self));
        program.function_decls.iter().for_each(|s| s.accept(self));
        program.main.iter().for_each(|s| s.accept(self));
    }

    pub fn collected(&self) -> &[String] {
        match &self.mode {
            ASTPrinterMode::Collect(collection) => &collection,
            _ => &[],
        }
    }

    fn write_ln(&mut self, token: &str) {
        let indent = if self.indent > 0 {
            (1..self.indent).map(|_| "|  ").collect::<String>() + "|--"
        } else {
            String::new()
        };

        let line = format!("{}{}", indent, token);

        match &mut self.mode {
            ASTPrinterMode::Stdout => println!("{}", line),
            ASTPrinterMode::Collect(collection) => collection.push(line),
        }
    }

    fn indent<T>(&mut self, block: T)
    where
        T: Fn(&mut ASTPrinter) -> (),
    {
        self.indent += 1;
        block(self);
        self.indent -= 1;
    }
}

impl StmtVisitor for ASTPrinter {
    type StmtResult = ();

    fn visit_type_decl(&mut self, stmt: &Stmt, name: &Token, fields: &[Stmt], methods: &[Stmt]) {
        let symbol = stmt
            .symbol
            .borrow()
            .as_ref()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "TypeDecl(name: {}, symbol: {})",
            name.lexeme(),
            symbol
        ));
        self.indent(|visitor| {
            visitor.write_ln("Fields");
            visitor.indent(|visitor| {
                fields.iter().for_each(|p| p.accept(visitor));
            });
            visitor.write_ln("Methods");
            visitor.indent(|visitor| {
                methods.iter().for_each(|p| p.accept(visitor));
            });
        });
    }

    fn visit_function_decl(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        params: &[Stmt],
        return_type: &Option<Token>,
        body: &[Stmt],
    ) {
        let symbol = stmt
            .symbol
            .borrow()
            .as_ref()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        let resolved_type = stmt
            .stmt_type
            .borrow()
            .as_ref()
            .map(|t| t.to_string())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "FunctionDecl(name: {}, return_type: {}, symbol: {}, resolved_type: {})",
            name.lexeme(),
            return_type.as_ref().map(|r| r.lexeme()).unwrap_or("<void>"),
            symbol,
            resolved_type
        ));
        self.indent(|visitor| {
            visitor.write_ln("Params");
            visitor.indent(|visitor| {
                params.iter().for_each(|p| p.accept(visitor));
            });
            visitor.write_ln("Body");
            visitor.indent(|visitor| {
                body.iter().for_each(|p| p.accept(visitor));
            });
        });
    }

    fn visit_variable_decl(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        kind: &Option<Token>,
        value: &Option<Expr>,
    ) {
        let var_type = kind.as_ref().map(|t| t.lexeme()).unwrap_or("<none>");

        let symbol = stmt
            .symbol
            .borrow()
            .as_ref()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        let resolved_type = stmt
            .stmt_type
            .borrow()
            .as_ref()
            .map(|s| s.to_string())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "VariableDecl(name: {}, explicit_type: {}, symbol: {}, resolved_type: {})",
            name.lexeme(),
            var_type,
            symbol,
            resolved_type,
        ));
        self.indent(|visitor| {
            if let Some(v) = value {
                v.accept(visitor);
            }
        })
    }

    fn visit_if_stmt(&mut self, _stmt: &Stmt, condition: &Expr, body: &[Stmt], else_body: &[Stmt]) {
        self.write_ln("If");
        self.indent(|visitor| {
            visitor.write_ln("Condition");
            visitor.indent(|visitor| {
                condition.accept(visitor);
            });
            visitor.write_ln("Body");
            visitor.indent(|visitor| {
                body.iter().for_each(|s| s.accept(visitor));
            });
            visitor.write_ln("ElseBody");
            visitor.indent(|visitor| {
                else_body.iter().for_each(|s| s.accept(visitor));
            });
        })
    }

    fn visit_return_stmt(&mut self, _stmt: &Stmt, expr: &Option<Expr>) {
        self.write_ln("ReturnStmt");
        if let Some(expr) = expr.as_ref() {
            self.indent(|visitor| {
                expr.accept(visitor);
            })
        }
    }

    fn visit_print_stmt(&mut self, _stmt: &Stmt, expr: &Option<Expr>) {
        self.write_ln("PrintStmt");
        if let Some(expr) = expr.as_ref() {
            self.indent(|visitor| {
                expr.accept(visitor);
            })
        }
    }

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, expr: &Expr) {
        self.write_ln("ExpressionStmt");
        self.indent(|visitor| {
            expr.accept(visitor);
        })
    }
}

impl ExprVisitor for ASTPrinter {
    type ExprResult = ();

    fn visit_assignment_expr(&mut self, _expr: &Expr, target: &Expr, value: &Expr) {
        self.write_ln("Assign");
        self.indent(|visitor| {
            target.accept(visitor);
            value.accept(visitor);
        })
    }

    fn visit_binary_expr(&mut self, _expr: &Expr, lhs: &Expr, op: &Token, rhs: &Expr) {
        self.write_ln(&format!("Binary({})", op.lexeme()));
        self.indent(|visitor| {
            lhs.accept(visitor);
            rhs.accept(visitor);
        })
    }

    fn visit_unary_expr(&mut self, _expr: &Expr, op: &Token, operand: &Expr) {
        self.write_ln(&format!("Unary({})", op.lexeme()));
        self.indent(|visitor| {
            operand.accept(visitor);
        })
    }

    fn visit_call_expr(&mut self, _expr: &Expr, target: &Expr, args: &[Expr]) {
        self.write_ln("Call");
        self.indent(|visitor| {
            visitor.write_ln("Target");
            visitor.indent(|visitor| {
                target.accept(visitor);
            });
            visitor.write_ln("Arguments");
            visitor.indent(|visitor| {
                args.iter().for_each(|a| a.accept(visitor));
            });
        })
    }

    fn visit_field_expr(&mut self, expr: &Expr, target: &Expr, field: &Token) {
        let symbol = expr
            .symbol
            .borrow()
            .as_ref()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "Field(name: {}, symbol: {})",
            field.lexeme(),
            symbol
        ));
        self.indent(|visitor| {
            target.accept(visitor);
        })
    }

    fn visit_literal_expr(&mut self, _expr: &Expr, token: &Token) {
        self.write_ln(&format!("Literal({})", token.lexeme()))
    }

    fn visit_variable_expr(&mut self, expr: &Expr, name: &Token) {
        let symbol = expr
            .symbol
            .borrow()
            .as_ref()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "Variable(name: {}, symbol: {})",
            name.lexeme(),
            symbol
        ))
    }
}

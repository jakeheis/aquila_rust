use crate::lexing::*;
use crate::source::*;

pub enum StmtKind {
    TypeDecl(Token, Vec<Expr>, Vec<Stmt>),
    FunctionDecl(Token, Vec<Expr>, Option<Token>, Vec<Stmt>),
    VariableDecl(Token, Option<Token>, Expr),
    IfStmt(Expr, Vec<Stmt>, Vec<Stmt>),
    Expression(Expr),
}

pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn accept<V: StmtVisitor>(&self, visitor: &mut V) -> V::StmtResult {
        match &self.kind {
            StmtKind::TypeDecl(name, fields, methods) => {
                visitor.visit_type_decl(&name, &fields, &methods)
            }
            StmtKind::FunctionDecl(name, params, return_type, body) => {
                visitor.visit_function_decl(&name, &params, &return_type, &body)
            }
            StmtKind::VariableDecl(name, kind, value) => {
                visitor.visit_variable_decl(&name, &kind, &value)
            }
            StmtKind::IfStmt(condition, body, else_body) => {
                visitor.visit_if_stmt(&condition, &body, &else_body)
            }
            StmtKind::Expression(expr) => visitor.visit_expression_stmt(expr),
        }
    }

    pub fn type_decl(
        type_span: Span,
        name: Token,
        fields: Vec<Expr>,
        methods: Vec<Stmt>,
        right_brace: &Token,
    ) -> Self {
        let span = Span::join(&type_span, right_brace);
        Stmt {
            kind: StmtKind::TypeDecl(name, fields, methods),
            span,
        }
    }

    pub fn function_decl(
        def_span: Span,
        name: Token,
        params: Vec<Expr>,
        return_type: Option<Token>,
        body: Vec<Stmt>,
        right_brace_span: Span,
    ) -> Self {
        let span = Span::join(&def_span, &right_brace_span);
        Stmt {
            kind: StmtKind::FunctionDecl(name, params, return_type, body),
            span,
        }
    }

    pub fn variable_decl(
        let_span: Span,
        name: Token,
        var_type: Option<Token>,
        value: Expr,
    ) -> Self {
        let span = Span::join(&let_span, &value.span);
        Stmt {
            kind: StmtKind::VariableDecl(name, var_type, value),
            span,
        }
    }

    pub fn if_stmt(
        if_span: Span,
        condition: Expr,
        body: Vec<Stmt>,
        else_body: Vec<Stmt>,
        end_brace_span: Span,
    ) -> Self {
        let span = Span::join(&if_span, &end_brace_span);
        Stmt {
            kind: StmtKind::IfStmt(condition, body, else_body),
            span,
        }
    }

    pub fn expression(expr: Expr) -> Self {
        let span = expr.span.clone();
        Stmt {
            kind: StmtKind::Expression(expr),
            span,
        }
    }
}

pub trait StmtVisitor {
    type StmtResult;

    fn visit_type_decl(
        &mut self,
        name: &Token,
        fields: &[Expr],
        methods: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_function_decl(
        &mut self,
        name: &Token,
        params: &[Expr],
        return_type: &Option<Token>,
        body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_variable_decl(
        &mut self,
        name: &Token,
        kind: &Option<Token>,
        value: &Expr,
    ) -> Self::StmtResult;

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Self::StmtResult;
}

// Expr

pub enum ExprKind {
    Assignment(Box<Expr>, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Field(Box<Expr>, Token),
    Literal(Token),
    Variable(Token, Option<Token>),
}

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::ExprResult {
        match &self.kind {
            ExprKind::Assignment(target, value) => visitor.visit_assignment_expr(&target, &value),
            ExprKind::Binary(lhs, op, rhs) => visitor.visit_binary_expr(&lhs, &op, &rhs),
            ExprKind::Unary(op, expr) => visitor.visit_unary_expr(&op, &expr),
            ExprKind::Call(target, args) => visitor.visit_call_expr(&target, &args),
            ExprKind::Field(target, field) => visitor.visit_field_expr(&target, &field),
            ExprKind::Literal(token) => visitor.visit_literal_expr(&token),
            ExprKind::Variable(name, var_type) => visitor.visit_variable_expr(&name, &var_type),
        }
    }

    pub fn assignment(target: Expr, value: Expr) -> Self {
        let span = Span::join(&target.span, &value.span);
        Expr {
            kind: ExprKind::Assignment(Box::new(target), Box::new(value)),
            span,
        }
    }

    pub fn binary(lhs: Expr, operator: Token, rhs: Expr) -> Self {
        let span = Span::join(&lhs.span, &rhs.span);
        Expr {
            kind: ExprKind::Binary(Box::new(lhs), operator, Box::new(rhs)),
            span,
        }
    }

    pub fn unary(op: Token, expr: Expr) -> Self {
        let span = Span::join(&op.span, &expr.span);
        Expr {
            kind: ExprKind::Unary(op, Box::new(expr)),
            span,
        }
    }

    pub fn call(target: Expr, args: Vec<Expr>, right_paren: &Token) -> Self {
        let span = Span::join(&target, right_paren);
        Expr {
            kind: ExprKind::Call(Box::new(target), args),
            span,
        }
    }

    pub fn field(target: Expr, name: &Token) -> Self {
        let span = Span::join(&target, name);
        Expr {
            kind: ExprKind::Field(Box::new(target), name.clone()),
            span,
        }
    }

    pub fn literal(token: &Token) -> Self {
        Expr {
            kind: ExprKind::Literal(token.clone()),
            span: token.span.clone(),
        }
    }

    pub fn variable(name: Token, var_type: Option<Token>) -> Self {
        let span = Span::join_opt(&name, &var_type);
        Expr {
            kind: ExprKind::Variable(name, var_type),
            span,
        }
    }

    pub fn lexeme(&self) -> &str {
        self.span.lexeme()
    }
}

pub trait ExprVisitor {
    type ExprResult;

    fn visit_assignment_expr(&mut self, target: &Expr, value: &Expr) -> Self::ExprResult;
    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::ExprResult;
    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Self::ExprResult;
    fn visit_call_expr(&mut self, target: &Expr, args: &[Expr]) -> Self::ExprResult;
    fn visit_field_expr(&mut self, target: &Expr, field: &Token) -> Self::ExprResult;
    fn visit_literal_expr(&mut self, token: &Token) -> Self::ExprResult;
    fn visit_variable_expr(&mut self, name: &Token, var_type: &Option<Token>) -> Self::ExprResult;
}

// ASTPrinter

pub struct ASTPrinter {
    indent: i32,
}

impl ASTPrinter {
    pub fn new() -> ASTPrinter {
        ASTPrinter { indent: 0 }
    }

    pub fn write_ln(&self, token: &str) {
        let indent = if self.indent > 0 {
            (1..self.indent).map(|_| "|  ").collect::<String>() + "|--"
        } else {
            String::new()
        };
        println!("{}{}", indent, token)
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

    fn visit_type_decl(&mut self, name: &Token, fields: &[Expr], methods: &[Stmt]) {
        self.write_ln(&format!("TypeDecl(name: {})", name.lexeme()));
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
        name: &Token,
        params: &[Expr],
        return_type: &Option<Token>,
        body: &[Stmt],
    ) {
        self.write_ln(&format!(
            "FunctionDecl(name: {}, return_type: {})",
            name.lexeme(),
            return_type.as_ref().map(|r| r.lexeme()).unwrap_or("<void>")
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

    fn visit_variable_decl(&mut self, name: &Token, kind: &Option<Token>, value: &Expr) {
        let var_type = kind.as_ref().map(|t| t.lexeme()).unwrap_or("<none>");
        self.write_ln(&format!(
            "VariableDecl(name: {}, type: {})",
            name.lexeme(),
            var_type
        ));
        self.indent(|visitor| {
            value.accept(visitor);
        })
    }

    fn visit_if_stmt(&mut self, condition: &Expr, body: &[Stmt], else_body: &[Stmt]) {
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

    fn visit_expression_stmt(&mut self, expr: &Expr) {
        self.write_ln("ExpressionStmt");
        self.indent(|visitor| {
            expr.accept(visitor);
        })
    }
}

impl ExprVisitor for ASTPrinter {
    type ExprResult = ();

    fn visit_assignment_expr(&mut self, target: &Expr, value: &Expr) {
        self.write_ln("Assign");
        self.indent(|visitor| {
            target.accept(visitor);
            value.accept(visitor);
        })
    }

    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) {
        self.write_ln(&format!("Binary({})", op.lexeme()));
        self.indent(|visitor| {
            lhs.accept(visitor);
            rhs.accept(visitor);
        })
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) {
        self.write_ln(&format!("Unary({})", op.lexeme()));
        self.indent(|visitor| {
            expr.accept(visitor);
        })
    }

    fn visit_call_expr(&mut self, target: &Expr, args: &[Expr]) {
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

    fn visit_field_expr(&mut self, target: &Expr, field: &Token) {
        self.write_ln(&format!("Field({})", field.lexeme()));
        self.indent(|visitor| {
            target.accept(visitor);
        })
    }

    fn visit_literal_expr(&mut self, token: &Token) {
        self.write_ln(&format!("Literal({})", token.lexeme()))
    }

    fn visit_variable_expr(&mut self, name: &Token, var_type: &Option<Token>) {
        let var_type = var_type.as_ref().map(|t| t.lexeme()).unwrap_or("<none>");
        self.write_ln(&format!(
            "Variable(name: {}, type: {})",
            name.lexeme(),
            var_type
        ))
    }
}

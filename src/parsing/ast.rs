use crate::analysis::{NodeType, Symbol};
use crate::diagnostic::*;
use crate::lexing::*;
use crate::source::*;
use std::cell::RefCell;

pub enum StmtKind {
    TypeDecl(TypedToken, Vec<Stmt>, Vec<Stmt>, Vec<Stmt>),
    FunctionDecl(TypedToken, Vec<Stmt>, Option<Expr>, Vec<Stmt>, bool),
    VariableDecl(TypedToken, Option<Expr>, Option<Expr>),
    IfStmt(Expr, Vec<Stmt>, Vec<Stmt>),
    WhileStmt(Expr, Vec<Stmt>),
    ForStmt(TypedToken, Expr, Vec<Stmt>),
    ReturnStmt(Option<Expr>),
    PrintStmt(Option<Expr>, RefCell<Option<NodeType>>),
    ExpressionStmt(Expr),
    Builtin(Box<Stmt>),
}

pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Stmt { kind, span }
    }

    pub fn accept<V: StmtVisitor>(&self, visitor: &mut V) -> V::StmtResult {
        match &self.kind {
            StmtKind::TypeDecl(name, fields, methods, meta_methods) => {
                visitor.visit_type_decl(&self, &name, &fields, &methods, &meta_methods)
            }
            StmtKind::FunctionDecl(name, params, return_type, body, is_meta) => {
                visitor.visit_function_decl(&self, &name, &params, &return_type, &body, *is_meta)
            }
            StmtKind::VariableDecl(name, kind, value) => {
                visitor.visit_variable_decl(&self, &name, &kind, &value)
            }
            StmtKind::IfStmt(condition, body, else_body) => {
                visitor.visit_if_stmt(&self, &condition, &body, &else_body)
            }
            StmtKind::WhileStmt(condition, body) => {
                visitor.visit_while_stmt(&self, condition, &body)
            },
            StmtKind::ForStmt(variable, array, body) => {
                visitor.visit_for_stmt(&self, variable, array, &body)
            }
            StmtKind::ReturnStmt(expr) => visitor.visit_return_stmt(&self, expr),
            StmtKind::PrintStmt(expr, print_type) => {
                visitor.visit_print_stmt(&self, expr, print_type)
            }
            StmtKind::ExpressionStmt(expr) => visitor.visit_expression_stmt(&self, expr),
            StmtKind::Builtin(stmt) => visitor.visit_builtin_stmt(&self, &stmt),
        }
    }

    pub fn type_decl(
        type_span: Span,
        name: Token,
        fields: Vec<Stmt>,
        methods: Vec<Stmt>,
        meta_methods: Vec<Stmt>,
        right_brace: &Token,
    ) -> Self {
        let span = Span::join(&type_span, right_brace);
        Stmt::new(
            StmtKind::TypeDecl(TypedToken::new(name), fields, methods, meta_methods),
            span,
        )
    }

    pub fn function_decl(
        start_span: Span,
        name: Token,
        params: Vec<Stmt>,
        return_type: Option<Expr>,
        body: Vec<Stmt>,
        right_brace_span: Span,
        is_meta: bool,
    ) -> Self {
        let span = Span::join(&start_span, &right_brace_span);
        Stmt::new(
            StmtKind::FunctionDecl(TypedToken::new(name), params, return_type, body, is_meta),
            span,
        )
    }

    pub fn variable_decl(name: Token, explicit_type: Option<Expr>, value: Option<Expr>) -> Self {
        let end_span: &Span = if let Some(value) = &value {
            value.span()
        } else if let Some(explicit_type) = &explicit_type {
            explicit_type.span()
        } else {
            name.span()
        };
        let span = Span::join(&name, end_span);
        Stmt::new(
            StmtKind::VariableDecl(TypedToken::new(name), explicit_type, value),
            span,
        )
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

    pub fn while_stmt(
        while_span: Span,
        condition: Expr,
        body: Vec<Stmt>,
        end_brace_span: &Span,
    ) -> Self {
        let span = Span::join(&while_span, end_brace_span);
        Stmt::new(StmtKind::WhileStmt(condition, body), span)
    }

    pub fn for_stmt(
        for_span: Span,
        new_var: Token,
        array: Expr,
        body: Vec<Stmt>,
        end_brace_span: &Span,
    ) -> Self {
        let span = Span::join(&for_span, end_brace_span);
        Stmt::new(StmtKind::ForStmt(TypedToken::new(new_var), array, body), span)
    }

    pub fn return_stmt(return_keyword: Span, expr: Option<Expr>) -> Self {
        let span = Span::join_opt(&return_keyword, &expr);
        Stmt::new(StmtKind::ReturnStmt(expr), span)
    }

    pub fn print_stmt(print_keyword: Span, expr: Option<Expr>) -> Self {
        let span = Span::join_opt(&print_keyword, &expr);
        Stmt::new(StmtKind::PrintStmt(expr, RefCell::new(None)), span)
    }

    pub fn expression(expr: Expr) -> Self {
        let span = expr.span.clone();
        Stmt::new(StmtKind::ExpressionStmt(expr), span)
    }

    pub fn builtin(stmt: Stmt) -> Self {
        let span = stmt.span().clone();
        Stmt::new(StmtKind::Builtin(Box::new(stmt)), span)
    }
}

pub trait StmtVisitor {
    type StmtResult;

    fn visit_type_decl(
        &mut self,
        stmt: &Stmt,
        name: &TypedToken,
        fields: &[Stmt],
        methods: &[Stmt],
        meta_methods: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_function_decl(
        &mut self,
        stmt: &Stmt,
        name: &TypedToken,
        params: &[Stmt],
        return_type: &Option<Expr>,
        body: &[Stmt],
        is_meta: bool,
    ) -> Self::StmtResult;

    fn visit_variable_decl(
        &mut self,
        stmt: &Stmt,
        name: &TypedToken,
        kind: &Option<Expr>,
        value: &Option<Expr>,
    ) -> Self::StmtResult;

    fn visit_if_stmt(
        &mut self,
        stmt: &Stmt,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_while_stmt(
        &mut self,
        stmt: &Stmt,
        condition: &Expr,
        body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_for_stmt(
        &mut self,
        stmt: &Stmt,
        variable: &TypedToken,
        array: &Expr,
        body: &[Stmt],
    ) -> Self::StmtResult;

    fn visit_return_stmt(&mut self, stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult;

    fn visit_print_stmt(
        &mut self,
        stmt: &Stmt,
        expr: &Option<Expr>,
        print_type: &RefCell<Option<NodeType>>,
    ) -> Self::StmtResult;

    fn visit_expression_stmt(&mut self, stmt: &Stmt, expr: &Expr) -> Self::StmtResult;

    fn visit_builtin_stmt(&mut self, stmt: &Stmt, inner: &Box<Stmt>) -> Self::StmtResult;
}

// Expr

pub struct TypedToken {
    pub token: Token,
    symbol: RefCell<Option<Symbol>>,
    token_type: RefCell<Option<NodeType>>,
}

impl TypedToken {
    fn new(token: Token) -> Self {
        TypedToken {
            token,
            symbol: RefCell::new(None),
            token_type: RefCell::new(None),
        }
    }

    pub fn set_symbol(&self, symbol: Symbol) {
        self.symbol.replace(Some(symbol));
    }

    pub fn get_symbol(&self) -> Option<Symbol> {
        self.symbol.borrow().as_ref().map(|s| s.clone())
    }

    pub fn set_type(&self, token_type: NodeType) {
        self.token_type.replace(Some(token_type));
    }

    pub fn get_type(&self) -> Option<NodeType> {
        self.token_type.borrow().as_ref().map(|s| s.clone())
    }
}

impl ContainsSpan for TypedToken {
    fn span(&self) -> &Span {
        &self.token.span
    }
}

pub struct ResolvedToken {
    pub token: Token,
    symbol: RefCell<Option<Symbol>>,
}

impl ResolvedToken {
    pub fn new(token: Token) -> Self {
        ResolvedToken {
            token,
            symbol: RefCell::new(None),
        }
    }

    pub fn set_symbol(&self, symbol: Symbol) {
        self.symbol.replace(Some(symbol));
    }

    pub fn get_symbol(&self) -> Option<Symbol> {
        self.symbol.borrow().as_ref().map(|s| s.clone())
    }
}

impl ContainsSpan for ResolvedToken {
    fn span(&self) -> &Span {
        &self.token.span
    }
}

pub enum ExplicitTypeCategory {
    Simple(ResolvedToken),
    Array(Box<Expr>, Token),
    Pointer(Box<Expr>),
}

pub enum ExprKind {
    Assignment(Box<Expr>, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    FunctionCall(ResolvedToken, Vec<Expr>),
    MethodCall(Box<Expr>, ResolvedToken, Vec<Expr>),
    Field(Box<Expr>, ResolvedToken),
    Literal(Token),
    Variable(ResolvedToken),
    Array(Vec<Expr>),
    Subscript(Box<Expr>, Box<Expr>),
    ExplicitType(ExplicitTypeCategory),
}

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub node_type: RefCell<Option<NodeType>>,
}

impl Expr {
    pub fn set_type(&self, node_type: NodeType) -> DiagnosticResult<NodeType> {
        self.node_type.replace(Some(node_type.clone()));
        Ok(node_type)
    }

    pub fn get_type(&self) -> Option<NodeType> {
        self.node_type.borrow().as_ref().map(|t| t.clone())
    }

    pub fn new(kind: ExprKind, span: Span) -> Self {
        Expr {
            kind,
            span,
            node_type: RefCell::new(None),
        }
    }

    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::ExprResult {
        match &self.kind {
            ExprKind::Assignment(target, value) => {
                visitor.visit_assignment_expr(&self, &target, &value)
            }
            ExprKind::Binary(lhs, op, rhs) => visitor.visit_binary_expr(&self, &lhs, &op, &rhs),
            ExprKind::Unary(op, expr) => visitor.visit_unary_expr(&self, &op, &expr),
            ExprKind::FunctionCall(function, args) => {
                visitor.visit_function_call_expr(&self, &function, &args)
            }
            ExprKind::MethodCall(object, method, args) => {
                visitor.visit_method_call_expr(&self, &object, &method, &args)
            }
            ExprKind::Field(target, field) => visitor.visit_field_expr(&self, &target, &field),
            ExprKind::Literal(token) => visitor.visit_literal_expr(&self, &token),
            ExprKind::Variable(name) => visitor.visit_variable_expr(&self, &name),
            ExprKind::Array(elements) => visitor.visit_array_expr(&self, elements),
            ExprKind::Subscript(target, arg) => visitor.visit_subscript_expr(&self, target, arg),
            ExprKind::ExplicitType(category) => visitor.visit_explicit_type_expr(&self, &category),
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

    pub fn function_call(function: ResolvedToken, args: Vec<Expr>, right_paren: &Token) -> Self {
        let span = Span::join(&function, right_paren);
        Expr::new(ExprKind::FunctionCall(function, args), span)
    }

    pub fn method_call(
        object: Box<Expr>,
        method: ResolvedToken,
        args: Vec<Expr>,
        right_paren: &Token,
    ) -> Self {
        let object_ref: &Expr = &object;
        let span = Span::join(object_ref, right_paren);
        Expr::new(ExprKind::MethodCall(object, method, args), span)
    }

    pub fn field(target: Expr, name: &Token) -> Self {
        let span = Span::join(&target, name);
        Expr::new(
            ExprKind::Field(Box::new(target), ResolvedToken::new(name.clone())),
            span,
        )
    }

    pub fn literal(token: &Token) -> Self {
        Expr::new(ExprKind::Literal(token.clone()), token.span.clone())
    }

    pub fn variable(name: Token) -> Self {
        let span = name.span().clone();
        Expr::new(ExprKind::Variable(ResolvedToken::new(name)), span)
    }

    pub fn array(left_bracket: Span, elements: Vec<Expr>, right_bracket: &Token) -> Self {
        let span = Span::join(&left_bracket, right_bracket);
        Expr::new(ExprKind::Array(elements), span)
    }

    pub fn subscript(target: Expr, index: Expr, right_bracket: &Token) -> Self {
        let span = Span::join(&target, right_bracket);
        Expr::new(ExprKind::Subscript(Box::new(target), Box::new(index)), span)
    }

    pub fn explicit_type(first: Span, category: ExplicitTypeCategory, last: &Span) -> Self {
        let span = Span::join(&first, last);
        Expr::new(ExprKind::ExplicitType(category), span)
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
    fn visit_function_call_expr(
        &mut self,
        expr: &Expr,
        function: &ResolvedToken,
        args: &[Expr],
    ) -> Self::ExprResult;
    fn visit_method_call_expr(
        &mut self,
        expr: &Expr,
        object: &Expr,
        method: &ResolvedToken,
        args: &[Expr],
    ) -> Self::ExprResult;
    fn visit_field_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        field: &ResolvedToken,
    ) -> Self::ExprResult;
    fn visit_literal_expr(&mut self, expr: &Expr, token: &Token) -> Self::ExprResult;
    fn visit_variable_expr(&mut self, expr: &Expr, name: &ResolvedToken) -> Self::ExprResult;
    fn visit_array_expr(&mut self, expr: &Expr, elements: &[Expr]) -> Self::ExprResult;
    fn visit_subscript_expr(&mut self, expr: &Expr, target: &Expr, arg: &Expr) -> Self::ExprResult;
    fn visit_explicit_type_expr(
        &mut self,
        expr: &Expr,
        category: &ExplicitTypeCategory,
    ) -> Self::ExprResult;
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

    pub fn print(&mut self, stmts: &[Stmt]) {
        stmts.iter().for_each(|s| s.accept(self));
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

    fn visit_type_decl(
        &mut self,
        _stmt: &Stmt,
        name: &TypedToken,
        fields: &[Stmt],
        methods: &[Stmt],
        meta_methods: &[Stmt],
    ) {
        let symbol = name
            .get_symbol()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "TypeDecl(name: {}, symbol: {})",
            name.span().lexeme(),
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
            visitor.write_ln("MetaMethods");
            visitor.indent(|visitor| {
                meta_methods.iter().for_each(|p| p.accept(visitor));
            });
        });
    }

    fn visit_function_decl(
        &mut self,
        _stmt: &Stmt,
        name: &TypedToken,
        params: &[Stmt],
        return_type: &Option<Expr>,
        body: &[Stmt],
        is_meta: bool,
    ) {
        let symbol = name
            .get_symbol()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        let resolved_type = name
            .get_type()
            .map(|t| t.to_string())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "FunctionDecl(name: {}, return_type: {}, symbol: {}, resolved_type: {}, meta: {})",
            name.span().lexeme(),
            return_type.as_ref().map(|r| r.lexeme()).unwrap_or("<void>"),
            symbol,
            resolved_type,
            is_meta
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
        _stmt: &Stmt,
        name: &TypedToken,
        explicit_type: &Option<Expr>,
        value: &Option<Expr>,
    ) {
        let symbol = name
            .get_symbol()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        let resolved_type = name
            .get_type()
            .map(|s| s.to_string())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "VariableDecl(name: {}, symbol: {}, resolved_type: {})",
            name.span().lexeme(),
            symbol,
            resolved_type,
        ));
        self.indent(|visitor| {
            if let Some(e) = explicit_type {
                e.accept(visitor);
            }
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

    fn visit_while_stmt(
        &mut self,
        _stmt: &Stmt,
        condition: &Expr,
        body: &[Stmt],
    ) {
        self.write_ln("While");
        self.indent(|visitor| {
            visitor.write_ln("Condition");
            visitor.indent(|visitor| {
                condition.accept(visitor);
            });
            visitor.write_ln("Body");
            visitor.indent(|visitor| {
                body.iter().for_each(|s| s.accept(visitor));
            });
        })
    }

    fn visit_for_stmt(
        &mut self,
        _stmt: &Stmt,
        variable: &TypedToken,
        array: &Expr,
        body: &[Stmt],
    ) {
        self.write_ln(&format!("For({})", variable.token.lexeme()));
        self.indent(|visitor| {
            visitor.write_ln("Array");
            visitor.indent(|visitor| {
                array.accept(visitor);
            });
            visitor.write_ln("Body");
            visitor.indent(|visitor| {
                body.iter().for_each(|s| s.accept(visitor));
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

    fn visit_print_stmt(
        &mut self,
        _stmt: &Stmt,
        expr: &Option<Expr>,
        _print_type: &RefCell<Option<NodeType>>,
    ) {
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

    fn visit_builtin_stmt(&mut self, _stmt: &Stmt, inner: &Box<Stmt>) -> Self::StmtResult {
        self.write_ln("Builtin");
        self.indent(|visitor| {
            inner.accept(visitor);
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

    fn visit_function_call_expr(
        &mut self,
        _expr: &Expr,
        function: &ResolvedToken,
        args: &[Expr],
    ) -> Self::ExprResult {
        let symbol = function
            .get_symbol()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "FunctionCall(name: {}, symbol: {})",
            function.token.lexeme(),
            symbol
        ));
        self.indent(|visitor| {
            visitor.write_ln("Arguments");
            visitor.indent(|visitor| {
                args.iter().for_each(|a| a.accept(visitor));
            });
        })
    }

    fn visit_method_call_expr(
        &mut self,
        _expr: &Expr,
        object: &Expr,
        method: &ResolvedToken,
        args: &[Expr],
    ) -> Self::ExprResult {
        let symbol = method
            .get_symbol()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "MethodCall(name: {}, symbol: {})",
            method.token.lexeme(),
            symbol
        ));
        self.indent(|visitor| {
            visitor.write_ln("Object");
            visitor.indent(|visitor| {
                object.accept(visitor);
            });
            visitor.write_ln("Arguments");
            visitor.indent(|visitor| {
                args.iter().for_each(|a| a.accept(visitor));
            });
        })
    }

    fn visit_field_expr(&mut self, _expr: &Expr, target: &Expr, field: &ResolvedToken) {
        let symbol = field
            .get_symbol()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "Field(name: {}, symbol: {})",
            field.span().lexeme(),
            symbol
        ));
        self.indent(|visitor| {
            target.accept(visitor);
        })
    }

    fn visit_literal_expr(&mut self, _expr: &Expr, token: &Token) {
        self.write_ln(&format!("Literal({})", token.lexeme()))
    }

    fn visit_variable_expr(&mut self, _expr: &Expr, name: &ResolvedToken) {
        let symbol = name
            .get_symbol()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "Variable(name: {}, symbol: {})",
            name.span().lexeme(),
            symbol
        ))
    }

    fn visit_array_expr(&mut self, _expr: &Expr, elements: &[Expr]) -> Self::ExprResult {
        self.write_ln("Array");
        self.indent(|writer| {
            elements.iter().for_each(|e| e.accept(writer));
        })
    }

    fn visit_subscript_expr(
        &mut self,
        _expr: &Expr,
        target: &Expr,
        arg: &Expr,
    ) -> Self::ExprResult {
        self.write_ln("Subscript");
        self.indent(|writer| {
            target.accept(writer);
            arg.accept(writer);
        })
    }

    fn visit_explicit_type_expr(&mut self, _expr: &Expr, category: &ExplicitTypeCategory) {
        match category {
            ExplicitTypeCategory::Simple(token) => {
                let symbol = token
                    .get_symbol()
                    .map(|s| s.id.clone())
                    .unwrap_or(String::from("<none>"));
                self.write_ln(&format!(
                    "ExplicitType(name: {}, symbol: {})",
                    token.span().lexeme(),
                    symbol
                ));
            }
            ExplicitTypeCategory::Pointer(to) => {
                self.write_ln("ExplicitType(ptr)");
                self.indent(|visitor| {
                    (*to).accept(visitor);
                })
            }
            ExplicitTypeCategory::Array(of, count) => {
                self.write_ln(&format!("ExplicitType(array<count={}>)", count));
                self.indent(|visitor| {
                    (*of).accept(visitor);
                })
            }
        }
    }
}

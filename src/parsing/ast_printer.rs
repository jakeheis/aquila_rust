use super::*;
use crate::lexing::*;
use crate::source::*;

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

    fn write_explicit_type(&mut self, explicit_type: &ExplicitType) {
        match &explicit_type.kind {
            ExplicitTypeKind::Simple(token) => {
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
            ExplicitTypeKind::Pointer(to) => {
                self.write_ln("ExplicitType(ptr)");
                self.indent(|visitor| {
                    let to: &ExplicitType = &to;
                    visitor.write_explicit_type(to);
                })
            }
            ExplicitTypeKind::Array(of, count) => {
                self.write_ln(&format!("ExplicitType(array<count={}>)", count));
                self.indent(|visitor| {
                    let of: &ExplicitType = &of;
                    visitor.write_explicit_type(of);
                })
            }
        }
    }
}

impl StmtVisitor for ASTPrinter {
    type StmtResult = ();

    fn visit_type_decl(&mut self, decl: &TypeDecl) {
        let symbol = decl
            .name
            .get_symbol()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "TypeDecl(name: {}, symbol: {})",
            decl.name.span().lexeme(),
            symbol
        ));
        self.indent(|visitor| {
            visitor.write_ln("Fields");
            visitor.indent(|visitor| {
                decl.fields
                    .iter()
                    .for_each(|f| visitor.visit_variable_decl(f));
            });
            visitor.write_ln("Methods");
            visitor.indent(|visitor| {
                decl.methods
                    .iter()
                    .for_each(|p| visitor.visit_function_decl(p));
            });
            visitor.write_ln("MetaMethods");
            visitor.indent(|visitor| {
                decl.meta_methods
                    .iter()
                    .for_each(|p| visitor.visit_function_decl(p));
            });
        });
    }

    fn visit_function_decl(&mut self, decl: &FunctionDecl) {
        let symbol = decl
            .name
            .get_symbol()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        let resolved_type = decl
            .name
            .get_type()
            .map(|t| t.to_string())
            .unwrap_or(String::from("<none>"));
        let generics: Vec<_> = decl.generics.iter().map(|g| g.token.lexeme()).collect();
        let generics = format!("<{}>", generics.join(","));
        self.write_ln(&format!(
            "FunctionDecl(name: {}, generics: {}, symbol: {}, resolved_type: {}, meta: {})",
            decl.name.span().lexeme(),
            generics,
            symbol,
            resolved_type,
            decl.is_meta,
        ));
        self.indent(|visitor| {
            if let Some(return_type) = decl.return_type.as_ref() {
                visitor.write_ln("Return");
                visitor.indent(|visitor| visitor.write_explicit_type(return_type));
            }
            visitor.write_ln("Params");
            visitor.indent(|visitor| {
                decl.parameters
                    .iter()
                    .for_each(|p| visitor.visit_variable_decl(p));
            });
            visitor.write_ln("Body");
            visitor.indent(|visitor| {
                decl.body.iter().for_each(|p| p.accept(visitor));
            });
        });
    }

    fn visit_variable_decl(&mut self, decl: &VariableDecl) {
        let symbol = decl
            .name
            .get_symbol()
            .map(|s| s.id.clone())
            .unwrap_or(String::from("<none>"));
        let resolved_type = decl
            .name
            .get_type()
            .map(|s| s.to_string())
            .unwrap_or(String::from("<none>"));
        self.write_ln(&format!(
            "VariableDecl(name: {}, symbol: {}, resolved_type: {})",
            decl.name.span().lexeme(),
            symbol,
            resolved_type,
        ));
        self.indent(|visitor| {
            if let Some(e) = decl.explicit_type.as_ref() {
                visitor.write_explicit_type(e);
            }
            if let Some(v) = decl.initial_value.as_ref() {
                v.accept(visitor);
            }
        })
    }

    fn visit_trait_decl(&mut self, name: &TypedToken, requirements: &[Stmt]) {
        self.write_ln(&format!("TraitDecl(name: {})", name.span().lexeme(),));
        self.indent(|visitor| {
            requirements.iter().for_each(|p| p.accept(visitor));
        });
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

    fn visit_while_stmt(&mut self, condition: &Expr, body: &[Stmt]) {
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

    fn visit_for_stmt(&mut self, variable: &TypedToken, array: &Expr, body: &[Stmt]) {
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

    fn visit_print_stmt(&mut self, expr: &Option<Expr>) {
        self.write_ln("PrintStmt");
        if let Some(expr) = expr.as_ref() {
            self.indent(|visitor| {
                expr.accept(visitor);
            })
        }
    }

    fn visit_expression_stmt(&mut self, expr: &Expr) {
        self.write_ln("ExpressionStmt");
        self.indent(|visitor| {
            expr.accept(visitor);
        })
    }

    fn visit_builtin_stmt(&mut self, inner: &Box<Stmt>) -> Self::StmtResult {
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
        target: Option<&Expr>,
        function: &ResolvedToken,
        generics: &[ExplicitType],
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
            if let Some(target) = target {
                visitor.write_ln("Target");
                visitor.indent(|visitor| {
                    target.accept(visitor);
                });
            }
            if generics.len() > 0 {
                visitor.write_ln("Specializations");
                visitor.indent(|visitor| {
                    generics.iter().for_each(|a| visitor.write_explicit_type(a));
                });
            }
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

    fn visit_cast_expr(
        &mut self,
        _expr: &Expr,
        explicit_type: &ExplicitType,
        value: &Expr,
    ) -> Self::ExprResult {
        self.write_ln("Cast");
        self.indent(|writer| {
            writer.write_explicit_type(explicit_type);
            value.accept(writer);
        });
    }
}

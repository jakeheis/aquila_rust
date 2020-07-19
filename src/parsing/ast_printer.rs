use super::*;
use crate::lexing::Token;
use crate::library::Symbol;
use crate::source::*;
use log::trace;

enum ASTPrinterMode {
    Trace,
    Stdout,
    Collect(Vec<String>),
}

pub struct ASTPrinter {
    indent: i32,
    mode: ASTPrinterMode,
}

impl ASTPrinter {
    pub fn trace() -> ASTPrinter {
        ASTPrinter {
            indent: 0,
            mode: ASTPrinterMode::Trace,
        }
    }

    pub fn stdout() -> ASTPrinter {
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

    pub fn print_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            stmt.accept(self);
        }
    }

    pub fn visit(&mut self, node: &ASTNode) {
        match node {
            ASTNode::TypeDecl(decl) => self.visit_type_decl(decl),
            ASTNode::FunctionDecl(decl) => self.visit_function_decl(decl),
            ASTNode::TraitDecl(decl) => self.visit_trait_decl(decl),
            ASTNode::ConformanceDecl(decl) => self.visit_conformance_decl(decl),
            ASTNode::Stmt(stmt) => stmt.accept(self),
        }
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
            ASTPrinterMode::Trace => trace!(target: "parser", "{}", line),
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
                let symbol = token.get_symbol().unwrap_or(Symbol::lib_root("<none>"));
                self.write_ln(&format!(
                    "ExplicitType(name: {}, symbol: {})",
                    token.span().lexeme(),
                    symbol.unique_id()
                ));
                self.indent(|visitor| {
                    for spec in &token.specialization {
                        visitor.write_explicit_type(spec);
                    }
                });
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

    fn visit_type_decl(&mut self, decl: &TypeDecl) {
        let symbol = decl.name.get_symbol().unwrap_or(Symbol::lib_root("<none>"));
        self.write_ln(&format!(
            "TypeDecl(name: {}, symbol: {}, pub: {})",
            decl.name.span().lexeme(),
            symbol.unique_id(),
            decl.is_public
        ));
        self.indent(|visitor| {
            visitor.write_ln("Fields");
            visitor.indent(|visitor| {
                decl.fields
                    .iter()
                    .for_each(|f| visitor.visit_structural_variable_decl(f));
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

    pub fn visit_function_decl(&mut self, decl: &FunctionDecl) {
        let symbol = decl.name.get_symbol().unwrap_or(Symbol::lib_root("<none>"));
        let generics: Vec<_> = decl.generics.iter().map(|g| g.span().lexeme()).collect();
        let generics = format!("<{}>", generics.join(","));
        self.write_ln(&format!(
            "FunctionDecl(name: {}, generics: {}, symbol: {}, meta: {}, pub: {}, include_caller: {})",
            decl.name.span().lexeme(),
            generics,
            symbol.unique_id(),
            decl.is_meta,
            decl.is_public,
            decl.include_caller,
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
                    .for_each(|p| visitor.visit_structural_variable_decl(p));
            });
            visitor.write_ln("Body");
            visitor.indent(|visitor| {
                decl.body.iter().for_each(|p| p.accept(visitor));
            });
        });
    }

    fn visit_structural_variable_decl(&mut self, decl: &StructuralVariableDecl) {
        self.write_ln(&format!(
            "StructuralVariableDecl(name: {}, pub: {})",
            decl.name.span().lexeme(),
            decl.is_public,
        ));
        self.indent(|visitor| {
            visitor.write_explicit_type(&decl.explicit_type);
        })
    }

    fn visit_trait_decl(&mut self, decl: &TraitDecl) {
        self.write_ln(&format!("TraitDecl(name: {})", decl.name.span().lexeme(),));
        self.indent(|visitor| {
            decl.requirements
                .iter()
                .for_each(|p| visitor.visit_function_decl(p));
        });
    }

    fn visit_conformance_decl(&mut self, decl: &ConformanceDecl) {
        self.write_ln(&format!(
            "Conformance(name: {}, trait: {})",
            decl.target.span().lexeme(),
            decl.trait_name.span().lexeme()
        ));
        self.indent(|visitor| {
            decl.implementations
                .iter()
                .for_each(|p| visitor.visit_function_decl(p));
        });
    }
}

impl StmtVisitor for ASTPrinter {
    type StmtResult = ();

    fn visit_local_variable_decl(&mut self, decl: &LocalVariableDecl) {
        let symbol = decl.name.get_symbol().unwrap_or(Symbol::lib_root("<none>"));
        self.write_ln(&format!(
            "LocalVariableDecl(name: {}, symbol: {})",
            decl.name.span().lexeme(),
            symbol.unique_id(),
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

    fn visit_assignment_stmt(&mut self, target: &Expr, value: &Expr) {
        self.write_ln("Assign");
        self.indent(|visitor| {
            target.accept(visitor);
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

    fn visit_conformance_condition_stmt(
        &mut self,
        type_name: &SymbolicToken,
        trait_name: &SymbolicToken,
        body: &[Stmt],
    ) {
        let message = format!(
            "Conformance(type: {}, trait: {})",
            type_name.token.lexeme(),
            trait_name.token.lexeme()
        );
        self.write_ln(&message);
        self.indent(|visitor| {
            body.iter().for_each(|s| s.accept(visitor));
        });
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

    fn visit_for_stmt(&mut self, variable: &SymbolicToken, array: &Expr, body: &[Stmt]) {
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

    fn visit_expression_stmt(&mut self, expr: &Expr) {
        self.write_ln("ExpressionStmt");
        self.indent(|visitor| {
            expr.accept(visitor);
        })
    }

    fn visit_break_stmt(&mut self) {
        self.write_ln("BreakStmt");
    }
}

impl ExprVisitor for ASTPrinter {
    type ExprResult = ();

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

    fn visit_function_call_expr(&mut self, _expr: &Expr, call: &FunctionCall) -> Self::ExprResult {
        let symbol = call.name.get_symbol().unwrap_or(Symbol::lib_root("<none>"));
        self.write_ln(&format!(
            "FunctionCall(name: {}, symbol: {})",
            call.name.token.lexeme(),
            symbol.unique_id()
        ));
        self.indent(|visitor| {
            if let Some(target) = &call.target {
                visitor.write_ln("Target");
                visitor.indent(|visitor| {
                    target.accept(visitor);
                });
            }
            if call.name.specialization.len() > 0 {
                visitor.write_ln("Specializations");
                visitor.indent(|visitor| {
                    call.name
                        .specialization
                        .iter()
                        .for_each(|a| visitor.write_explicit_type(a));
                });
            }
            visitor.write_ln("Arguments");
            visitor.indent(|visitor| {
                call.arguments.iter().for_each(|a| a.accept(visitor));
            });
        })
    }

    fn visit_field_expr(&mut self, _expr: &Expr, target: &Expr, field: &SpecializedToken) {
        let symbol = field.get_symbol().unwrap_or(Symbol::lib_root("<none>"));
        self.write_ln(&format!(
            "Field(name: {}, symbol: {})",
            field.span().lexeme(),
            symbol.unique_id()
        ));
        self.indent(|visitor| {
            target.accept(visitor);
        })
    }

    fn visit_literal_expr(&mut self, _expr: &Expr, token: &Token) {
        self.write_ln(&format!("Literal({})", token.lexeme()))
    }

    fn visit_variable_expr(&mut self, _expr: &Expr, name: &SpecializedToken) {
        let symbol = name.get_symbol().unwrap_or(Symbol::lib_root("<none>"));
        self.write_ln(&format!(
            "VariableAccess(name: {}, symbol: {})",
            name.span().lexeme(),
            symbol.unique_id()
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

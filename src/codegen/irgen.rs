use super::builtins;
use super::ir::*;
use super::irwriter::IRWriter;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use log::trace;
use std::rc::Rc;

pub struct IRGen {
    lib: Rc<Lib>,
    writer: IRWriter,
    current_type: Option<TypeMetadata>,
}

impl IRGen {
    pub fn new(lib: Lib) -> Self {        
        let lib = Rc::new(lib);
        let lib_copy = Rc::clone(&lib);
        IRGen {
            lib,
            writer: IRWriter::new(lib_copy),
            current_type: None,
        }
    }

    pub fn generate(mut self) -> Vec<Module> {
        let lib = Rc::clone(&self.lib);

        for t in &lib.type_decls {
            self.visit_type_decl(t);
        }
        self.gen_function_decls(&lib.function_decls);
        for c in &lib.conformance_decls {
            self.conformance_decl(c);
        }

        if !lib.main.is_empty() {
            self.writer.start_block();
            for s in &lib.main {
                s.accept(&mut self);
            }
            self.writer.return_value(IRExpr::int_literal("0"));
            self.writer.end_decl_main();
        }

        let (lib_copy, structs, funcs) = (self.writer.lib, self.writer.structures, self.writer.functions);

        std::mem::drop(lib);
        std::mem::drop(lib_copy);

        let lib = Rc::try_unwrap(self.lib).ok().unwrap();
        let (name, symbols, tracker, mut modules) = (lib.name, lib.symbols, lib.specialization_tracker, lib.dependencies);

        let new = Module {
            name,
            structures: structs,
            functions: funcs,
            symbols: Rc::new(symbols),
            specialization_tracker: tracker
        };
        modules.push(new);
        modules
    }

    fn gen_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            stmt.accept(self);
        }
    }

    fn gen_function_decls(&mut self, decls: &[FunctionDecl]) {
        for decl in decls {
            self.gen_function_decl(decl);
        }
    }

    fn self_type(&self) -> Option<NodeType> {
        let ct = self.current_type.as_ref();
        if let Some(current_type) = ct {
            Some(current_type.unspecialized_type())
        } else {
            None
        }
    }

    fn array_count(&self, expr: &Expr) -> usize {
        if let NodeType::Array(_, count) = expr.get_type().unwrap() {
            count
        } else {
            panic!()
        }
    }

    fn get_expr_type(&self, expr: &Expr) -> NodeType {
        expr.get_type().unwrap()
    }

    fn visit_type_decl(&mut self, decl: &TypeDecl) {
        let type_symbol = decl.name.get_symbol().unwrap();

        let type_metadata = self.lib.type_metadata(&type_symbol).unwrap();

        self.writer.declare_struct(&type_metadata);

        let meta_symbol = Symbol::meta_symbol(&type_symbol);
        let init_symbol = Symbol::init_symbol(&meta_symbol);
        let init_metadata = self.lib.function_metadata(&init_symbol).unwrap();

        let new_item = IRVariable::new("new_item", init_metadata.return_type.clone());

        self.writer.start_block();
        self.writer.declare_var(&new_item);

        for (field, field_type) in type_metadata
            .field_symbols
            .iter()
            .zip(&type_metadata.field_types)
        {
            let field_expr = IRExpr::field(&new_item, &field.mangled(), field_type.clone());
            let param = IRVariable::new_sym(&field, field_type.clone());
            self.writer.assign(field_expr, IRExpr::variable(&param));
        }
        self.writer.return_value(IRExpr::variable(&new_item));
        self.writer.end_decl_func(&init_metadata);

        trace!("Writing methods for {}", type_symbol);

        self.current_type = Some(type_metadata.clone());
        self.gen_function_decls(&decl.methods);
        self.gen_function_decls(&decl.meta_methods);
        self.current_type = None;

        trace!(target: "codegen", "Finished methods for {}", type_symbol);
    }

    fn gen_function_decl(&mut self, decl: &FunctionDecl) {
        let func_symbol = decl.name.get_symbol().unwrap();

        let mut func_metadata = self.lib.function_metadata(&func_symbol).unwrap().clone();

        if decl.include_caller {
            let caller_symbol = Symbol::new_str(&func_symbol, "caller");
            func_metadata.parameter_symbols.push(caller_symbol);
            func_metadata.parameter_types.push(NodeType::pointer_to(NodeType::Byte));
        }

        if decl.is_builtin {
            builtins::write_special_function(
                &mut self.writer,
                &func_metadata,
            )
        } else {
            self.writer.start_block();
            self.gen_stmts(&decl.body);
            self.writer.end_decl_func(&func_metadata);
        }
    }

    fn conformance_decl(&mut self, decl: &ConformanceDecl) {
        let type_symbol = decl.target.get_symbol().unwrap();
        let type_metadata = self.lib.type_metadata(&type_symbol).unwrap();

        self.current_type = Some(type_metadata.clone());
        self.gen_function_decls(&decl.implementations);
        self.current_type = None;
    }
}

impl StmtVisitor for IRGen {
    type StmtResult = ();

    fn visit_local_variable_decl(&mut self, decl: &LocalVariableDecl) -> Self::StmtResult {
        let var_symbol = decl.name.get_symbol().unwrap();
        let mut var_type = decl.get_type().unwrap();
        var_type = var_type.coerce_array_to_ptr();

        let local = self.writer.declare_local(var_symbol, var_type);

        if let Some(init_value) = decl.initial_value.as_ref() {
            let init_value = init_value.accept(self);
            self.writer.assign(IRExpr::variable(&local), init_value);
        }
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) -> Self::StmtResult {
        let condition = condition.accept(self);
        self.writer.start_block();
        self.gen_stmts(body);
        if else_body.is_empty() {
            self.writer.end_if_block(condition);
        } else {
            self.writer.start_block();
            self.gen_stmts(else_body);
            self.writer.end_if_else_blocks(condition);
        }
    }

    fn visit_while_stmt(&mut self, condition: &Expr, body: &[Stmt]) {
        self.writer.start_block();

        let condition = condition.accept(self);
        self.writer.start_block();
        self.writer.break_loop();
        self.writer.end_if_block(IRExpr {
            kind: IRExprKind::Unary(IRUnaryOperator::Invert, Box::new(condition)),
            expr_type: NodeType::Bool,
        });

        self.gen_stmts(body);

        self.writer.end_loop();
    }

    fn visit_for_stmt(&mut self, variable: &SymbolicToken, array_expr: &Expr, body: &[Stmt]) {
        let array = array_expr.accept(self);
        let limit = self.array_count(array_expr);

        let counter = self.writer.declare_local_str("i", NodeType::Int);

        self.writer.start_block();

        self.writer.start_block();
        self.writer.break_loop();
        self.writer.end_if_block(IRExpr {
            kind: IRExprKind::Binary(
                Box::new(IRExpr::variable(&counter)),
                IRBinaryOperator::GreaterEqual,
                Box::new(IRExpr::int_literal(&limit.to_string())),
            ),
            expr_type: NodeType::Bool,
        });

        let var_type = array_expr.get_type().unwrap().coerce_array_to_ptr();
        
        let local = self
            .writer
            .declare_local(variable.get_symbol().unwrap(), var_type.clone());

        self.writer.assign(
            IRExpr::variable(&local),
            IRExpr {
                kind: IRExprKind::Binary(
                    Box::new(array),
                    IRBinaryOperator::Plus,
                    Box::new(IRExpr::variable(&counter)),
                ),
                expr_type: var_type,
            },
        );

        self.gen_stmts(body);
        self.writer.assign(
            IRExpr::variable(&counter),
            IRExpr {
                kind: IRExprKind::Binary(
                    Box::new(IRExpr::variable(&counter)),
                    IRBinaryOperator::Plus,
                    Box::new(IRExpr::int_literal("1")),
                ),
                expr_type: NodeType::Int,
            },
        );
        self.writer.end_loop();
    }

    fn visit_return_stmt(&mut self, _stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult {
        let ret = expr.as_ref().map(|e| e.accept(self));
        self.writer.return_opt(ret);
    }

    fn visit_assignment_stmt(&mut self, target: &Expr, value: &Expr) -> Self::StmtResult {
        let target = target.accept(self);
        let value = value.accept(self);
        self.writer.assign(target, value);
    }

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Self::StmtResult {
        let expr = expr.accept(self);
        self.writer.expr(expr);
    }

    fn visit_break_stmt(&mut self) {
        self.writer.break_loop();
    }
}

impl ExprVisitor for IRGen {
    type ExprResult = IRExpr;

    fn visit_binary_expr(
        &mut self,
        expr: &Expr,
        lhs: &Expr,
        op: &Token,
        rhs: &Expr,
    ) -> Self::ExprResult {
        let lhs = lhs.accept(self);
        let rhs = rhs.accept(self);
        let op_type = match &op.kind {
            TokenKind::Plus => IRBinaryOperator::Plus,
            TokenKind::Minus => IRBinaryOperator::Minus,
            TokenKind::Star => IRBinaryOperator::Multiply,
            TokenKind::Slash => IRBinaryOperator::Divide,
            TokenKind::EqualEqual => IRBinaryOperator::EqualEqual,
            TokenKind::BangEqual => IRBinaryOperator::BangEqual,
            TokenKind::Greater => IRBinaryOperator::Greater,
            TokenKind::GreaterEqual => IRBinaryOperator::GreaterEqual,
            TokenKind::Less => IRBinaryOperator::Less,
            TokenKind::LessEqual => IRBinaryOperator::LessEqual,
            TokenKind::AmpersandAmpersand => IRBinaryOperator::And,
            TokenKind::BarBar => IRBinaryOperator::Or,
            tk => panic!("Illegal operator {:?}", tk),
        };
        IRExpr {
            kind: IRExprKind::Binary(Box::new(lhs), op_type, Box::new(rhs)),
            expr_type: self.get_expr_type(expr),
        }
    }

    fn visit_unary_expr(&mut self, expr: &Expr, op: &Token, operand: &Expr) -> Self::ExprResult {
        let operand = operand.accept(self);
        let op_type = match &op.kind {
            TokenKind::Minus => IRUnaryOperator::Negate,
            TokenKind::Bang => IRUnaryOperator::Invert,
            TokenKind::Ampersand => IRUnaryOperator::Reference,
            TokenKind::Star => IRUnaryOperator::Dereference,
            tk => panic!("Illegal operator {:?}", tk),
        };
        IRExpr {
            kind: IRExprKind::Unary(op_type, Box::new(operand)),
            expr_type: self.get_expr_type(expr),
        }
    }

    fn visit_function_call_expr(
        &mut self,
        expr: &Expr,
        call: &FunctionCall,
    ) -> Self::ExprResult {
        let mut arg_exprs: Vec<_> = call.arguments.iter().map(|a| a.accept(self)).collect();

        let function_symbol = call.name.get_symbol().unwrap();
        let function_metadata = self.lib.function_metadata(&function_symbol).unwrap();

        trace!("Writing call to {}", function_symbol);

        let specialization = call.get_specialization().unwrap();

        if let Some(target) = &call.target {
            if let FunctionKind::Method(..) = function_metadata.kind {
                let target_expr = target.accept(self);
                let target_expr = match &target_expr.kind {
                    IRExprKind::Variable(v) if v == "self" => target_expr,
                    _ => {
                        let expr_type = NodeType::pointer_to(target_expr.expr_type.clone());
                        IRExpr {
                            kind: IRExprKind::Unary(
                                IRUnaryOperator::Reference,
                                Box::new(target_expr),
                            ),
                            expr_type,
                        }
                    }
                };
                arg_exprs.insert(0, target_expr);
            }
        } else {
            if let FunctionKind::Method(..) = function_metadata.kind {
                arg_exprs.insert(
                    0,
                    IRExpr {
                        kind: IRExprKind::Variable(String::from("self")),
                        expr_type: self.self_type().unwrap(),
                    },
                );
            }
        }

        let function_metadata = self.lib.function_metadata(&function_symbol).unwrap();
        if function_metadata.include_caller {
            let location = expr.span.location();
            arg_exprs.push(IRExpr::string_literal(&location));
        }

        if builtins::is_direct_c_binding(&function_symbol) {
            return IRExpr::call_nongenric(function_symbol.last_component(), arg_exprs, self.get_expr_type(expr))
        } else if builtins::can_write_special_call(&function_symbol) {
            builtins::write_special_call(&function_symbol, &specialization, arg_exprs)
        } else {
            IRExpr::call_generic(function_symbol, specialization, arg_exprs, self.get_expr_type(expr))
        }
    }

    fn visit_field_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        field: &SpecializedToken,
    ) -> Self::ExprResult {
        let target_expr = target.accept(self);
        let field_symbol = field.get_symbol().unwrap();

        let kind = match &target.kind {
            ExprKind::Variable(t) if t.get_symbol().unwrap().is_self() => {
                IRExprKind::DerefFieldAccess(Box::new(target_expr), field_symbol.mangled())
            }
            _ => IRExprKind::FieldAccess(Box::new(target_expr), field_symbol.mangled()),
        };

        IRExpr {
            kind,
            expr_type: self.get_expr_type(expr),
        }
    }

    fn visit_literal_expr(&mut self, expr: &Expr, token: &Token) -> Self::ExprResult {
        IRExpr {
            kind: IRExprKind::Literal(token.lexeme().to_string()),
            expr_type: self.get_expr_type(expr),
        }
    }

    fn visit_variable_expr(&mut self, expr: &Expr, name: &SpecializedToken) -> Self::ExprResult {
        let nonspec_expr_type = expr.get_type().unwrap();

        if let NodeType::Metatype(..) = &nonspec_expr_type {
            return IRExpr {
                kind: IRExprKind::ExplicitType,
                expr_type: nonspec_expr_type,
            };
        }

        let expr_type = self.get_expr_type(expr);
        let symbol = name.get_symbol().unwrap();

        if let Some(current_type) = self.current_type.as_ref() {
            let self_expr = IRExpr {
                kind: IRExprKind::Variable(String::from("self")),
                expr_type: current_type.unspecialized_type(),
            };
            if symbol.is_self() {
                return self_expr;
            } else if current_type.symbol.owns(&symbol) {
                let kind = IRExprKind::DerefFieldAccess(
                    Box::new(IRExpr {
                        kind: self_expr.kind,
                        expr_type: NodeType::pointer_to(self_expr.expr_type),
                    }),
                    symbol.mangled(),
                );
                return IRExpr {
                    kind: kind,
                    expr_type: expr_type,
                };
            }
        }

        IRExpr {
            kind: IRExprKind::Variable(symbol.mangled()),
            expr_type: expr_type,
        }
    }

    fn visit_array_expr(&mut self, expr: &Expr, elements: &[Expr]) -> Self::ExprResult {
        let elements: Vec<_> = elements.iter().map(|e| e.accept(self)).collect();
        IRExpr {
            kind: IRExprKind::Array(elements),
            expr_type: self.get_expr_type(expr),
        }
    }

    fn visit_subscript_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        index: &Expr,
    ) -> Self::ExprResult {
        let array_count = self.array_count(target);
        let array_count = array_count.to_string();

        let index = index.accept(self);
        let index = self.writer.declare_temp(index);

        let guard = IRExpr {
            kind: IRExprKind::Binary(
                Box::new(IRExpr::variable(&index)),
                IRBinaryOperator::GreaterEqual,
                Box::new(IRExpr::int_literal(&array_count)),
            ),
            expr_type: NodeType::Bool,
        };

        self.writer
            .write_guard(guard, "index out of bounds", &expr.span);
        let kind = IRExprKind::Subscript(
            Box::new(target.accept(self)),
            Box::new(IRExpr::variable(&index)),
        );
        IRExpr {
            kind,
            expr_type: self.get_expr_type(expr),
        }
    }

    fn visit_cast_expr(
        &mut self,
        expr: &Expr,
        _explicit_type: &ExplicitType,
        value: &Expr,
    ) -> Self::ExprResult {
        IRExpr {
            kind: IRExprKind::Cast(Box::new(value.accept(self))),
            expr_type: self.get_expr_type(expr),
        }
    }
}

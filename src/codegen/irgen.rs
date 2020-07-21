use super::builtins;
use super::ir::*;
use super::irwriter::IRWriter;
use super::specialize::{SpecializationRecorder, SpecializationRecord};
use super::memory;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use log::trace;
use std::rc::Rc;

pub struct IRGen {
    lib: Rc<Lib>,
    all_symbols: SymbolStore,
    writer: IRWriter,
    current_type: Option<TypeMetadata>,
}

impl IRGen {
    pub fn new(lib: Lib, all_symbols: SymbolStore) -> Self {
        let lib = Rc::new(lib);
        let writer = IRWriter::new(Rc::clone(&lib), all_symbols.clone());
        IRGen {
            lib,
            all_symbols,
            writer,
            current_type: None,
        }
    }

    pub fn generate(mut self, lib_symbols: Rc<SymbolTable>) -> Module {
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

        let IRWriter {
            lib: lib_copy,
            structures,
            functions,
            ..
        } = self.writer;

        std::mem::drop(lib);
        std::mem::drop(lib_copy);

        let lib = Rc::try_unwrap(self.lib).ok().unwrap();

        let mut new = Module {
            name: lib.name,
            structures,
            functions,
            symbols: lib_symbols,
            specialization_record: SpecializationRecord::new(),
        };

        for func in &mut new.functions {
            let mut writer = memory::FreeWriter::new(func);
            writer.write();
        }

        SpecializationRecorder::record(&mut new);

        new
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

        let type_metadata = self.all_symbols.type_metadata(&type_symbol).unwrap();

        self.writer.declare_struct(&type_metadata);

        builtins::write_type_init(&mut self.writer, &type_metadata);
        builtins::write_type_deinit(
            &mut self.writer,
            &type_metadata,
        );

        trace!("Writing methods for {}", type_symbol);

        self.current_type = Some(type_metadata.clone());
        self.gen_function_decls(&decl.methods);
        self.gen_function_decls(&decl.meta_methods);
        self.current_type = None;

        trace!("Finished methods for {}", type_symbol);
    }

    fn gen_function_decl(&mut self, decl: &FunctionDecl) {
        let func_symbol = decl.name.get_symbol().unwrap();

        let mut func_metadata = self.all_symbols.function_metadata(&func_symbol).unwrap().clone();

        if decl.include_caller {
            func_metadata.parameters.push(VarMetadata {
                name: String::from("caller"),
                var_type: NodeType::pointer_to(NodeType::Byte),
                public: false,
            });
        }

        if decl.is_builtin {
            builtins::write_special_function(&mut self.writer, &func_metadata)
        } else {
            self.writer.start_block();
            self.gen_stmts(&decl.body);
            self.writer.end_decl_func(&func_metadata);
        }
    }

    fn conformance_decl(&mut self, decl: &ConformanceDecl) {
        let type_symbol = decl.target.get_symbol().unwrap();
        let type_metadata = self.all_symbols.type_metadata(&type_symbol).unwrap();

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

    fn visit_conformance_condition_stmt(
        &mut self,
        type_name: &SymbolicToken,
        trait_name: &SymbolicToken,
        body: &[Stmt],
    ) {
        self.writer.start_block();
        self.gen_stmts(body);
        self
            .writer
            .end_conformance_check(type_name.get_symbol().unwrap(), trait_name.get_symbol().unwrap())
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

    fn visit_for_stmt(&mut self, variable: &SymbolicToken, iter_expr: &Expr, body: &[Stmt]) {
        let counter = self.writer.declare_temp_no_init(NodeType::Int);

        let iterator = iter_expr.accept(self);
        let iterator_type = iter_expr.get_type().unwrap();

        let limit = match &iterator_type {
            NodeType::Instance(symbol, _) if symbol == &Symbol::stdlib("Range") => {
                let start = IRExpr {
                    kind: IRExprKind::FieldAccess(
                        Box::new(iterator.clone()),
                        symbol.child("start").mangled(),
                    ),
                    expr_type: NodeType::Int,
                };
                self.writer.assign(IRExpr::variable(&counter), start);
                IRExpr {
                    kind: IRExprKind::FieldAccess(
                        Box::new(iterator.clone()),
                        symbol.child("end").mangled(),
                    ),
                    expr_type: NodeType::Int,
                }
            }
            NodeType::Instance(symbol, _) if symbol == &Symbol::stdlib("Vec") => {
                self.writer
                    .assign(IRExpr::variable(&counter), IRExpr::int_literal("0"));
                IRExpr {
                    kind: IRExprKind::FieldAccess(
                        Box::new(iterator.clone()),
                        symbol.child("count").mangled(),
                    ),
                    expr_type: NodeType::Int,
                }
            }
            NodeType::Array(_, count) => {
                self.writer
                    .assign(IRExpr::variable(&counter), IRExpr::int_literal("0"));
                IRExpr::int_literal(&count.to_string())
            }
            _ => unimplemented!(),
        };

        self.writer.start_block();

        self.writer.start_block();
        self.writer.break_loop();
        self.writer.end_if_block(IRExpr {
            kind: IRExprKind::Binary(
                Box::new(IRExpr::variable(&counter)),
                IRBinaryOperator::GreaterEqual,
                Box::new(limit),
            ),
            expr_type: NodeType::Bool,
        });

        match iterator_type {
            NodeType::Instance(symbol, _) if symbol == Symbol::stdlib("Range") => {
                let local = self
                    .writer
                    .declare_local(variable.get_symbol().unwrap(), NodeType::Int);

                self.writer
                    .assign(IRExpr::variable(&local), IRExpr::variable(&counter));
            }
            NodeType::Instance(symbol, spec) if symbol == Symbol::stdlib("Vec") => {
                let element_ty = spec.type_for(&symbol.child("T")).unwrap().clone();
                let element_ty = NodeType::pointer_to(element_ty);

                let local = self
                    .writer
                    .declare_local(variable.get_symbol().unwrap(), element_ty.clone());

                let storage = IRExpr {
                    kind: IRExprKind::FieldAccess(
                        Box::new(iterator.clone()),
                        symbol.child("storage").mangled(),
                    ),
                    expr_type: element_ty.clone(),
                };

                self.writer.assign(
                    IRExpr::variable(&local),
                    IRExpr {
                        kind: IRExprKind::Binary(
                            Box::new(storage),
                            IRBinaryOperator::Plus,
                            Box::new(IRExpr::variable(&counter)),
                        ),
                        expr_type: element_ty,
                    },
                );
            }
            NodeType::Array(element_ty, _) => {
                let element_ty = NodeType::pointer_to(*element_ty);

                let local = self
                    .writer
                    .declare_local(variable.get_symbol().unwrap(), element_ty.clone());

                self.writer.assign(
                    IRExpr::variable(&local),
                    IRExpr {
                        kind: IRExprKind::Binary(
                            Box::new(iterator),
                            IRBinaryOperator::Plus,
                            Box::new(IRExpr::variable(&counter)),
                        ),
                        expr_type: element_ty,
                    },
                );
            }
            _ => unimplemented!(),
        };

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
            TokenKind::Ampersand => {
                return self.writer.addres_of_expr(operand);
            }
            TokenKind::Star => {
                if let IRExprKind::Unary(IRUnaryOperator::Reference, _) = &operand.kind {
                    return operand;
                }
                IRUnaryOperator::Dereference
            }
            tk => panic!("Illegal operator {:?}", tk),
        };
        IRExpr {
            kind: IRExprKind::Unary(op_type, Box::new(operand)),
            expr_type: self.get_expr_type(expr),
        }
    }

    fn visit_function_call_expr(&mut self, expr: &Expr, call: &FunctionCall) -> Self::ExprResult {
        let mut arg_exprs: Vec<_> = call.arguments.iter().map(|a| a.accept(self)).collect();

        let function_symbol = call.name.get_symbol().unwrap();
        let mut ir_symbol = function_symbol.clone();

        trace!("Writing call {}", function_symbol);

        let function_metadata = self.all_symbols.function_metadata(&function_symbol).unwrap();

        let specialization = call.get_specialization().unwrap();

        if let FunctionKind::Method(..) = function_metadata.kind {
            if let Some(target) = &call.target {
                let target_expr = target.accept(self);

                let possible_trait = function_symbol.owner_symbol().and_then(|o| self.all_symbols.trait_metadata_symbol(&o));
                if possible_trait.is_some() {
                    if let NodeType::Instance(sym, _) = &target_expr.expr_type {
                        ir_symbol = sym.child(function_symbol.name());
                    }
                }

                let target_expr = self.writer.addres_of_expr(target_expr);
                arg_exprs.insert(0, target_expr);
            } else {
                let current_type = self.current_type.as_ref().unwrap();
                arg_exprs.insert(
                    0,
                    IRExpr::variable(&IRVariable::self_var(current_type)),
                );
            }
        }

        let function_metadata = self.all_symbols.function_metadata(&function_symbol).unwrap();
        if function_metadata.include_caller {
            let location = expr.span.location();
            arg_exprs.push(IRExpr::string_literal(&location));
        }

        if builtins::is_direct_c_binding(&function_symbol) {
            IRExpr::call_extern(function_symbol.name(), arg_exprs, self.get_expr_type(expr))
        } else if builtins::can_write_special_call(&function_symbol) {
            builtins::write_special_call(
                &mut self.writer,
                &function_symbol,
                &specialization,
                arg_exprs,
            )
        } else {
            IRExpr::call(
                ir_symbol,
                specialization,
                arg_exprs,
                self.get_expr_type(expr),
            )
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

        IRExpr {
            kind: IRExprKind::FieldAccess(Box::new(target_expr), field_symbol.mangled()),
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
            let self_expr = IRExpr::variable(&IRVariable::self_var(current_type));
            if current_type.symbol.self_symbol() == symbol {
                return self_expr;
            } else if current_type.symbol.directly_owns(&symbol) {
                let kind = IRExprKind::DerefFieldAccess(
                    Box::new(self_expr),
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
        let array = self.writer.declare_temp_no_init(expr.get_type().unwrap());

        for (index, element) in elements.iter().enumerate() {
            let element = element.accept(self);
            let value = self.writer.declare_temp(element);

            let offset = IRExpr::binary(
                IRExpr::variable(&array),
                IRBinaryOperator::Plus,
                IRExpr::int_literal(&index.to_string()),
                array.var_type.clone(),
            );
            let deref = IRExpr {
                kind: IRExprKind::Unary(IRUnaryOperator::Dereference, Box::new(offset)),
                expr_type: NodeType::Ambiguous,
            };
            self.writer.assign(deref, IRExpr::variable(&value));
        }

        IRExpr::variable(&array)
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
        let array = target.accept(self);
        let array_type = array.expr_type.coerce_array_to_ptr();
        let add = IRExpr::binary(
            array,
            IRBinaryOperator::Plus,
            IRExpr::variable(&index),
            array_type,
        );
        let kind = IRExprKind::Unary(IRUnaryOperator::Dereference, Box::new(add));
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

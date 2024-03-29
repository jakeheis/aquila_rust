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
    all_symbols: SymbolStore,
    writer: IRWriter,
    current_type: Option<TypeMetadata>,
}

impl IRGen {
    pub fn new(all_symbols: SymbolStore) -> Self {
        let writer = IRWriter::new(all_symbols.clone());
        IRGen {
            all_symbols,
            writer,
            current_type: None,
        }
    }

    pub fn generate(mut self, module: ParsedModule, module_symbols: Rc<SymbolTable>) -> Module {
        for t in &module.type_decls {
            self.visit_type_decl(t);
        }
        self.gen_function_decls(&module.function_decls);
        for c in &module.conformance_decls {
            self.conformance_decl(c);
        }

        if !module.main.is_empty() {
            trace!("Gen IR for main");
            self.writer.start_block();
            for s in &module.main {
                s.accept(&mut self);
            }
            self.writer.return_value(IRExpr::int_literal("0"));
            self.writer.end_decl_main(Symbol::main_symbol(&module.name));
        }

        let IRWriter {
            structures,
            functions,
            ..
        } = self.writer;

        let mut new = Module {
            name: module.name,
            structures,
            functions,
            symbols: module_symbols,
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
        builtins::write_type_name_func(&mut self.writer, &type_metadata);

        trace!("Writing methods for {}", type_symbol);

        self.current_type = Some(type_metadata.clone());
        self.gen_function_decls(&decl.methods);
        self.gen_function_decls(&decl.meta_methods);
        self.current_type = None;

        trace!("Finished methods for {}", type_symbol);
    }

    fn gen_function_decl(&mut self, decl: &FunctionDecl) {
        let func_symbol = decl.name.get_symbol().unwrap();

        trace!("Gen IR for func {}", func_symbol);

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

    fn visit_conditional_compilation_stmt(
        &mut self,
        condition: &CompilerCondition,
        body: &[Stmt],
    ) {
        self.writer.start_block();
        self.gen_stmts(body);

        match condition {
            CompilerCondition::Conformance(gen_token, trait_token) =>
                self
                    .writer
                    .end_conformance_check(gen_token.get_symbol().unwrap(), trait_token.get_symbol().unwrap()),
            CompilerCondition::Equality(gen_token, _, resolved_type) => {
                let resolved_type = resolved_type.borrow().as_ref().unwrap().clone();
                self
                    .writer
                    .end_type_equality_check(gen_token.get_symbol().unwrap(), resolved_type);
            }
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

    fn visit_for_stmt(&mut self, variable: &SymbolicToken, iter_expr: &Expr, body: &[Stmt]) {
        let counter = self.writer.declare_temp_no_init(NodeType::Int);

        let iter_expr = iter_expr.accept(self);
        let iterator_type = iter_expr.expr_type.clone();
        let iter_expr = IRExpr {
            kind: iter_expr.kind,
            expr_type: iterator_type.coerce_array_to_ptr(),
        };
        let iterator = self.writer.declare_temp(iter_expr);

        let limit = match &iterator_type {
            NodeType::Reference(to) => {
                match to.as_ref() {
                    NodeType::Instance(symbol, _) if symbol == &Symbol::stdlib("Range") => {
                        let start = IRExpr {
                            kind: IRExprKind::DerefFieldAccess(
                                Box::new(IRExpr::variable(&iterator)),
                                symbol.child("start").mangled(),
                            ),
                            expr_type: NodeType::Int,
                        };
                        self.writer.assign(IRExpr::variable(&counter), start);
                        IRExpr {
                            kind: IRExprKind::DerefFieldAccess(
                                Box::new(IRExpr::variable(&iterator)),
                                symbol.child("end").mangled(),
                            ),
                            expr_type: NodeType::Int,
                        }
                    }
                    NodeType::Instance(symbol, _) if symbol == &Symbol::stdlib("Vec") => {
                        self.writer
                            .assign(IRExpr::variable(&counter), IRExpr::int_literal("0"));
                        IRExpr {
                            kind: IRExprKind::DerefFieldAccess(
                                Box::new(IRExpr::variable(&iterator)),
                                symbol.child("count").mangled(),
                            ),
                            expr_type: NodeType::Int,
                        }
                    },
                    ty => unimplemented!("can't iterate over {}", ty)
                }
            }
            NodeType::Array(_, count) => {
                self.writer
                    .assign(IRExpr::variable(&counter), IRExpr::int_literal("0"));
                IRExpr::int_literal(&count.to_string())
            }
            ty => unimplemented!("can't iterate over {}", ty),
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
            NodeType::Reference(to) => {
                match to.as_ref() {
                    NodeType::Instance(symbol, _) if symbol == &Symbol::stdlib("Range") => {
                        let local = self
                            .writer
                            .declare_local(variable.get_symbol().unwrap(), NodeType::Int);

                        self.writer
                            .assign(IRExpr::variable(&local), IRExpr::variable(&counter));
                    }
                    NodeType::Instance(symbol, spec) if symbol == &Symbol::stdlib("Vec") => {
                        let element_ty = spec.type_for(&symbol.child("T")).unwrap().clone();
                        let element_ty = NodeType::pointer_to(element_ty);

                        let local = self
                            .writer
                            .declare_local(variable.get_symbol().unwrap(), element_ty.clone());

                        let storage = IRExpr {
                            kind: IRExprKind::DerefFieldAccess(
                                Box::new(IRExpr::variable(&iterator)),
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
                    },
                    _ => unimplemented!()
                }
            },
            NodeType::Array(element_ty, _) => {
                let element_ty = NodeType::pointer_to(*element_ty);

                let local = self
                    .writer
                    .declare_local(variable.get_symbol().unwrap(), element_ty.clone());

                self.writer.assign(
                    IRExpr::variable(&local),
                    IRExpr {
                        kind: IRExprKind::Binary(
                            Box::new(IRExpr::variable(&iterator)),
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

        let adddress_type = NodeType::pointer_to(target.expr_type.clone());
        let address = self.writer.declare_temp(IRExpr {
            kind: IRExprKind::Unary(IRUnaryOperator::Reference, Box::new(target)),
            expr_type: adddress_type,
        });

        // Declare old value as var so memory.rs cleans it up
        self.writer.declare_temp(IRExpr::dereference(&address));

        let value = value.accept(self);
        self.writer.assign(IRExpr::dereference(&address), value);
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
                return self.writer.addres_of_expr(operand, false);
            }
            TokenKind::At => {
                return self.writer.addres_of_expr(operand, true);
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
        let mut arg_exprs: Vec<_> = Vec::new();
        for arg in &call.arguments {
            let mut arg = arg.accept(self);
            if !arg.has_defined_location() {
                match &arg.expr_type {
                    NodeType::Instance(..) | NodeType::GenericInstance(..) => {
                        let var = self.writer.declare_temp(arg);
                        arg = IRExpr::variable(&var);
                    }
                    _ => ()
                }
            }
            arg_exprs.push(arg);
        }

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
                    if let NodeType::GenericInstance(sym) = &target_expr.expr_type {
                        ir_symbol = sym.child(function_symbol.name());
                    } else if let NodeType::Reference(to) = &target_expr.expr_type {
                        if let NodeType::GenericInstance(sym) = to.as_ref() {
                            ir_symbol = sym.child(function_symbol.name());
                        } else {
                            panic!()
                        }
                    } else {
                        panic!()
                    }
                }

                let target_expr = if let NodeType::Reference(..) = target_expr.expr_type {
                    target_expr
                } else {
                    self.writer.addres_of_expr(target_expr, false)
                };
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

        let kind = if let NodeType::Reference(..) = &target_expr.expr_type {
            IRExprKind::DerefFieldAccess(Box::new(target_expr), field_symbol.mangled())
        } else {
            IRExprKind::FieldAccess(Box::new(target_expr), field_symbol.mangled())
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

        match &nonspec_expr_type {
            NodeType::Metatype(..) => {
                return IRExpr {
                    kind: IRExprKind::ExplicitType,
                    expr_type: nonspec_expr_type,
                };
            },
            _ => (),
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
        let (element_ty, size) = if let NodeType::Array(ty, size) = expr.get_type().unwrap() {
            (ty, size)
        } else {
            panic!()
        };
        let size = size.to_string();
        let array = self.writer.declare_array(*element_ty, IRExpr::int_literal(&size));

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

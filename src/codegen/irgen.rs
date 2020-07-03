use super::builtins;
use super::ir::*;
use super::irwriter::IRWriter;
use crate::analysis::FinalSpecializationMap;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use log::trace;
use std::rc::Rc;

pub struct IRGen {
    lib: Rc<Lib>,
    spec_map: Rc<FinalSpecializationMap>,
    writer: IRWriter,
    current_type: Option<TypeMetadata>,
    func_specialization: Option<GenericSpecialization>,
}

impl IRGen {
    pub fn new(lib: Rc<Lib>, spec_map: Rc<FinalSpecializationMap>) -> Self {
        let lib_copy = Rc::clone(&lib);
        IRGen {
            lib,
            spec_map,
            writer: IRWriter::new(lib_copy),
            current_type: None,
            func_specialization: None,
        }
    }

    pub fn generate(mut self) -> IRProgram {
        let lib = Rc::clone(&self.lib);

        for t in &lib.type_decls {
            self.visit_type_decl(t);
        }
        for f in &lib.function_decls {
            self.visit_function_decl(f);
        }

        if !lib.main.is_empty() {
            self.writer.start_block();
            self.func_specialization = Some(GenericSpecialization::empty());
            for s in &lib.main {
                s.accept(&mut self);
            }
            self.writer.return_value(IRExpr::int_literal("0"));
            self.writer.end_decl_main();
        }

        self.writer.program
    }

    fn gen_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            stmt.accept(self);
        }
    }

    fn gen_function_decls(&mut self, decls: &[FunctionDecl]) {
        for decl in decls {
            self.visit_function_decl(decl);
        }
    }

    fn self_type(&self) -> Option<NodeType> {
        let ct = self.current_type.as_ref();
        let fs = self.func_specialization.as_ref();
        if let (Some(current_type), Some(func_spec)) = (ct, fs) {
            let spec = func_spec.subset(&current_type.symbol);
            Some(NodeType::Instance(current_type.symbol.clone(), spec))
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

    fn specialize_expr(&self, expr: &Expr) -> NodeType {
        expr.get_type()
            .unwrap()
            .specialize_opt(self.lib.as_ref(), self.func_specialization.as_ref())
    }

    fn visit_type_decl(&mut self, decl: &TypeDecl) {
        let type_symbol = decl.name.get_symbol().unwrap();
        let specs = self.spec_map.specs_for(&type_symbol);

        let type_metadata = self.lib.type_metadata_ref(&type_symbol).unwrap();

        if let Some(specs) = specs {
            for spec in specs {
                trace!("Writing type {} with specialization {}", type_symbol, spec);

                self.writer.declare_struct(&type_metadata, spec);

                let meta_symbol = Symbol::meta_symbol(&type_symbol);
                let init_symbol = Symbol::init_symbol(&meta_symbol);
                let init_metadata = self.lib.function_metadata(&init_symbol).unwrap();

                let instance_type = init_metadata
                    .return_type
                    .specialize(self.lib.as_ref(), &spec);

                let new_item = IRVariable::new("new_item", instance_type.clone());

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
                self.writer.end_decl_func(&init_metadata, &spec);

                trace!("Finished type {} with specialization {}", type_symbol, spec);
            }
        }

        trace!("Writing methods for {}", type_symbol);

        self.current_type = Some(type_metadata.clone());
        self.gen_function_decls(&decl.methods);
        self.gen_function_decls(&decl.meta_methods);
        self.current_type = None;

        trace!(target: "codegen", "Finished methods for {}", type_symbol);
    }

    fn visit_function_decl(&mut self, decl: &FunctionDecl) {
        let func_symbol = decl.name.get_symbol().unwrap();

        let spec_map = Rc::clone(&self.spec_map);
        let specs = spec_map.specs_for(&func_symbol);

        let func_metadata = self.lib.function_metadata(&func_symbol).unwrap().clone();

        if let Some(specs) = specs {
            for specialization in specs {
                trace!(
                    "Writing function {} with specialization {}",
                    func_symbol,
                    specialization
                );

                self.func_specialization = Some(specialization.clone());

                if decl.is_builtin {
                    builtins::write_special_function(
                        &mut self.writer,
                        &func_metadata,
                        specialization,
                    )
                } else {
                    self.writer.start_block();
                    self.gen_stmts(&decl.body);
                    self.writer.end_decl_func(&func_metadata, specialization);
                }

                trace!(target: "codegen", "Finished function {} with specialization {}", func_symbol, specialization);

                self.func_specialization = None;
            }
        }
    }
}

impl StmtVisitor for IRGen {
    type StmtResult = ();

    fn visit_local_variable_decl(&mut self, decl: &LocalVariableDecl) -> Self::StmtResult {
        let var_symbol = decl.name.get_symbol().unwrap();
        let mut var_type = decl.name.get_type().unwrap();

        if let NodeType::Array(of, _) = var_type {
            var_type = NodeType::Pointer(of.clone());
        }

        let var_type =
            var_type.specialize_opt(self.lib.as_ref(), self.func_specialization.as_ref());

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

    fn visit_for_stmt(&mut self, variable: &TypedToken, array_expr: &Expr, body: &[Stmt]) {
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

        let var_type = variable.get_type().unwrap();
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

    fn visit_print_stmt(&mut self, expr: &Option<Expr>) -> Self::StmtResult {
        if let Some(expr) = expr.as_ref() {
            let node_type = expr.get_type().unwrap();
            if let NodeType::Instance(type_symbol, spec) = node_type {
                let full_symbol = Symbol::new_str(&type_symbol, "write");
                let metadata = self.lib.function_metadata(&full_symbol).unwrap();
                let function_name = metadata.function_name(&self.lib, &spec);

                self.writer
                    .expr(IRExpr::call(&function_name, Vec::new(), NodeType::Void));
            } else {
                let format_specificer = match node_type {
                    NodeType::Int | NodeType::Bool => "%i",
                    NodeType::Double => "%f",
                    node_type if node_type.is_pointer_to(NodeType::Byte) => "%s",
                    _ => unreachable!(),
                };

                let format_line = format!("\"{}\\n\"", format_specificer);
                let format_expr = IRExpr {
                    kind: IRExprKind::Literal(format_line),
                    expr_type: NodeType::pointer_to(NodeType::Byte),
                };
                let expr = expr.accept(self);
                self.writer.expr(IRExpr::call(
                    "printf",
                    vec![format_expr, expr],
                    NodeType::Void,
                ));
            }
        } else {
            let empty_line = IRExpr {
                kind: IRExprKind::Literal(String::from("\"\\n\"")),
                expr_type: NodeType::pointer_to(NodeType::Byte),
            };
            self.writer
                .expr(IRExpr::call("printf", vec![empty_line], NodeType::Void));
        }
    }

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Self::StmtResult {
        if let ExprKind::Assignment(target, value) = &expr.kind {
            let target = target.accept(self);
            let value = value.accept(self);
            self.writer.assign(target, value);
        } else {
            let expr = expr.accept(self);
            self.writer.expr(expr);
        }
    }
}

impl ExprVisitor for IRGen {
    type ExprResult = IRExpr;

    fn visit_assignment_expr(
        &mut self,
        _expr: &Expr,
        _target: &Expr,
        _value: &Expr,
    ) -> Self::ExprResult {
        unreachable!()
    }

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
            expr_type: self.specialize_expr(expr),
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
            expr_type: self.specialize_expr(expr),
        }
    }

    fn visit_function_call_expr(
        &mut self,
        expr: &Expr,
        target: Option<&Expr>,
        function: &ResolvedToken,
        args: &[Expr],
    ) -> Self::ExprResult {
        let mut arg_exprs: Vec<_> = args.iter().map(|a| a.accept(self)).collect();

        let function_symbol = function.get_symbol().unwrap();
        let function_metadata = self.lib.function_metadata(&function_symbol).unwrap();

        trace!("Writing call to {}", function_symbol);

        let mut specialization = if function.specialization.is_empty() {
            let arg_types: Vec<_> = args.iter().map(|a| a.get_type().unwrap()).collect();
            GenericSpecialization::infer(self.lib.as_ref(), &function_metadata, &arg_types)
                .ok()
                .unwrap()
        } else {
            let explicit_types: Vec<_> = function
                .specialization
                .iter()
                .map(|s| s.guarantee_resolved())
                .collect();

            match &function_metadata.kind {
                FunctionKind::MetaMethod(owner) if function_symbol.is_init() => {
                    let type_init = self.lib.type_metadata_ref(&owner).unwrap();
                    GenericSpecialization::new(&type_init.generics, explicit_types)
                }
                _ => GenericSpecialization::new(&function_metadata.generics, explicit_types),
            }
        };

        if let Some(target) = target {
            match target.get_type().unwrap() {
                NodeType::Instance(_, specs) | NodeType::Metatype(_, specs) => {
                    specialization = specialization.merge(self.lib.as_ref(), &specs);
                }
                _ => (),
            }
        }

        if let Some(caller_specs) = self.func_specialization.as_ref() {
            specialization = specialization.merge(self.lib.as_ref(), caller_specs)
        }

        let function_name = if builtins::is_direct_c_binding(&function_symbol) {
            String::from(function_symbol.last_component())
        } else {
            function_metadata.function_name(self.lib.as_ref(), &specialization)
        };

        if let Some(target) = target {
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

        if let Some(special) =
            builtins::write_special_call(&function_symbol, &arg_exprs, &specialization)
        {
            special
        } else {
            IRExpr {
                kind: IRExprKind::Call(function_name, arg_exprs),
                expr_type: self.specialize_expr(expr),
            }
        }
    }

    fn visit_field_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        field: &ResolvedToken,
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
            expr_type: self.specialize_expr(expr),
        }
    }

    fn visit_literal_expr(&mut self, expr: &Expr, token: &Token) -> Self::ExprResult {
        IRExpr {
            kind: IRExprKind::Literal(token.lexeme().to_string()),
            expr_type: self.specialize_expr(expr),
        }
    }

    fn visit_variable_expr(&mut self, expr: &Expr, name: &ResolvedToken) -> Self::ExprResult {
        let nonspec_expr_type = expr.get_type().unwrap();

        if let NodeType::Metatype(symbol, _) = &nonspec_expr_type {
            if let Some(spec) = self.func_specialization.as_ref() {
                if spec.map.contains_key(&symbol) {
                    let spec_type = nonspec_expr_type.specialize(self.lib.as_ref(), spec);
                    return IRExpr {
                        kind: IRExprKind::ExplicitType,
                        expr_type: spec_type,
                    };
                }
            }
        }

        let expr_type = self.specialize_expr(expr);
        let symbol = name.get_symbol().unwrap();

        if let Some(current_type) = self.current_type.as_ref() {
            let spec = self
                .func_specialization
                .as_ref()
                .unwrap()
                .subset(&current_type.symbol);
            let self_expr = IRExpr {
                kind: IRExprKind::Variable(String::from("self")),
                expr_type: NodeType::Instance(current_type.symbol.clone(), spec),
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
            expr_type: self.specialize_expr(expr),
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
            expr_type: self.specialize_expr(expr),
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
            expr_type: self.specialize_expr(expr),
        }
    }
}

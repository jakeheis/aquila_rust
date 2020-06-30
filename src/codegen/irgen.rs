use super::ir::*;
use crate::library::*;
use std::rc::Rc;
use crate::parsing::*;
use crate::lexing::*;
use super::core;
use log::trace;

pub struct IRGen {
    lib: Rc<Lib>,
    writer: IRWriter,
    current_type: Option<TypeMetadata>,
    func_specialization: Option<GenericSpecialization>,
}

impl IRGen {
    pub fn new(lib: Rc<Lib>) -> Self {
        let lib_copy = Rc::clone(&lib);
        IRGen {
            lib,
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

        self.writer.start_block();
        for s in &lib.other {
            s.accept(&mut self);
        }
        self.writer.end_decl_main();

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
        if let (Some(current_type), Some(func_spec)) = (ct, fs)  {
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
}

impl StmtVisitor for IRGen {
    type StmtResult = ();

    fn visit_type_decl(&mut self, decl: &TypeDecl) -> Self::StmtResult {
        let type_symbol = decl.name.get_symbol().unwrap();
        let type_metadata = self.lib.type_metadata(&type_symbol).unwrap();

        for spec in &type_metadata.specializations {
            trace!("Writing type {} with specialization {}", type_symbol, spec);

            self.writer.declare_struct(&type_metadata, spec);

            let meta_symbol = Symbol::meta_symbol(&type_symbol);
            let init_symbol = Symbol::init_symbol(&meta_symbol);
            let init_metadata = self.lib.function_metadata(&init_symbol).unwrap();

            let instance_type = init_metadata
            .return_type
            .specialize(self.lib.as_ref(), &spec);

            self.writer.start_block();
            self.writer.declare_local_str(
                "new_item",
                instance_type.clone(),
            );
            
            for (field, field_type) in type_metadata.field_symbols.iter().zip(&type_metadata.field_types) {
                let v = IRExpr {
                    kind: IRExprKind::Variable(String::from("new_item")),
                    expr_type: instance_type.clone()
                };
                self.writer.assign_field(v, &field.mangled(), IRExpr {
                    kind: IRExprKind::Variable(field.mangled()),
                    expr_type: field_type.clone()
                });
            }
            self.writer.return_value(Some(IRExpr {
                kind: IRExprKind::Variable(String::from("new_item")),
                expr_type: instance_type
            }));
            self.writer.end_decl_func(&init_metadata, &spec);

            trace!("Finished type {} with specialization {}", type_symbol, spec);
        }

        trace!("Writing methods for {}", type_symbol);

        self.current_type = Some(type_metadata.clone());
        self.gen_function_decls(&decl.methods);
        self.gen_function_decls(&decl.meta_methods);
        self.current_type = None;

        trace!(target: "codegen", "Finished methods for {}", type_symbol);
    }

    fn visit_function_decl(&mut self, decl: &FunctionDecl) -> Self::StmtResult {
        let func_symbol = decl.name.get_symbol().unwrap();
        if core::is_direct_c_binding(&func_symbol) {
            return;
        }

        let func_metadata = self.lib.function_metadata(&func_symbol).unwrap();

        for specialization in &func_metadata.specializations {        
            trace!("Writing function {} with specialization {}", func_symbol, specialization);
    
            self.func_specialization = Some(specialization.clone());
    
            self.writer.start_block();
            if false {
                core::write(&func_symbol, &mut self.writer);
            } else {
                self.gen_stmts(&decl.body);
            }
            self.writer.end_decl_func(&func_metadata, specialization);

            trace!(target: "codegen", "Finished function {} with specialization {}", func_symbol, specialization);
    
            self.func_specialization = None;
        }
    }

    fn visit_variable_decl(&mut self, decl: &VariableDecl) -> Self::StmtResult {
        let var_symbol = decl.name.get_symbol().unwrap();
        let mut var_type = decl.name.get_type().unwrap();

        if let NodeType::Array(of, _) = var_type {
            var_type = NodeType::Pointer(of.clone());
        }

        let var_type = var_type.specialize_opt(self.lib.as_ref(), self.func_specialization.as_ref());

        let local = self.writer.declare_local(var_symbol, var_type);

        if let Some(init_value) = decl.initial_value.as_ref() {
            let init_value = init_value.accept(self);
            self.writer.assign_local(&local, init_value);
        }
    }

    fn visit_trait_decl(&mut self, _decl: &TraitDecl) {}

    fn visit_conformance_decl(&mut self, _decl: &ConformanceDecl)  {

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
            kind: IRExprKind::Unary(String::from("!"), Box::new(condition)),
            expr_type: NodeType::Bool
        });

        self.gen_stmts(body);

        self.writer.end_loop();
    }

    fn visit_for_stmt(&mut self, variable: &TypedToken, array_expr: &Expr, body: &[Stmt]) {
        let array = array_expr.accept(self);
        let limit = self.array_count(array_expr);

        let counter = String::from("i");
        self.writer.declare_local_str(&counter, NodeType::Int);
        
        self.writer.start_block();

        self.writer.start_block();
        self.writer.break_loop();
        self.writer.end_if_block(IRExpr {
            kind: IRExprKind::Binary(
                Box::new(IRExpr {
                    kind: IRExprKind::Variable(counter.clone()),
                    expr_type: NodeType::Int
                }),
                String::from(">="),
                Box::new(IRExpr {
                    kind: IRExprKind::Literal(limit.to_string()),
                    expr_type: NodeType::Int
                }),
            ),
            expr_type: NodeType::Bool
        });

        let var_type = variable.get_type().unwrap();
        let local = self.writer.declare_local(variable.get_symbol().unwrap(), NodeType::pointer_to(var_type.clone()));
        let indexed = IRExpr {
            kind: IRExprKind::Subscript(Box::new(array), Box::new(IRExpr {
                kind: IRExprKind::Variable(counter.clone()),
                expr_type: NodeType::Int
            })),
            expr_type: var_type.clone()
        };
        self.writer.assign_local(&local, IRExpr {
            kind: IRExprKind::Unary(String::from("&"), Box::new(indexed)),
            expr_type: var_type
        });

        self.gen_stmts(body);
        self.writer.assign_local(&counter, IRExpr {
            kind: IRExprKind::Binary(
                Box::new(IRExpr {
                    kind: IRExprKind::Variable(counter.clone()),
                    expr_type: NodeType::Int,
                }),
                String::from("+"),
                Box::new(IRExpr {
                    kind: IRExprKind::Literal(String::from("1")),
                    expr_type: NodeType::Int,
                })
            ),
            expr_type: NodeType::Int
        });
        self.writer.end_loop();
    }

    fn visit_return_stmt(&mut self, _stmt: &Stmt, expr: &Option<Expr>) -> Self::StmtResult {
        let ret = expr.as_ref().map(|e| e.accept(self));
        self.writer.return_value(ret);
        // let val = expr.as_ref().map(|e| e.accept(self));
        // self.writer.write_return(val)
    }

    fn visit_print_stmt(&mut self, expr: &Option<Expr>) -> Self::StmtResult {
        let function = String::from("printf");

        if let Some(expr) = expr.as_ref() {
            let node_type = expr.get_type().unwrap();
            if let NodeType::Instance(type_symbol, spec) = node_type {
                let full_symbol = Symbol::new_str(&type_symbol, "write");
                let metadata = self.lib.function_metadata(&full_symbol).unwrap();
                let function_name = metadata.function_name(&self.lib, &spec);

                self.writer.expr(IRExpr {
                    kind: IRExprKind::Call(function_name, Vec::new()),
                    expr_type: NodeType::Void,
                });
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
                    expr_type: NodeType::pointer_to(NodeType::Byte)
                };
                let expr = expr.accept(self);
                self.writer.expr(IRExpr {
                    kind: IRExprKind::Call(function, vec![format_expr, expr]),
                    expr_type: NodeType::Void,
                });
            }
        } else {
            let empty_line = IRExpr {
                kind: IRExprKind::Literal(String::from("\"\\n\"")),
                expr_type: NodeType::pointer_to(NodeType::Byte)
            };
            self.writer.expr(IRExpr {
                kind: IRExprKind::Call(function, vec![empty_line]),
                expr_type: NodeType::Void,
            });
        }
    }

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Self::StmtResult {
        if let ExprKind::Assignment(target, value) = &expr.kind {
            let value = value.accept(self);
            match &target.kind {
                ExprKind::Variable(name) => {
                    let local_name = name.get_symbol().unwrap().mangled();
                    self.writer.assign_local(&local_name, value);
                },
                ExprKind::Field(target, field) => {
                    let target = target.accept(self);
                    let field_name = field.get_symbol().unwrap().mangled();
                    self.writer.assign_field(target, &field_name, value);
                },
                _ => unreachable!()
            }
        } else {
            let expr = expr.accept(self);
            self.writer.expr(expr);
        }
    }

    fn visit_builtin_stmt(&mut self, inner: &Box<Stmt>) {
        // self.is_builtin = true;
        // inner.accept(self);
        // self.is_builtin = false;
    }
}

impl ExprVisitor for IRGen {
    type ExprResult = IRExpr;

    fn visit_assignment_expr(
        &mut self,
        _expr: &Expr,
        _target: &Expr,
        value: &Expr,
    ) -> Self::ExprResult {
        value.accept(self)
        // self.writer
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
        IRExpr {
            kind: IRExprKind::Binary(Box::new(lhs), op.lexeme().to_string(), Box::new(rhs)),
            expr_type: expr.get_type().unwrap()
        }
    }

    fn visit_unary_expr(&mut self, expr: &Expr, op: &Token, operand: &Expr) -> Self::ExprResult {
        let operand = operand.accept(self);
        IRExpr {
            kind: IRExprKind::Unary(op.lexeme().to_string(), Box::new(operand)),
            expr_type: expr.get_type().unwrap()
        }
    }

    fn visit_function_call_expr(
        &mut self,
        expr: &Expr,
        target: Option<&Expr>,
        function: &ResolvedToken,
        args: &[Expr],
    ) -> Self::ExprResult {
        // let args: Vec<_> = args.iter().map(|a| a.accept(self)).collect();
        
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
                    let type_init = self.lib.type_metadata(&owner).unwrap();
                    GenericSpecialization::new(&type_init.generics, explicit_types)
                },
                _ => GenericSpecialization::new(&function_metadata.generics, explicit_types)
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

        let function_name = if core::is_direct_c_binding(&function_symbol) {
            String::from(function_symbol.last_component())
        }  else { 
            function_metadata.function_name(self.lib.as_ref(), &specialization)
        };
        let mut args: Vec<_> = args.iter().map(|a| a.accept(self)).collect();

        if let Some(target) = target {
            let target_expr = target.accept(self);
            if let FunctionKind::Method(..) = function_metadata.kind {
                let target_expr = match &target_expr.kind {
                    IRExprKind::Variable(v) if v == "self" => target_expr,
                    _ => {
                        let expr_type = NodeType::pointer_to(target_expr.expr_type.clone());
                        IRExpr {
                            kind: IRExprKind::Unary(String::from("&"), Box::new(target_expr)),
                            expr_type
                        }
                    },
                };
                args.insert(0, target_expr);
            }
        } else {
            if let FunctionKind::Method(..) = function_metadata.kind {
                args.insert(0, IRExpr {
                    kind: IRExprKind::Variable(String::from("self")),
                    expr_type: self.self_type().unwrap()
                });
            }
        }

        IRExpr {
            kind: IRExprKind::Call(function_name, args),
            expr_type: expr.get_type().unwrap()
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
            _ => IRExprKind::FieldAccess(Box::new(target_expr), field_symbol.mangled())
        };

        IRExpr {
            kind,
            expr_type: expr.get_type().unwrap()
        }
    }

    fn visit_literal_expr(&mut self, expr: &Expr, token: &Token) -> Self::ExprResult {
        IRExpr {
            kind: IRExprKind::Literal(token.lexeme().to_string()),
            expr_type: expr.get_type().unwrap()
        }
    }

    fn visit_variable_expr(&mut self, expr: &Expr, name: &ResolvedToken) -> Self::ExprResult {
        // IRExpr::Variable(name.get_symbol().unwrap().mangled())

        let expr_type = expr.get_type().unwrap();

        if let NodeType::Metatype(symbol, _) = &expr_type {
            let spec = self.func_specialization.as_ref().unwrap();
            if spec.map.contains_key(&symbol) {
                let spec_type = expr_type.specialize(self.lib.as_ref(), spec);
                return IRExpr {
                    kind: IRExprKind::ExplicitType,
                    expr_type: spec_type
                };
            }
        }

        let symbol = name.get_symbol().unwrap();

        if let Some(current_type) = self.current_type.as_ref() {
            let spec = self.func_specialization.as_ref().unwrap().subset(&current_type.symbol);
            let self_expr = IRExpr {
                kind: IRExprKind::Variable(String::from("self")),
                expr_type: NodeType::Instance(current_type.symbol.clone(), spec)
            };
            if symbol.is_self() {
                return self_expr;
            } else if current_type.symbol.owns(&symbol) {
                let kind = IRExprKind::DerefFieldAccess(
                    Box::new(IRExpr {
                        kind: self_expr.kind,
                        expr_type: NodeType::pointer_to(self_expr.expr_type)
                    }),
                    symbol.mangled()
                );
                return IRExpr {
                    kind: kind,
                    expr_type: expr_type
                };
            }
        }

        IRExpr {
            kind: IRExprKind::Variable(symbol.mangled()),
            expr_type: expr_type
        }
    }

    fn visit_array_expr(&mut self, expr: &Expr, elements: &[Expr]) -> Self::ExprResult {
        unimplemented!()
    }

    fn visit_subscript_expr(&mut self, expr: &Expr, target: &Expr, arg: &Expr) -> Self::ExprResult {
        let kind = IRExprKind::Subscript(
            Box::new(target.accept(self)),
            Box::new(arg.accept(self))
        );
        IRExpr {
            kind,
            expr_type: expr.get_type().unwrap()
        }
    }

    fn visit_cast_expr(
        &mut self,
        expr: &Expr,
        explicit_type: &ExplicitType,
        value: &Expr,
    ) -> Self::ExprResult {
        unimplemented!()
    }
}

pub struct IRWriter {
    lib: Rc<Lib>,
    program: IRProgram,
    blocks: Vec<Vec<IRStatement>>
}

impl IRWriter {
    fn new(lib: Rc<Lib>) -> Self {
        IRWriter {
            lib,
            program: IRProgram::new(),
            blocks: Vec::new()
        }
    }

    pub fn declare_struct(
        &mut self,
        type_metadata: &TypeMetadata,
        specialization: &GenericSpecialization,
    ) {
        let name = type_metadata.type_name(specialization);

        let fields: Vec<_> = type_metadata
            .field_types
            .iter()
            .zip(&type_metadata.field_symbols)
            .map(|(node_type, symbol)| {
                IRVariable {
                    name: symbol.mangled(),
                    var_type: node_type.specialize(self.lib.as_ref(), specialization)
                }
            }).collect();

        let structure = IRStructure {
            name,
            fields
        };
        self.program.structures.push(structure);
    }

    pub fn start_block(&mut self,) {
        self.blocks.push(Vec::new());
    }

    pub fn end_decl_main(&mut self) {
        let main = IRFunction {
            name: String::from("main"),
            parameters: Vec::new(),
            return_type: NodeType::Int,
            statements: self.blocks.pop().unwrap(),
        };
        self.program.functions.push(main);
    }

    pub fn end_decl_func(&mut self, function: &FunctionMetadata, specialization: &GenericSpecialization) {
        let mut parameters: Vec<_> = function
            .parameter_types
            .iter()
            .zip(&function.parameter_symbols)
            .map(|(param_type, symbol)| {
                IRVariable {
                    name: symbol.mangled(),
                    var_type: param_type.clone(),
                }
            })
            .collect();

        if let FunctionKind::Method(owner) = &function.kind {
            let self_instance = NodeType::Instance(owner.clone(), specialization.subset(owner));
            let self_type = NodeType::pointer_to(self_instance);
            parameters.insert(0, IRVariable {
                name: String::from("self"),
                var_type: self_type
            });
        }

        let function = IRFunction {
            name: function.function_name(self.lib.as_ref(), specialization),
            parameters,
            return_type: function.return_type.clone(),
            statements: self.blocks.pop().unwrap()
        };

        self.program.functions.push(function);
    }

    pub fn end_if_block(&mut self, condition: IRExpr) {
        let if_block = self.blocks.pop().unwrap();
        self.add_stmt(IRStatement::Condition(condition, if_block, Vec::new()));
    }

    pub fn end_if_else_blocks(&mut self, condition: IRExpr) {
        let else_block = self.blocks.pop().unwrap();
        let if_block = self.blocks.pop().unwrap();
        self.add_stmt(IRStatement::Condition(condition, if_block, else_block));
    }

    pub fn end_loop(&mut self) {
        let loop_stmt = IRStatement::Loop(self.blocks.pop().unwrap());
        self.add_stmt(loop_stmt);
    }

    pub fn declare_local(&mut self, symbol: Symbol, var_type: NodeType) -> String {
        let name = symbol.mangled();
        self.declare_local_str(&name, var_type);
        name
    }

    pub fn declare_local_str(&mut self, name: &str, var_type: NodeType) {
        let var = IRVariable {
            name: String::from(name),
            var_type,
        };
        self.add_stmt(IRStatement::DeclLocal(var));
    }

    pub fn assign_local(&mut self, var: &str, value: IRExpr) {
        self.add_stmt(IRStatement::AssignLocal(String::from(var), value));
    }

    pub fn assign_field(&mut self, var: IRExpr, field: &str, value: IRExpr) {

    }

    pub fn call(&mut self, symbol: Symbol, arguments: Vec<Symbol>) {
        // self.add_stmt(IRStatement::)
    }

    pub fn return_value(&mut self, expr: Option<IRExpr>) {

    }

    pub fn expr(&mut self, expr: IRExpr) {
        self.add_stmt(IRStatement::Execute(expr));
    }

    pub fn break_loop(&mut self) {
        // self.body().statements.push(Stmt)
    }

    pub fn add_stmt(&mut self, stmt: IRStatement) {
        self.body().push(stmt);
    }

    pub fn body(&mut self) -> &mut Vec<IRStatement> {
        self.blocks.last_mut().unwrap()
    }

    // pub fn write_guard<T: ContainsSpan>(&mut self, guard: String, message: &str, span: &T) {
    //     self.start_condition_block("if", guard);

    //     let message = format!(
    //         "\\nFatal error: {}\\n\\n{}\\n",
    //         message,
    //         span.span().location(),
    //     );

    //     self.writeln(&format!("printf(\"{}\\n\");", message));
    //     self.writeln("exit(1);");
    //     self.end_conditional_block();
    // }
}

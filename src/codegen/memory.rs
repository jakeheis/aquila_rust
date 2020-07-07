use super::ir::*;
use crate::library::*;
use std::rc::Rc;

pub struct FreeWriter<'a> {
    tables: &'a [Rc<SymbolTable>],
    function_sym: &'a Symbol,
    lines: &'a mut Vec<IRStatement>,
    tracker: &'a SpecializationTracker,
    free_at_return: Vec<IRVariable>,
    free_at_break: Vec<IRVariable>,
    inside_loop: bool,
}

impl<'a> FreeWriter<'a> {
    pub fn new(
        tables: &'a [Rc<SymbolTable>],
        function: &'a mut IRFunction,
        tracker: &'a SpecializationTracker,
    ) -> Self {
        FreeWriter {
            tables,
            function_sym: &function.name,
            lines: &mut function.statements,
            tracker,
            free_at_return: Vec::new(),
            free_at_break: Vec::new(),
            inside_loop: false,
        }
    }

    pub fn write(&mut self) {
        let mut locals: Vec<IRVariable> = Vec::new();

        let mut stop_index: usize = self.lines.len();
        let mut hit_return = false;
        let mut hit_break = false;

        let mut return_var: Option<String> = None;

        for (index, stmt) in self.lines.iter_mut().enumerate() {
            let mut done = false;
            match stmt {
                IRStatement::DeclLocal(var) => locals.push(var.clone()),
                IRStatement::Return(value) => {
                    stop_index = index;
                    if let Some(expr) = value {
                        if let IRExprKind::Variable(var) = &expr.kind {
                            if locals.iter().find(|v| &v.name == var).is_some() {
                                return_var = Some(var.clone());
                            } else if self.free_at_break.iter().find(|v| &v.name == var).is_some() {
                                return_var = Some(var.clone());
                            } else if self
                                .free_at_return
                                .iter()
                                .find(|v| &v.name == var)
                                .is_some()
                            {
                                return_var = Some(var.clone());
                            }
                        }
                    }
                    hit_return = true;
                    done = true;
                }
                IRStatement::Break => {
                    stop_index = index;
                    hit_break = true;
                    done = true;
                }
                IRStatement::Condition(_, if_body, else_body) => {
                    let mut free_at_return = self.free_at_return.clone();
                    let mut free_at_break = self.free_at_break.clone();

                    if self.inside_loop {
                        free_at_break.append(&mut locals.clone());
                    } else {
                        free_at_return.append(&mut locals.clone());
                    }

                    let mut if_writer = FreeWriter {
                        tables: self.tables,
                        function_sym: self.function_sym,
                        lines: if_body,
                        tracker: self.tracker,
                        free_at_return: free_at_return.clone(),
                        free_at_break: free_at_break.clone(),
                        inside_loop: self.inside_loop,
                    };
                    if_writer.write();

                    let mut else_writer = FreeWriter {
                        tables: self.tables,
                        function_sym: self.function_sym,
                        lines: else_body,
                        tracker: self.tracker,
                        free_at_return: free_at_return,
                        free_at_break: free_at_break,
                        inside_loop: self.inside_loop,
                    };
                    else_writer.write();
                }
                IRStatement::Loop(body) => {
                    let mut free_at_return = self.free_at_return.clone();
                    free_at_return.append(&mut self.free_at_break.clone());
                    free_at_return.append(&mut locals.clone());

                    let mut loop_writer = FreeWriter {
                        tables: self.tables,
                        function_sym: self.function_sym,
                        lines: body,
                        tracker: self.tracker,
                        free_at_return: free_at_return,
                        free_at_break: Vec::new(),
                        inside_loop: true,
                    };
                    loop_writer.write();
                }
                IRStatement::Assign(..) | IRStatement::Execute(..) => (),
            }
            if done {
                break;
            }
        }

        if hit_return {
            let free_at_return = std::mem::replace(&mut self.free_at_return, Vec::new());
            self.add_frees(&free_at_return, stop_index, &return_var);
        }
        if hit_break || hit_return {
            let free_at_break = std::mem::replace(&mut self.free_at_break, Vec::new());
            self.add_frees(&free_at_break, stop_index, &return_var);
        }
        self.add_frees(&locals, stop_index, &return_var);
    }

    fn add_frees(&mut self, list: &[IRVariable], return_index: usize, return_var: &Option<String>) {
        for var in list {
            if let Some(rv) = return_var.as_ref() {
                if &var.name == rv {
                    continue;
                }
            }
            if let NodeType::Instance(sym, spec) = &var.var_type {
                let deinit_sym = Symbol::deinit_symbol(&sym);
                let deinit = IRExpr::call_generic(
                    deinit_sym.clone(),
                    spec.clone(),
                    vec![IRExpr::address_of(&var)],
                    NodeType::Void,
                );
                let stmt = IRStatement::Execute(deinit);
                self.lines.insert(return_index, stmt);

                self.tracker
                    .add_call(self.function_sym.clone(), deinit_sym, spec.clone());
            }
        }
    }

    // fn get_type_metadata(&self, symbol: &Symbol) -> &TypeMetadata {
    //     for table in self.tables {
    //         if let Some(metadata) = table.get_type_metadata(symbol) {
    //             return metadata;
    //         }
    //     }
    //     panic!()
    // }
}

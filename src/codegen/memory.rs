use super::ir::*;
use crate::library::*;
use std::rc::Rc;

pub struct FreeWriter<'a> {
    tables: &'a [Rc<SymbolTable>],
    function_sym: &'a Symbol,
    lines: &'a mut Vec<IRStatement>,
    tracker: &'a SpecializationTracker,
    parent_vars: Vec<IRVariable>,
}

impl<'a> FreeWriter<'a> {

    pub fn new(tables: &'a [Rc<SymbolTable>], function: &'a mut IRFunction, tracker: &'a SpecializationTracker) -> Self {
        FreeWriter {
            tables,
            function_sym: &function.name,
            lines: &mut function.statements,
            tracker,
            parent_vars: Vec::new(),
        }
    }

    pub fn write(&mut self) {
        let mut locals: Vec<IRVariable> = Vec::new();

        let mut return_index: usize = self.lines.len();
        let mut return_var: Option<String> = None;

        for (index, stmt) in self.lines.iter_mut().enumerate() {
            let mut done = false;
            match stmt {
                IRStatement::DeclLocal(var) => locals.push(var.clone()),
                IRStatement::Return(value) => {
                    return_index = index;
                    if let Some(expr) = value {
                        if let IRExprKind::Variable(var) = &expr.kind {
                            if self.parent_vars.iter().find(|v| &v.name == var).is_some() {
                                return_var = Some(var.clone());
                            } else if locals.iter().find(|v| &v.name == var).is_some() {
                                return_var = Some(var.clone());
                            }
                        }
                    }
                    done = true;
                },
                IRStatement::Break => {
                    return_index = index;
                    done = true;
                },
                IRStatement::Condition(_, if_body, else_body) => {
                    let mut combined = self.parent_vars.clone();
                    combined.append(&mut locals.clone());
                    let mut if_writer = FreeWriter {
                        tables: self.tables,
                        function_sym: self.function_sym,
                        lines: if_body,
                        tracker: self.tracker,
                        parent_vars: combined.clone(),
                    };
                    if_writer.write();

                    let mut else_writer = FreeWriter {
                        tables: self.tables,
                        function_sym: self.function_sym,
                        lines: else_body,
                        tracker: self.tracker,
                        parent_vars: combined,
                    };
                    else_writer.write();
                },
                IRStatement::Loop(body) => {
                    panic!()
                }
                IRStatement::Assign(..) | IRStatement::Execute(..) => (),
            }
            if done {
                break;
            }
        }

        if return_index < self.lines.len() {
            let pvs = std::mem::replace(&mut self.parent_vars, Vec::new());
            self.add_frees(&pvs, return_index, &return_var);
        }
        self.add_frees(&locals, return_index, &return_var);
    }

    fn add_frees(&mut self, list: &[IRVariable], return_index: usize, return_var: &Option<String>) {
        for var in list {
            if let Some(rv) = return_var.as_ref() {
                if &var.name == rv {
                    continue;
                }
            }
            if let NodeType::Instance(sym, spec) = &var.var_type {
                let metadata = self.get_type_metadata(sym);
                if metadata.conforms_to(&Symbol::stdlib("Freeable")) {
                    let free_sym = Symbol::new_str(sym, "free");
                    let free = IRExpr::call_generic(
                        free_sym.clone(), 
                        spec.clone(),
                        vec![IRExpr::address_of(&var)], 
                        NodeType::Void
                    );
                    let stmt = IRStatement::Execute(free);
                    self.lines.insert(return_index, stmt);

                    self.tracker.add_call(self.function_sym.clone(), free_sym, spec.clone());
                }
            }
        }
    }

    fn get_type_metadata(&self, symbol: &Symbol) -> &TypeMetadata {
        for table in self.tables {
            if let Some(metadata) = table.get_type_metadata(symbol) {
                return metadata;
            }
        }
        panic!()
    }

}

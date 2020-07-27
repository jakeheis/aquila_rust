use crate::diagnostic::*;
use crate::library::*;
use log::trace;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub struct CycleChecker<'a> {
    module: &'a mut ParsedModule,
    field_map: HashMap<Symbol, HashSet<Symbol>>,
    reporter: Rc<dyn Reporter>,
}

impl<'a> CycleChecker<'a> {
    pub fn check(module: &mut ParsedModule, symbols: &SymbolTable, reporter: Rc<dyn Reporter>) {
        let mut field_map: HashMap<Symbol, HashSet<Symbol>> = HashMap::new();

        for (type_symbol, type_info) in &symbols.type_metadata {
            let mut field_set: HashSet<Symbol> = HashSet::new();
            for field in &type_info.fields {
                if let NodeType::Instance(field_type, _) = &field.var_type {
                    field_set.insert(field_type.clone());
                }
            }
            field_map.insert(type_symbol.clone(), field_set);
        }

        trace!(target: "cycle_checker", "Field map: {:#?}", field_map);

        let mut checker = CycleChecker {
            module,
            field_map,
            reporter,
        };

        checker.run();
    }

    fn run(&mut self) {
        let mut visited = HashSet::new();
        for type_symbol in self.field_map.keys() {
            let mut chain = vec![type_symbol.clone()];
            self.visit(&mut chain, &mut visited);
        }

        if !self.reporter.has_errored() {
            let mut visited = HashSet::new();
            let mut ordered: Vec<Symbol> = Vec::new();

            for type_symbol in self.field_map.keys() {
                self.post_order(&mut ordered, &mut visited, type_symbol.clone());
            }

            self.module.type_decls.sort_by(|lhs, rhs| {
                let lhs_symbol = lhs.name.get_symbol().unwrap();
                let rhs_symbol = rhs.name.get_symbol().unwrap();

                let lhs_index = ordered.iter().position(|s| s == &lhs_symbol).unwrap();
                let rhs_index = ordered.iter().position(|s| s == &rhs_symbol).unwrap();
                lhs_index.cmp(&rhs_index)
            })
        }
    }

    fn visit(&self, chain: &mut Vec<Symbol>, visited: &mut HashSet<Symbol>) {
        let cur_symbol = chain.last().unwrap().clone();

        if visited.contains(&cur_symbol) {
            return;
        }

        visited.insert(cur_symbol.clone());

        // If field_map doesn't have symbol, isn't in this lib -> won't be a cycle
        let fields = if let Some(f) = self.field_map.get(&cur_symbol) {
            f
        } else {
            return;
        };
        for field_symbol in fields {
            if field_symbol == &cur_symbol {
                self.reporter.report(Diagnostic::error(
                    self.span_for_type_symbol(&cur_symbol),
                    "Cannot contain a property of the same type",
                ));
            } else if chain.contains(field_symbol) {
                let message = format!(
                    "Circular reference between {} and {}",
                    cur_symbol.name(),
                    field_symbol.name()
                );
                self.reporter.report(Diagnostic::error(
                    self.span_for_type_symbol(&cur_symbol),
                    &message,
                ));
            } else {
                chain.push(field_symbol.clone());
                self.visit(chain, visited);
                chain.pop();
            }
        }
    }

    fn span_for_type_symbol(&self, symbol: &Symbol) -> &Span {
        for type_decl in &self.module.type_decls {
            if &type_decl.name.get_symbol().unwrap() == symbol {
                return type_decl.name.span();
            }
        }
        panic!()
    }

    fn post_order(
        &self,
        list: &mut Vec<Symbol>,
        visited: &mut HashSet<Symbol>,
        cur_symbol: Symbol,
    ) {
        if visited.contains(&cur_symbol) {
            return;
        }

        visited.insert(cur_symbol.clone());

        // If field_map doesn't have symbol, isn't in this lib -> won't be a cycle
        let fields = if let Some(f) = self.field_map.get(&cur_symbol) {
            f
        } else {
            return;
        };
        for field_symbol in fields {
            self.post_order(list, visited, field_symbol.clone());
        }

        list.push(cur_symbol);
    }
}

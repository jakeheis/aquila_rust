use super::*;
use crate::diagnostic::*;
use crate::library::*;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub struct CycleChecker {
    field_map: HashMap<Symbol, HashSet<Symbol>>,
    reporter: Rc<dyn Reporter>,
}

impl CycleChecker {
    pub fn check(lib: &mut Lib, reporter: Rc<dyn Reporter>) {
        let mut field_map: HashMap<Symbol, HashSet<Symbol>> = HashMap::new();

        for (type_symbol, type_info) in &lib.symbols.type_metadata {
            let mut field_set: HashSet<Symbol> = HashSet::new();
            for field in &type_info.field_types {
                if let NodeType::Instance(field_type, _) = field {
                    field_set.insert(field_type.clone());
                }
            }
            field_map.insert(type_symbol.clone(), field_set);
        }

        let mut checker = CycleChecker {
            field_map,
            reporter,
        };

        checker.run(lib);
    }

    fn run(&mut self, lib: &mut Lib) {
        let mut visited = HashSet::new();
        for type_symbol in self.field_map.keys() {
            let mut chain = vec![type_symbol.clone()];
            self.visit(&mut chain, &mut visited, lib);
        }

        if !self.reporter.has_errored() {
            let mut visited = HashSet::new();
            let mut ordered: Vec<Symbol> = Vec::new();

            for type_symbol in self.field_map.keys() {
                self.post_order(&mut ordered, &mut visited, type_symbol.clone());
            }

            lib.type_decls.sort_by(|lhs, rhs| {
                let lhs_symbol = lhs.name.get_symbol().unwrap();
                let rhs_symbol = rhs.name.get_symbol().unwrap();

                let lhs_index = ordered.iter().position(|s| s == &lhs_symbol).unwrap();
                let rhs_index = ordered.iter().position(|s| s == &rhs_symbol).unwrap();
                lhs_index.cmp(&rhs_index)
            })
        }
    }

    fn visit(&self, chain: &mut Vec<Symbol>, visited: &mut HashSet<Symbol>, lib: &Lib) {
        let cur_symbol = chain.last().unwrap().clone();

        if visited.contains(&cur_symbol) {
            return;
        }

        visited.insert(cur_symbol.clone());

        let fields = self.field_map.get(&cur_symbol).unwrap();
        for field_symbol in fields {
            if field_symbol == &cur_symbol {
                self.reporter.report(Diagnostic::error(
                    &lib.symbol_span(&cur_symbol).unwrap(),
                    "Cannot contain a property of the same type",
                ));
            } else if chain.contains(field_symbol) {
                let message = format!(
                    "Circular reference between {} and {}",
                    cur_symbol.mangled(),
                    field_symbol.mangled()
                );
                self.reporter.report(Diagnostic::error(
                    &lib.symbol_span(&cur_symbol).unwrap(),
                    &message,
                ));
            } else {
                chain.push(field_symbol.clone());
                self.visit(chain, visited, lib);
                chain.pop();
            }
        }
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

        let fields = self.field_map.get(&cur_symbol).unwrap();
        for field_symbol in fields {
            self.post_order(list, visited, field_symbol.clone());
        }

        list.push(cur_symbol);
    }
}

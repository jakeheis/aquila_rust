use super::*;
use crate::diagnostic::*;
use crate::guard;
use crate::parsing::*;
use crate::source::*;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub struct CycleChecker {
    field_map: HashMap<Symbol, (Span, HashSet<Symbol>)>,
    reporter: Rc<dyn Reporter>,
}

impl CycleChecker {
    pub fn check(program: ParsedProgram, reporter: Rc<dyn Reporter>) -> ParsedProgram {
        let mut field_map: HashMap<Symbol, (Span, HashSet<Symbol>)> = HashMap::new();

        for stmt in &program.type_decls {
            guard!(StmtKind::TypeDecl[name, fields, _methods, _meta_methods] = &stmt.kind);

            let borrowed_type_symbol = stmt.symbol.borrow();
            let type_symbol = borrowed_type_symbol.clone().unwrap();

            let mut field_set: HashSet<Symbol> = HashSet::new();

            for field in fields {
                let borrowed_field_type = field.stmt_type.borrow();
                if let NodeType::Type(field_type) = borrowed_field_type.as_ref().unwrap() {
                    field_set.insert(field_type.clone());
                }
            }

            field_map.insert(type_symbol, (name.span().clone(), field_set));
        }

        let mut checker = CycleChecker {
            field_map,
            reporter,
        };

        checker.run(program)
    }

    fn run(&mut self, mut program: ParsedProgram) -> ParsedProgram {
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

            program.type_decls.sort_by(|lhs, rhs| {
                let lhs_symbol_borrowed = lhs.symbol.borrow();
                let lhs_symbol = lhs_symbol_borrowed.as_ref().unwrap();
                let rhs_symbol_borrowed = rhs.symbol.borrow();
                let rhs_symbol = rhs_symbol_borrowed.as_ref().unwrap();

                let lhs_index = ordered.iter().position(|s| s == lhs_symbol).unwrap();
                let rhs_index = ordered.iter().position(|s| s == rhs_symbol).unwrap();
                lhs_index.cmp(&rhs_index)
            })
        }

        program
    }

    fn visit(&self, chain: &mut Vec<Symbol>, visited: &mut HashSet<Symbol>) {
        let cur_symbol = chain.last().unwrap().clone();

        if visited.contains(&cur_symbol) {
            return;
        }

        visited.insert(cur_symbol.clone());

        let (span, fields) = self.field_map.get(&cur_symbol).unwrap();
        for field_symbol in fields {
            if field_symbol == &cur_symbol {
                self.reporter.report(Diagnostic::error(
                    span,
                    "Cannot contain a property of the same type",
                ));
            } else if chain.contains(field_symbol) {
                let message = format!(
                    "Circular reference between {} and {}",
                    cur_symbol.mangled(),
                    field_symbol.mangled()
                );
                self.reporter.report(Diagnostic::error(span, &message));
            } else {
                chain.push(field_symbol.clone());
                self.visit(chain, visited);
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

        let (_, fields) = self.field_map.get(&cur_symbol).unwrap();
        for field_symbol in fields {
            self.post_order(list, visited, field_symbol.clone());
        }

        list.push(cur_symbol);
    }
}

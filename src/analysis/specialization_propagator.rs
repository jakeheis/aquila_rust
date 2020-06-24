use crate::analysis::*;
use crate::library::*;
use log::trace;
use std::collections::{HashMap, HashSet};
use std::cell::RefCell;

pub struct SpecializationTracker {
    call_map: RefCell<HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>>,
    explicit_type_specializations: RefCell<HashMap<Symbol, HashSet<GenericSpecialization>>>,
}

impl SpecializationTracker {
    pub fn new() -> Self {
        SpecializationTracker {
            call_map: RefCell::new(HashMap::new()),
            explicit_type_specializations: RefCell::new(HashMap::new()),
        }
    }

    pub fn add_call(&self, from: Symbol, to: Symbol, with: GenericSpecialization) {
        self.call_map.borrow_mut().entry(from).or_insert(Vec::new()).push((to, with));
    }

    pub fn add_required_type_spec(&self, type_symbol: Symbol, spec: GenericSpecialization) {
        self.explicit_type_specializations.borrow_mut().entry(type_symbol).or_insert(HashSet::new()).insert(spec);
    }
}

pub struct SpecializationPropagator<'a> {
    lib: &'a mut Lib,
    call_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>,
    visited: HashSet<String>,
}

impl<'a> SpecializationPropagator<'a> {
    pub fn propogate(lib: &mut Lib) {
        let explicit_list = lib.specialization_tracker.explicit_type_specializations.replace(HashMap::new());
        for (type_symbol, specs) in explicit_list.into_iter() {
            let type_metadata = lib.type_metadata_mut(&type_symbol).unwrap();
            for spec in specs {
                type_metadata.specializations.insert(spec);
            }
        }

        let mut call_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>> = HashMap::new();
        SpecializationPropagator::flattened_call_map(lib, &mut call_map);

        for (caller, calls) in &call_map {
            for (call, spec) in calls {
                let suffix = if spec.map.is_empty() {
                    String::new()
                } else {
                    format!(" {}", spec)
                };
                trace!(target: "spec_propagate", "Call entry: {} -> {}{}", caller, call, suffix);
            }
        }

        let mut prop = SpecializationPropagator {
            lib,
            call_map,
            visited: HashSet::new(),
        };
        prop.go();

        trace!(target: "spec_propagate", "{}", lib.symbols);
        trace!(target: "spec_propagate", "{}", lib.dependencies[0].symbols);
    }

    pub fn flattened_call_map(
        lib: &Lib,
        call_map: &mut HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>,
    ) {
        let lib_map = lib.specialization_tracker.call_map.borrow();
        for (caller, callees) in lib_map.iter() {
            let all = call_map.entry(caller.clone()).or_insert(Vec::new());
            all.append(&mut callees.clone());
        }
        for lib in &lib.dependencies {
            SpecializationPropagator::flattened_call_map(lib, call_map);
        }
    }

    pub fn go(&mut self) {
        self.propagate(&Symbol::main_symbol(), &GenericSpecialization::empty());
    }

    fn propagate(&mut self, cur: &Symbol, current_spec: &GenericSpecialization) {
        let metadata = self.lib.function_metadata(cur);
        let func_id = metadata
            .as_ref()
            .map(|m| m.function_name(self.lib, current_spec))
            .unwrap_or(cur.mangled());
        if self.visited.contains(&func_id) {
            return;
        }

        trace!(target: "spec_propagate", "Visiting function  {} -- {}", cur, func_id);

        self.visited.insert(func_id);

        trace!(target: "spec_propagate", "Adding function spec {} to {}", current_spec, cur);
        if let Some(mut_metadata) = self.lib.function_metadata_mut(cur) {
            mut_metadata.specializations.insert(current_spec.clone());
        }

        match metadata.map(|m| m.kind) {
            Some(FunctionKind::Method(owner)) | Some(FunctionKind::MetaMethod(owner)) => {
                let subset = current_spec.subset(&owner);
                let type_metadata = self.lib.type_metadata_mut(&owner).unwrap();
                type_metadata.specializations.insert(subset);
            }
            _ => (),
        }

        if let Some(calls) = self.call_map.get(cur) {
            let calls = calls.clone();
            for (callee_function_symbol, call_spec) in calls {
                let call_spec = call_spec.resolve_generics_using(self.lib, current_spec);
                self.propagate(&callee_function_symbol, &call_spec);
            }
        }

        trace!(target: "spec_propagate", "Finished function {}", cur);
    }
}

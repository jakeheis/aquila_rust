use crate::analysis::*;
use crate::library::*;
use log::trace;
use std::collections::{HashMap, HashSet};

pub struct SpecializationPropagator<'a> {
    lib: &'a mut Lib,
    call_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>,
    visited: HashSet<String>,
}

impl<'a> SpecializationPropagator<'a> {
    pub fn propogate(lib: &mut Lib) {
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
        for (caller, callees) in &lib.symbols.call_map {
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

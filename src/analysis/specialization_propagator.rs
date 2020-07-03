use crate::library::*;
use log::trace;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

type SpecializationTrackerMap = HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>;

pub struct SpecializationTracker {
    call_map: RefCell<SpecializationTrackerMap>,
    explicit_type_map: RefCell<SpecializationTrackerMap>,
}

impl SpecializationTracker {
    pub fn new() -> Self {
        SpecializationTracker {
            call_map: RefCell::new(HashMap::new()),
            explicit_type_map: RefCell::new(HashMap::new()),
        }
    }

    pub fn add_call(&self, from: Symbol, to: Symbol, with: GenericSpecialization) {
        self.call_map
            .borrow_mut()
            .entry(from)
            .or_insert(Vec::new())
            .push((to, with));
    }

    pub fn add_required_type_spec(
        &self,
        in_func: Symbol,
        type_symbol: Symbol,
        spec: GenericSpecialization,
    ) {
        self.explicit_type_map
            .borrow_mut()
            .entry(in_func)
            .or_insert(Vec::new())
            .push((type_symbol, spec));
    }
}

impl std::fmt::Display for SpecializationTracker {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let call_map = self.call_map.borrow();
        for (caller, calls) in call_map.iter() {
            write!(f, "\nCalls for {}:", caller.id)?;
            for (call, spec) in calls {
                write!(f, "\n  {} -- {}", call, spec)?;
            }
        }

        let explicit_type_specializations = self.explicit_type_map.borrow();
        for (caller, explicit_types) in explicit_type_specializations.iter() {
            write!(f, "\nExplicit types for {}:", caller.id)?;
            for (et, spec) in explicit_types {
                write!(f, "\n  {} -- {}", et, spec)?;
            }
        }

        Ok(())
    }
}

pub struct SpecializationPropagator<'a> {
    lib: &'a mut Lib,
    call_map: SpecializationTrackerMap,
    explicit_type_map: SpecializationTrackerMap,
    visited: HashSet<String>,
}

impl<'a> SpecializationPropagator<'a> {
    pub fn propogate(lib: &mut Lib) {
        trace!(target: "spec_propagate", "{}", lib.specialization_tracker);
        for dep in &lib.dependencies {
            trace!(target: "spec_propagate", "{}", dep.specialization_tracker);
        }

        let mut call_map: SpecializationTrackerMap = HashMap::new();
        SpecializationPropagator::flattened_call_map(lib, &mut call_map);

        let mut explicit_type_map: SpecializationTrackerMap = HashMap::new();
        SpecializationPropagator::flattened_explicit_type_map(lib, &mut explicit_type_map);

        let mut prop = SpecializationPropagator {
            lib,
            call_map,
            explicit_type_map,
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

    pub fn flattened_explicit_type_map(
        lib: &Lib,
        explicit_type_map: &mut HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>,
    ) {
        let lib_map = lib.specialization_tracker.explicit_type_map.borrow();
        for (enclosing_func, explicit_types) in lib_map.iter() {
            let all = explicit_type_map
                .entry(enclosing_func.clone())
                .or_insert(Vec::new());
            all.append(&mut explicit_types.clone());
        }
        for lib in &lib.dependencies {
            SpecializationPropagator::flattened_explicit_type_map(lib, explicit_type_map);
        }
    }

    pub fn go(&mut self) {
        let main_symbol = Symbol::main_symbol(self.lib);
        self.propagate(&main_symbol, &GenericSpecialization::empty());
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

        // match metadata.map(|m| m.kind) {
        //     Some(FunctionKind::Method(owner)) | Some(FunctionKind::MetaMethod(owner)) => {
        //         let subset = current_spec.subset(&owner);
        //         let type_metadata = self.lib.type_metadata_mut(&owner).unwrap();
        //         // self.propagate_through_types(&owner, &subset);
        //         println!("adding subset {}", subset);
        //         type_metadata.specializations.insert(subset);
        //     }
        //     _ => (),
        // }

        if let Some(calls) = self.call_map.get(cur) {
            let calls = calls.clone();
            for (callee_function_symbol, call_spec) in calls {
                let call_spec = call_spec.resolve_generics_using(self.lib, current_spec);
                self.propagate(&callee_function_symbol, &call_spec);
            }
        }

        if let Some(explicit_types) = self.explicit_type_map.get(cur) {
            let explicit_types = explicit_types.clone();
            for (explicit_type_symbol, explicit_type_spec) in explicit_types {
                let explicit_type_spec =
                    explicit_type_spec.resolve_generics_using(self.lib, current_spec);

                trace!(target: "spec_propagate", "in func {} Propping from type spec {} to {}", cur, explicit_type_spec, explicit_type_symbol);

                self.propagate_through_type(&explicit_type_symbol, &explicit_type_spec);
            }
        }

        trace!(target: "spec_propagate", "Finished function {}", cur);
    }

    fn propagate_through_type(&mut self, cur: &Symbol, current_spec: &GenericSpecialization) {
        let metadata = self.lib.type_metadata_ref(cur).unwrap();
        let type_id = metadata.type_name(current_spec);

        if self.visited.insert(type_id) == false {
            return;
        }

        trace!(target: "spec_propagate", "Adding type spec {} to {}", current_spec, cur);

        if let Some(mut_metadata) = self.lib.type_metadata_mut(cur) {
            mut_metadata.specializations.insert(current_spec.clone());
        }

        let type_metadata = self.lib.type_metadata_ref(cur).unwrap().clone();
        for field_type in &type_metadata.field_types {
            if let NodeType::Instance(target, field_spec) = field_type {
                let field_spec = field_spec.resolve_generics_using(self.lib, &current_spec);
                self.propagate_through_type(target, &field_spec);
            }
        }

        trace!(target: "spec_propagate", "Finished function {}", cur);
    }
}

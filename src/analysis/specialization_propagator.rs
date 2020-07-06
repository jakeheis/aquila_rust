use crate::library::*;
use log::trace;
use std::collections::{HashMap, HashSet};

pub struct FinalSpecializationMap {
    map: HashMap<Symbol, HashSet<GenericSpecialization>>,
}

impl FinalSpecializationMap {
    fn new() -> Self {
        FinalSpecializationMap {
            map: HashMap::new(),
        }
    }

    fn add_spec(&mut self, symbol: Symbol, spec: GenericSpecialization) {
        self.map
            .entry(symbol)
            .or_insert(HashSet::new())
            .insert(spec);
    }

    pub fn specs_for(&self, symbol: &Symbol) -> Option<&HashSet<GenericSpecialization>> {
        self.map.get(symbol)
    }

    pub fn dump(&self) {
        println!("FinalSpecializationMap");
        for (symbol, specs) in &self.map {
            println!("  {}", symbol);
            for spec in specs {
                println!("    {}", spec);
            }
        }
    }
 }

pub struct SpecializationPropagator<'a> {
    libs: &'a [Module],
    call_map: SpecializationTrackerMap,
    explicit_type_map: SpecializationTrackerMap,
    visited: HashSet<String>,
    map: FinalSpecializationMap,
}

impl<'a> SpecializationPropagator<'a> {
    pub fn propagate(libs: &[Module]) -> FinalSpecializationMap {
        for lib in libs {
            trace!(target: "spec_propagate", "{}", lib.specialization_tracker);
        }

        let mut call_map: SpecializationTrackerMap = HashMap::new();
        let mut explicit_type_map: SpecializationTrackerMap = HashMap::new();

        for lib in libs {
            let lib_map = lib.specialization_tracker.call_map.borrow();
            for (caller, callees) in lib_map.iter() {
                let all = call_map.entry(caller.clone()).or_insert(Vec::new());
                all.append(&mut callees.clone());
            }

            let lib_map = lib.specialization_tracker.explicit_type_map.borrow();
            for (enclosing_func, explicit_types) in lib_map.iter() {
                let all = explicit_type_map
                    .entry(enclosing_func.clone())
                    .or_insert(Vec::new());
                all.append(&mut explicit_types.clone());
            }
        }

        let mut prop = SpecializationPropagator {
            libs,
            call_map,
            explicit_type_map,
            visited: HashSet::new(),
            map: FinalSpecializationMap::new(),
        };
        prop.go();

        prop.map
    }

    pub fn go(&mut self) {
        let main_symbol = Symbol::main_symbol();
        self.propagate_through_function(&main_symbol, &GenericSpecialization::empty());
    }

    fn propagate_through_function(&mut self, cur: &Symbol, current_spec: &GenericSpecialization) {
        let func_id = cur.specialized(current_spec);
        if self.visited.contains(&func_id) {
            return;
        }

        trace!(target: "spec_propagate", "Visiting function  {} -- {}", cur, func_id);

        self.visited.insert(func_id);

        trace!(target: "spec_propagate", "Adding function spec {} to {}", current_spec, cur);
        self.map.add_spec(cur.clone(), current_spec.clone());

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
                let call_spec = call_spec.resolve_generics_using(current_spec);
                self.propagate_through_function(&callee_function_symbol, &call_spec);
            }
        }

        if let Some(explicit_types) = self.explicit_type_map.get(cur) {
            let explicit_types = explicit_types.clone();
            for (explicit_type_symbol, explicit_type_spec) in explicit_types {
                let explicit_type_spec =
                    explicit_type_spec.resolve_generics_using(current_spec);

                trace!(target: "spec_propagate", "in func {} Propping from type spec {} to {}", cur, explicit_type_spec, explicit_type_symbol);

                self.propagate_through_type(&explicit_type_symbol, &explicit_type_spec);
            }
        }

        trace!(target: "spec_propagate", "Finished function {}", cur);
    }

    fn propagate_through_type(&mut self, cur: &Symbol, current_spec: &GenericSpecialization) {
        let type_id = cur.specialized(current_spec);

        if self.visited.insert(type_id) == false {
            return;
        }

        trace!(target: "spec_propagate", "Adding type spec {} to {}", current_spec, cur);

        self.map.add_spec(cur.clone(), current_spec.clone());

        let type_metadata = self.type_metadata(cur).clone();
        for field_type in &type_metadata.field_types {
            if let NodeType::Instance(target, field_spec) = field_type {
                let field_spec = field_spec.resolve_generics_using(&current_spec);
                self.propagate_through_type(target, &field_spec);
            }
        }

        trace!(target: "spec_propagate", "Finished function {}", cur);
    }

    fn type_metadata(&self, symbol: &Symbol) -> &TypeMetadata {
        for lib in self.libs {
            if let Some(metadata) = lib.symbols.get_type_metadata(symbol) {
                return metadata;
            }
        }
        panic!("Propagating through undefined type {}", symbol)
    }
}

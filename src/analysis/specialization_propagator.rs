use crate::analysis::*;
use crate::library::*;
use log::trace;
use std::collections::{HashMap, HashSet};

// pub struct SpecializationTracker {
//     type_map: HashMap<Symbol, Vec<GenericSpecialization>>,
//     call_map: HashMap<Symbol, Vec<(Symbol, Option<GenericSpecialization>)>>,
// }

pub struct SpecializationPropagator<'a> {
    lib: &'a mut Lib,
    call_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>,
    visited: HashSet<String>,
}

impl<'a> SpecializationPropagator<'a> {
    pub fn propogate(lib: &mut Lib) {
        for (caller, calls) in &lib.symbols.call_map {
            for (call, spec) in calls {
                let suffix = if spec.node_types.is_empty() {
                    String::new()
                } else {
                    format!(" {}", spec)
                };
                trace!(target: "spec_propagate", "Call entry: {} -> {}{}", caller, call, suffix);
            }
        }

        let mut call_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>> =
            HashMap::new();
        SpecializationPropagator::flattened_call_map(lib, &mut call_map);

        let mut prop = SpecializationPropagator {
            lib,
            call_map,
            visited: HashSet::new(),
        };
        prop.go();
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
        self.propagate(&Symbol::main_symbol(), &GenericSpecialization::empty(&Symbol::main_symbol()));
    }

    fn propagate(&mut self, cur: &Symbol, current_spec: &GenericSpecialization) {
        let metadata = self.lib.function_metadata(cur);
        let func_id = metadata
            .map(|m| m.function_name(current_spec))
            .unwrap_or(cur.mangled());
        if self.visited.contains(&func_id) {
            return;
        }

        trace!(target: "spec_propagate", "Visiting {} -- {}", cur, func_id);

        self.visited.insert(func_id);

        if !current_spec.node_types.is_empty() {
            trace!(target: "spec_propagate", "Adding spec {} to {}", current_spec, cur);
            let metadata = self.lib.function_metadata_mut(cur).unwrap();
            metadata.specializations.push(current_spec.clone());
        }

        if let Some(calls) = self.call_map.get(cur) {
            let calls = calls.clone();
            for (callee_function_symbol, call_spec) in calls {

                // let call_spec = if !current_spec.node_types.is_empty() {
                    // call_spec.map(|call_spec| {
                        // trace!(target: "spec_propagate", "Generic func {} calling generic func {}", cur, call);

                        // trace!(target: "spec_propagate", "Generic func {} calling generic func {}", cur, call);
                        // let spec_types: Vec<_> = call_spec.node_types.iter().map(|arg_type| {
                        //     match arg_type {
                        //         NodeType::Instance(type_symbol, type_specialization) => {
                        //             if callee_function_symbol.owns(type_symbol) {
                        //                 let generic_index = lib.type_metadata(symbol).and_then(|t| t.generic_index).unwrap();
                        //                 trace!(target: "spec_propagate", "Specializing {} to {}", arg_type, cur_spec.node_types[*index]);
                        //                 cur_spec.node_types[*index].clone()
                        //             }
                        //             let type_metadata = self.lib.type_metadata(type_symbol).unwrap();

                        //         }
                        //         NodeType::Generic(_, index) => {
                        //             trace!(target: "spec_propagate", "Specializing {} to {}", arg_type, cur_spec.node_types[*index]);
                        //             cur_spec.node_types[*index].clone()
                        //         },
                        //         _ => arg_type.clone(),
                        //     }
                        // }).collect();
                //         GenericSpecialization::new(&call, spec_types)
                //     })
                // } else {
                //     call_spec
                // };
                let call_spec = call_spec.resolve_generics_using(self.lib, current_spec);
                self.propagate(&callee_function_symbol, &call_spec);
            }
        }

        trace!(target: "spec_propagate", "Finished {}", cur);
    }
}

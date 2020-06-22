use crate::analysis::*;
use crate::library::*;
use log::trace;
use std::collections::{HashMap, HashSet};

// pub struct SpecializationTracker {
//     type_map: HashMap<Symbol, Vec<GenericSpecialization>>,
//     call_map: HashMap<Symbol, Vec<(Symbol, Option<GenericSpecialization>)>>,
// }
/*
pub struct TypeSpecializationPropagator<'a> {
    lib: &'a mut Lib,
    type_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>,
    visited: HashSet<String>,
}


impl<'a> TypeSpecializationPropagator<'a> {
    pub fn propogate(lib: &mut Lib) {
        for (caller, initializations) in &lib.symbols.type_specialization_map {
            for (call, initialization) in initializations {
                let suffix = if initialization.node_types.is_empty() {
                    String::new()
                } else {
                    format!(" {}", initialization)
                };
                trace!(target: "spec_propagate", "Call entry: {} -> {}{}", caller, call, suffix);
            }
        }

        let mut type_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>> =
            HashMap::new();
        TypeSpecializationPropagator::flattened_call_map(lib, &mut type_map);

        let mut prop = TypeSpecializationPropagator {
            lib,
            type_map,
            visited: HashSet::new(),
        };
        prop.go();
    }

    pub fn flattened_call_map(
        lib: &Lib,
        type_specialization_map: &mut HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>,
    ) {
        for (caller, inits) in &lib.symbols.type_specialization_map {
            let all = type_specialization_map.entry(caller.clone()).or_insert(Vec::new());
            all.append(&mut inits.clone());
        }
        for lib in &lib.dependencies {
            FunctionSpecializationPropagator::flattened_call_map(lib, type_specialization_map);
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

        trace!(target: "spec_propagate", "Visiting type {} -- {}", cur, func_id);

        self.visited.insert(func_id);

        if !current_spec.node_types.is_empty() {
            trace!(target: "spec_propagate", "Adding type spec {} to {}", current_spec, cur);
            let metadata = self.lib.function_metadata_mut(cur).unwrap();
            metadata.specializations.push(current_spec.clone());
        }

        if let Some(inits) = self.type_map.get(cur) {
            let inits = inits.clone();
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

        trace!(target: "spec_propagate", "Finished type {}", cur);
    }
}

*/
pub struct FunctionSpecializationPropagator<'a> {
    lib: &'a mut Lib,
    call_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>,
    visited: HashSet<String>,
}

impl<'a> FunctionSpecializationPropagator<'a> {
    pub fn propogate(lib: &mut Lib) {
        for (caller, calls) in &lib.symbols.call_map {
            for (call, spec) in calls {
                let suffix = if spec.map.is_empty() {
                    String::new()
                } else {
                    format!(" {}", spec)
                };
                trace!(target: "spec_propagate", "Call entry: {} -> {}{}", caller, call, suffix);
            }
        }

        let mut call_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>> =
            HashMap::new();
        FunctionSpecializationPropagator::flattened_call_map(lib, &mut call_map);

        let mut prop = FunctionSpecializationPropagator {
            lib,
            call_map,
            visited: HashSet::new(),
        };
        prop.go();

        trace!(target: "spec_propagate", "{}", lib.symbols);
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
            FunctionSpecializationPropagator::flattened_call_map(lib, call_map);
        }
    }

    pub fn go(&mut self) {
        self.propagate(&Symbol::main_symbol(), &GenericSpecialization::empty());
    }

    fn propagate(&mut self, cur: &Symbol, current_spec: &GenericSpecialization) {
        let metadata = self.lib.function_metadata(cur);
        let func_id = metadata
            .map(|m| m.function_name(self.lib, current_spec))
            .unwrap_or(cur.mangled());
        if self.visited.contains(&func_id) {
            return;
        }

        trace!(target: "spec_propagate", "Visiting function  {} -- {}", cur, func_id);

        self.visited.insert(func_id);

        trace!(target: "spec_propagate", "Adding function spec {} to {}", current_spec, cur);
        if let Some(mut_metadata) = self.lib.function_metadata_mut(cur) {
            mut_metadata.specializations.push(current_spec.clone());
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
                if callee_function_symbol.last_component() == "init" {
                    println!("init of {} with {}", callee_function_symbol, call_spec);
                }
                self.propagate(&callee_function_symbol, &call_spec);
            }
        }

        trace!(target: "spec_propagate", "Finished function {}", cur);
    }
}

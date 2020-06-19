use crate::analysis::*;
use crate::library::*;
use std::collections::{HashMap, HashSet};
use log::trace;

pub struct SpecializationPropagator<'a> {
    lib: &'a mut Lib,
    call_map: HashMap<Symbol, Vec<(Symbol, Option<GenericSpecialization>)>>,
    visited: HashSet<String>,
}

impl<'a> SpecializationPropagator<'a> {

    pub fn propogate(lib: &mut Lib, call_map: HashMap<Symbol, Vec<(Symbol, Option<GenericSpecialization>)>>) {
        let mut prop = SpecializationPropagator {
            lib,
            call_map,
            visited: HashSet::new(),
        };
        prop.go();
    }

    pub fn go(&mut self) {
        self.propagate(&Symbol::main_symbol(), None);
    }

    fn propagate(&mut self, cur: &Symbol, spec: Option<&GenericSpecialization>) {
        let metadata = self.lib.function_metadata(cur);
        let func_id = metadata.map(|m| m.function_name(spec)).unwrap_or(cur.mangled());
        if self.visited.contains(&func_id) {
            return;
        }

        trace!(target: "spec_propagate", "Visiting {} -- {}", cur, func_id);

        self.visited.insert(func_id);

        if let Some(spec) = spec {  
            trace!(target: "spec_propagate", "Adding spec {} to {}", spec, cur);
            let metadata = self.lib.function_metadata_mut(cur).unwrap();
            metadata.specializations.push(spec.clone());
        }

        if let Some(calls) = self.call_map.get(cur) {
            let calls = calls.clone();
            for (call, call_spec) in calls {
                let call_spec = if let Some(cur_spec) = spec {
                    call_spec.map(|call_spec| {
                        trace!(target: "spec_propagate", "Generic func {} calling generic func {}", cur, call);
                        let spec_types: Vec<_> = call_spec.node_types.iter().map(|arg_type| {
                            match arg_type {
                                NodeType::Generic(_, index) => {
                                    trace!(target: "spec_propagate", "Specializing {} to {}", arg_type, cur_spec.node_types[*index]);
                                    cur_spec.node_types[*index].clone()
                                },
                                _ => arg_type.clone(),
                            }
                        }).collect();
                        GenericSpecialization::new(&call, spec_types)
                    })
                } else {
                    call_spec
                };
                self.propagate(&call, call_spec.as_ref());
            }
        }

        trace!(target: "spec_propagate", "Finished {}", cur);
    }

}

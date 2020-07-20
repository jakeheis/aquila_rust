use crate::library::*;
use super::ir::*;
use std::collections::{HashSet, HashMap};
use log::trace;

#[derive(Debug)]
pub struct SpecializationRecord {
    call_map: HashMap<Symbol, Vec<SpecRecordEntry>>,
    explicit_type_map: HashMap<Symbol, Vec<SpecRecordEntry>>,
}

impl SpecializationRecord {
    pub fn new() -> Self {
        SpecializationRecord {
            call_map: HashMap::new(),
            explicit_type_map: HashMap::new(),
        }
    }

    pub fn dump(&self) {
        for (caller, records) in self.call_map.iter() {
            println!("Calls for {}:", caller.unique_id());
            for record in records {
                println!("  {} -- {}", record.target, record.specialization);
                for cond in &record.conditions {
                    println!("    #if {}: {}", cond.gen_sym.unique_id(), cond.trait_sym.unique_id());
                }
            }
        }

        for (caller, explicit_types) in self.explicit_type_map.iter() {
            println!("Explicit types for {}:", caller.unique_id());
            for record in explicit_types {
                println!("  {} -- {}", record.target, record.specialization);
            }
        }
    }
}

#[derive(Debug)]
struct SpecRecordEntry {
    target: Symbol,
    specialization: GenericSpecialization,
    conditions: Vec<PropagateCondition>,
}

#[derive(Clone, Debug)]
struct PropagateCondition {
    gen_sym: Symbol,
    trait_sym: Symbol,
}

pub struct SpecializationRecorder<'a> {
    record: &'a mut SpecializationRecord,
    conditions: Vec<PropagateCondition>,
}

impl<'a> SpecializationRecorder<'a> {
    pub fn record(module: &'a mut Module) {
        let mut recorder = SpecializationRecorder {
            record: &mut module.specialization_record,
            conditions: vec![],
        };
        
        for func in &module.functions {
            recorder.visit_block(&func.name, &func.statements);
            for param in &func.parameters {
                recorder.add_ir_var(&func.name, param);
            }
            recorder.add_node_type(&func.name, &func.return_type);
        }
    }

    fn visit_block(&mut self, context: &Symbol, block: &[IRStatement]) {
        for stmt in block {
            self.visit_stmt(context, stmt);
        }
    }

    fn visit_stmt(&mut self, context: &Symbol, stmt: &IRStatement) {
        match stmt {
            IRStatement::DeclLocal(var) => self.add_ir_var(context, var),
            IRStatement::Assign(_, value) => {
                self.visit_expr(context, value);
            }
            IRStatement::Loop(block) => {
                self.visit_block(context, block);
            }
            IRStatement::Condition(condition, if_block, else_block) => {
                self.visit_expr(context, condition);
                self.visit_block(context, if_block);
                self.visit_block(context, else_block);
            }
            IRStatement::ConformanceCheck(gen_sym, trait_sym, block) => {
                self.conditions.push(PropagateCondition {
                    gen_sym: gen_sym.clone(),
                    trait_sym: trait_sym.clone()
                });
                self.visit_block(context, block);
                self.conditions.pop();
            }
            IRStatement::Execute(expr) => {
                self.visit_expr(context, expr);
            }
            IRStatement::Return(value) => {
                if let Some(value) = value {
                    self.visit_expr(context, value);
                }
            }
            IRStatement::Break => (),
        }
    }

    fn visit_expr(&mut self, context: &Symbol, expr: &IRExpr) {
        match &expr.kind {
            IRExprKind::FieldAccess(target, _) => {
                self.visit_expr(context, target);
            }
            IRExprKind::DerefFieldAccess(target, _) => {
                self.visit_expr(context, target);
            }
            IRExprKind::Call(function, spec, args) => {
                for arg in args {
                    self.visit_expr(context, arg);
                }

                // Don't record root extern symbol
                if !function.is_root() {
                    return;
                }

                let record = SpecRecordEntry {
                    target: function.clone(),
                    specialization: spec.clone(),
                    conditions: self.conditions.clone(),
                };
                self.record.call_map.entry(context.clone()).or_insert(Vec::new()).push(record);
            }
            IRExprKind::Binary(lhs, _, rhs) => {
                self.visit_expr(context, lhs);
                self.visit_expr(context, rhs);
            }
            IRExprKind::Unary(_, operand) => {
                self.visit_expr(context, operand);
            }
            IRExprKind::Literal(..) | IRExprKind::Variable(..) | IRExprKind::ExplicitType => (),
            IRExprKind::Cast(value) => {
                self.visit_expr(context, value);
            }
        }
    }

    fn add_ir_var(&mut self, context: &Symbol, var: &IRVariable) {
        self.add_node_type(context, &var.var_type);
    }

    fn add_node_type(&mut self, context: &Symbol, node_type: &NodeType) {
        if let NodeType::Instance(sym, spec) = node_type {
            let record = SpecRecordEntry {
                target: sym.clone(),
                specialization: spec.clone(),
                conditions: self.conditions.clone(),
            };
            self
                .record
                .explicit_type_map
                .entry(context.clone())
                .or_insert(Vec::new())
                .push(record);
        }
    }
}

pub struct FinalSpecializationMap {
    map: HashMap<Symbol, HashSet<GenericSpecialization>>,
}

impl FinalSpecializationMap {
    pub fn new() -> Self {
        FinalSpecializationMap {
            map: HashMap::new(),
        }
    }

    pub fn add_spec(&mut self, symbol: Symbol, spec: GenericSpecialization) {
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
    visited: HashSet<String>,
    map: FinalSpecializationMap,
}

impl<'a> SpecializationPropagator<'a> {
    pub fn propagate(libs: &[Module], main: Symbol) -> FinalSpecializationMap {
        // for lib in libs {
        //     lib.specialization_record.dump();
        // }

        let mut prop = SpecializationPropagator {
            libs,
            visited: HashSet::new(),
            map: FinalSpecializationMap::new(),
        };
        prop.propagate_through_function(&main, &GenericSpecialization::empty());

        prop.map
    }

    fn propagate_through_function(&mut self, cur: &Symbol, current_spec: &GenericSpecialization) {
        let func_id = cur.add_spec_suffix(current_spec);
        if self.visited.contains(&func_id) {
            return;
        }

        trace!("Visiting function  {} -- {}", cur, func_id);

        self.visited.insert(func_id);

        trace!("Adding function spec {} to {}", current_spec, cur);
        self.map.add_spec(cur.clone(), current_spec.clone());

        let lib = self.libs.iter().find(|l| &l.name == cur.lib()).unwrap();

        if let Some(calls) = lib.specialization_record.call_map.get(cur) {
            let calls = calls.clone();
            for call in calls {
                if !self.should_propagate(&current_spec, &call.conditions) {
                    trace!("Skipping {} -- {}", call.target, current_spec);
                    continue;
                }

                if let Some(owner) = call.target.owner_symbol() {
                    if let Some(NodeType::Instance(sym, spec)) = current_spec.type_for(&owner) {
                        let func_symbol = sym.child(call.target.name());
                        self.propagate_through_function(&func_symbol, spec);
                        continue;
                    }
                }

                let call_spec = call.specialization.resolve_generics_using(current_spec);
                self.propagate_through_function(&call.target, &call_spec);
            }
        }

        if let Some(explicit_types) = lib.specialization_record.explicit_type_map.get(cur) {
            let explicit_types = explicit_types.clone();
            for explicit_type in explicit_types {
                let explicit_type_spec = explicit_type.specialization.resolve_generics_using(current_spec);

                trace!("in func {} Propping from type spec {} to {}", cur, explicit_type_spec, explicit_type.target);

                self.propagate_through_type(&explicit_type.target, &explicit_type_spec);
            }
        }

        trace!("Finished function {}", cur);
    }

    fn propagate_through_type(&mut self, cur: &Symbol, current_spec: &GenericSpecialization) {
        let type_id = cur.add_spec_suffix(current_spec);

        if self.visited.insert(type_id) == false {
            return;
        }

        trace!("Adding type spec {} to {}", current_spec, cur);

        self.map.add_spec(cur.clone(), current_spec.clone());

        if let Some(type_metadata) = self.type_metadata(cur) {
            let fields = type_metadata.fields.clone();
            for field in &fields {
                if let NodeType::Instance(target, field_spec) = &field.var_type {
                    let field_spec = field_spec.resolve_generics_using(&current_spec);
                    self.propagate_through_type(target, &field_spec);
                }
            }
        }

        trace!("Finished function {}", cur);
    }

    fn should_propagate(&self, current_spec: &GenericSpecialization, conditions: &[PropagateCondition]) -> bool {
        for condition in conditions {
            let resolved_type = current_spec.type_for(&condition.gen_sym).unwrap();
            if let NodeType::Instance(type_sym, ..) = resolved_type {
                let metadata = self.type_metadata(&type_sym).unwrap();
                if !metadata.conforms_to(&condition.trait_sym) {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }

    fn type_metadata(&self, symbol: &Symbol) -> Option<&TypeMetadata> {
        for lib in self.libs {
            if let Some(metadata) = lib.symbols.get_type_metadata(symbol) {
                return Some(metadata);
            }
        }
        None
    }
}


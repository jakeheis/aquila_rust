use crate::library::*;
use super::ir::*;
use std::collections::{HashSet, HashMap};
use log::trace;

#[derive(Clone, Debug)]
pub struct CallRecord {
    function: Symbol,
    specialization: GenericSpecialization,
    conditions: Vec<PropagateCondition>,
}

#[derive(Debug)]
pub struct SpecializationRecord {
    pub call_map: HashMap<Symbol, Vec<CallRecord>>,
    pub explicit_type_map: HashMap<Symbol, Vec<CallRecord>>,
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
                println!("  {} -- {}", record.function, record.specialization);
            }
        }

        for (caller, explicit_types) in self.explicit_type_map.iter() {
            println!("Explicit types for {}:", caller.unique_id());
            for record in explicit_types {
                println!("  {} -- {}", record.function, record.specialization);
            }
        }
    }
}

#[derive(Clone, Debug)]
struct PropagateCondition {
    gen_sym: Symbol,
    trait_sym: Symbol,
}

pub struct SpecializationRecorder {
    conditions: Vec<PropagateCondition>,
    record: SpecializationRecord,
}

impl SpecializationRecorder {
    pub fn new() -> SpecializationRecorder {
        SpecializationRecorder {
            conditions: vec![],
            record: SpecializationRecord::new() 
        }
    }

    pub fn record(mut self, module: &Module) -> SpecializationRecord {
        for func in &module.functions {
            self.visit_block(&func.name, &func.statements);
            for param in &func.parameters {
                self.add_ir_var(&func.name, param);
            }
            self.add_node_type(&func.name, &func.return_type);
        }

        self.record
    }

    fn add_ir_var(&mut self, context: &Symbol, var: &IRVariable) {
        self.add_node_type(context, &var.var_type);
    }

    fn add_node_type(&mut self, context: &Symbol, node_type: &NodeType) {
        if let NodeType::Instance(sym, spec) = node_type {
            let record = CallRecord {
                function: sym.clone(),
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

                let record = CallRecord {
                    function: function.clone(),
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
    call_map: HashMap<Symbol, Vec<CallRecord>>,
    explicit_type_map: HashMap<Symbol, Vec<CallRecord>>,
    visited: HashSet<String>,
    map: FinalSpecializationMap,
}

impl<'a> SpecializationPropagator<'a> {
    pub fn propagate(libs: &[Module]) -> FinalSpecializationMap {
        // for lib in libs {
        //     lib.specialization_record.dump();
        // }

        let mut call_map: HashMap<Symbol, Vec<CallRecord>> = HashMap::new();
        let mut explicit_type_map: HashMap<Symbol, Vec<CallRecord>> = HashMap::new();

        for lib in libs {
            for (caller, callees) in lib.specialization_record.call_map.iter() {
                let all = call_map.entry(caller.clone()).or_insert(Vec::new());
                all.append(&mut callees.clone());
            }

            let lib_map = &lib.specialization_record.explicit_type_map;
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
        let func_id = cur.add_spec_suffix(current_spec);
        if self.visited.contains(&func_id) {
            return;
        }

        trace!("Visiting function  {} -- {}", cur, func_id);

        self.visited.insert(func_id);

        trace!("Adding function spec {} to {}", current_spec, cur);
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
            for call in calls {
                if let Some(owner) = call.function.owner_symbol() {
                    if let Some(NodeType::Instance(sym, spec)) = current_spec.type_for(&owner) {
                        let func_symbol = sym.child(call.function.name());
                        self.propagate_through_function(&func_symbol, spec);
                        continue;
                    }
                }

                let call_spec = call.specialization.resolve_generics_using(current_spec);
                self.propagate_through_function(&call.function, &call_spec);
            }
        }

        if let Some(explicit_types) = self.explicit_type_map.get(cur) {
            let explicit_types = explicit_types.clone();
            for explicit_type in explicit_types {
                let explicit_type_spec = explicit_type.specialization.resolve_generics_using(current_spec);

                trace!("in func {} Propping from type spec {} to {}", cur, explicit_type_spec, explicit_type.function);

                self.propagate_through_type(&explicit_type.function, &explicit_type_spec);
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

    fn type_metadata(&self, symbol: &Symbol) -> Option<&TypeMetadata> {
        for lib in self.libs {
            if let Some(metadata) = lib.symbols.get_type_metadata(symbol) {
                return Some(metadata);
            }
        }
        None
    }
}


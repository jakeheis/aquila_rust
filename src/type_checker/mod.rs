mod expr_checker;
mod node_type;
mod symbol_table;
mod type_checker;

pub use node_type::{FunctionType, NodeType};
pub use symbol_table::SymbolTableBuilder;
pub use type_checker::TypeChecker;

use crate::diagnostic::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::ContainsSpan;
use log::trace;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub enum ScopeType {
    TopLevel,
    InsideType(TypeMetadata),
    InsideMetatype(TypeMetadata),
    InsideFunction(FunctionMetadata),
}

pub struct Scope {
    id: Option<Symbol>,
    variable_types: HashMap<Symbol, NodeType>,
    scope_type: ScopeType,
}

impl Scope {
    fn new(id: Option<Symbol>, scope_type: ScopeType) -> Self {
        Scope {
            id,
            variable_types: HashMap::new(),
            scope_type,
        }
    }

    fn type_of(&self, symbol: &Symbol) -> Option<&NodeType> {
        self.variable_types.get(symbol)
    }
}

pub struct ContextTracker {
    lib: Rc<Lib>,
    scopes: Vec<Scope>,
}

impl ContextTracker {
    fn new(lib: Rc<Lib>) -> Self {
        ContextTracker {
            lib,
            scopes: vec![Scope::new(None, ScopeType::TopLevel)],
        }
    }

    fn push_scope_named(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new_str(self.current_symbol(), name);
        let copied = self.current_scope().scope_type.clone();
        self.push_scope(symbol.clone(), copied);
        symbol
    }

    fn push_scope_meta(&mut self, type_meta: &TypeMetadata) {
        let symbol = Symbol::meta_symbol(self.current_symbol());
        self.push_scope(symbol, ScopeType::InsideMetatype(type_meta.clone()));
    }

    fn push_type_scope(&mut self, name: &ResolvedToken) -> (Symbol, TypeMetadata) {
        let symbol = Symbol::new(self.current_symbol(), &name.token);
        let metadata = self.lib.type_metadata(&symbol).unwrap();
        self.push_scope(symbol.clone(), ScopeType::InsideType(metadata.clone()));
        (symbol, metadata)
    }

    fn push_function_scope(&mut self, name: &ResolvedToken) -> (Symbol, FunctionMetadata) {
        let symbol = Symbol::new(self.current_symbol(), &name.token);
        let metadata = self.lib.function_metadata(&symbol).unwrap();
        self.push_scope(symbol.clone(), ScopeType::InsideFunction(metadata.clone()));
        (symbol, metadata)
    }

    fn push_scope(&mut self, id: Symbol, scope_type: ScopeType) {
        trace!(target: "type_checker", "Pushing scope -- {}", id);
        self.scopes.push(Scope::new(Some(id), scope_type));
    }

    fn pop_scope(&mut self) {
        let popped = self.scopes.pop().unwrap();
        trace!(target: "type_checker", "Popped scope {}", popped.id.unwrap());
    }

    pub fn symbolic_context(&self) -> Vec<Symbol> {
        self.scopes
            .iter()
            .flat_map(|s| s.id.as_ref().map(|id| id.clone()))
            .collect()
    }

    pub fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn current_scope_immut(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn current_symbol(&self) -> Option<&Symbol> {
        self.scopes.last().unwrap().id.as_ref()
    }

    pub fn enclosing_type(&self) -> Option<&TypeMetadata> {
        match &self.current_scope_immut().scope_type {
            ScopeType::InsideType(t) => Some(t),
            ScopeType::InsideFunction(_) if self.scopes.len() > 1 => {
                if let ScopeType::InsideType(t) = &self.scopes[self.scopes.len() - 2].scope_type {
                    Some(t)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn enclosing_function(&self) -> Option<&FunctionMetadata> {
        if let ScopeType::InsideFunction(f) = &self.current_scope_immut().scope_type {
            Some(f)
        } else {
            None
        }
    }

    // Variables

    pub fn put_in_scope(&mut self, symbol: &Symbol, var_type: &NodeType) {
        self.current_scope()
            .variable_types
            .insert(symbol.clone(), var_type.clone());
    }

    pub fn define_var(&mut self, name: &TypedToken, var_type: &NodeType) {
        if var_type.contains_ambiguity() {
            panic!("Should never define var as ambiguous");
        }

        let new_symbol = Symbol::new((&self.current_scope().id).as_ref(), &name.token);

        trace!(target: "type_checker", "Defining {} (symbol = {}) as {}", name.span().lexeme(), new_symbol, var_type);

        self.current_scope()
            .variable_types
            .insert(new_symbol.clone(), var_type.clone());

        name.set_symbol(new_symbol);
        name.set_type(var_type.clone());
    }

    pub fn resolve_var(&self, name: &str) -> Option<(Symbol, NodeType)> {
        for scope in self.scopes.iter().rev() {
            let possible_symbol = Symbol::new_str((&scope.id).as_ref(), name);
            if let Some(node_type) = scope.type_of(&possible_symbol) {
                return Some((possible_symbol, node_type.clone()));
            }
        }

        let lib_symbol = Symbol::new_str(None, name);
        if let Some(metadata) = self.lib.function_metadata(&lib_symbol) {
            Some((lib_symbol, metadata.node_type()))
        } else {
            None
        }
    }

    pub fn resolve_explicit_type(
        &mut self,
        explicit_type: &ExplicitType,
    ) -> DiagnosticResult<NodeType> {
        let context = self.symbolic_context();

        if let Some(deduced) = explicit_type.resolve_with_lib(&self.lib, &context) {
            if let NodeType::Any = deduced {
                Err(Diagnostic::error(
                    explicit_type,
                    "Cannot have type any; must be ptr any",
                ))
            } else {
                TypeChecker::confirm_fully_specialized(self.lib.as_ref(), explicit_type, &deduced)?;
                if let NodeType::Instance(type_symbol, spec) = &deduced {
                    self.lib
                        .specialization_tracker
                        .add_required_type_spec(type_symbol.clone(), spec.clone());
                }
                Ok(deduced)
            }
        } else {
            Err(Diagnostic::error(explicit_type, "Undefined type"))
        }
    }
}

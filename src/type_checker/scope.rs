use crate::diagnostic::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::ContainsSpan;
use super::type_resolver::{TypeResolution, TypeResolutionError};
use log::trace;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub enum ScopeType {
    TopLevel,
    InsideType,
    InsideMetatype,
    InsideFunction,
    InsideTrait,
}

// use std::cell::RefCell;

// #[derive(Clone)]
// pub struct GenericParameterMetadata {
//     pub owner: Symbol,
//     pub name: String,
//     pub trait_impls: RefCell<Vec<Symbol>>,
// }

#[derive(Clone)]
pub enum ScopeDefinition {
    Variable(Symbol, NodeType),
    Function(Symbol),
    SelfVar(Symbol, NodeType),
    ExplicitType(DiagnosticResult<NodeType>),
    // GenericType(GenericParameterMetadata)
}

pub struct Scope {
    pub id: Symbol,
    definitions: HashMap<String, ScopeDefinition>,
    pub scope_type: ScopeType,
}

impl Scope {
    fn new(id: Symbol, scope_type: ScopeType) -> Self {
        Scope {
            id,
            definitions: HashMap::new(),
            scope_type,
        }
    }
}

pub struct ContextTracker {
    lib: Rc<Lib>,
    pub scopes: Vec<Scope>,
}

impl ContextTracker {
    pub fn new(lib: Rc<Lib>) -> Self {
        let root_symbol = Symbol::lib_root(&lib.name);
        ContextTracker {
            lib,
            scopes: vec![Scope::new(root_symbol, ScopeType::TopLevel)],
        }
    }

    pub fn push_subscope(&mut self) {
        let symbol = self.current_symbol().clone();
        let copied = self.current_scope().scope_type.clone();
        self.push_scope(symbol, copied);
    }

    pub fn push_scope_meta(&mut self) {
        let symbol = Symbol::meta_symbol(self.current_symbol());
        self.push_scope(symbol, ScopeType::InsideMetatype);
    }

    pub fn push_type_scope(&mut self, name: &SymbolicToken) -> (Symbol, TypeMetadata) {
        let symbol = self.current_symbol().child_token(&name.token);
        let metadata = self.lib.type_metadata(&symbol).unwrap().clone();
        self.push_scope(symbol.clone(), ScopeType::InsideType);
        (symbol, metadata)
    }

    pub fn push_function_scope(&mut self, name: &SymbolicToken) -> (Symbol, FunctionMetadata) {
        let symbol = self.current_symbol().child_token(&name.token);
        let metadata = self.lib.function_metadata(&symbol).unwrap().clone();
        self.push_scope(symbol.clone(), ScopeType::InsideFunction);
        (symbol, metadata)
    }

    pub fn push_scope(&mut self, id: Symbol, scope_type: ScopeType) {
        trace!(target: "type_checker", "Pushing scope -- {}", id);
        self.scopes.push(Scope::new(id, scope_type));
    }

    pub fn pop_scope(&mut self) {
        let popped = self.scopes.pop().unwrap();
        trace!(target: "type_checker", "Popped scope {}", popped.id);
    }

    pub fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn current_scope_immut(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn current_symbol(&self) -> &Symbol {
        &self.scopes.last().unwrap().id
    }

    pub fn enclosing_function(&self) -> Symbol {
        if let ScopeType::InsideFunction = &self.current_scope_immut().scope_type {
            self.current_symbol().clone()
        } else {
            Symbol::main_symbol()
        }
    }

    // Variables

    pub fn put_in_scope(&mut self, name: String, definition: ScopeDefinition) {
        self.current_scope()
            .definitions
            .insert(name, definition);
    }

    pub fn define_variable(&mut self, name: &SymbolicToken, var_type: &NodeType) {
        if var_type.contains_ambiguity() {
            panic!("Should never define var as ambiguous");
        }

        let new_symbol = self.current_symbol().child_token(&name.token);

        trace!(target: "type_checker", "Defining {} (symbol = {}) as {}", name.span().lexeme(), new_symbol, var_type);

        let definition = ScopeDefinition::Variable(new_symbol.clone(), var_type.clone());
        self.put_in_scope(name.token.lexeme().to_owned(), definition);

        name.set_symbol(new_symbol);
    }

    pub fn resolve_token(&self, token: &SpecializedToken) -> Option<ScopeDefinition> {
        for scope in self.scopes.iter().rev() {
            if let Some(definition) = scope.definitions.get(token.token.lexeme()) {
                return Some(definition.clone());
            }
        }

        if let Some(metadata) = self.lib.top_level_function_named(token.token.lexeme()) {
            return Some(ScopeDefinition::Function(metadata.symbol.clone()));
        }

        let enclosing_func = self.enclosing_function();
        let resolver = TypeResolution::new(&self.lib, &self.lib.symbols, &self, Some(&enclosing_func));
        match resolver.resolve_simple(token) {
            Ok(resolved_type) => Some(ScopeDefinition::ExplicitType(Ok(resolved_type))),
            Err(TypeResolutionError::NotFound) => None,
            Err(TypeResolutionError::Inaccessible(diag)) | Err(TypeResolutionError::IncorrectlySpecialized(diag)) => {
                Some(ScopeDefinition::ExplicitType(Err(diag)))
            }
        }
    }

    pub fn resolve_type(&self, explicit_type: &ExplicitType) -> DiagnosticResult<NodeType> {
        let enclosing_func = self.enclosing_function();
        let resolver = TypeResolution::new(&self.lib, &self.lib.symbols, &self, Some(&enclosing_func));
        match resolver.resolve(explicit_type) {
            Ok(resolved_type) => Ok(resolved_type),
            Err(error) => Err(match error {
                TypeResolutionError::IncorrectlySpecialized(diag)
                | TypeResolutionError::Inaccessible(diag) => diag,
                TypeResolutionError::NotFound => Diagnostic::error(explicit_type, "Type not found"),
            }),
        }
    }
}

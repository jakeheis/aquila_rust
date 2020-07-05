mod expr_checker;
mod symbol_table_builder;
mod type_checker;
mod type_resolver;

pub use symbol_table_builder::SymbolTableBuilder;
pub use type_checker::TypeChecker;
pub use type_resolver::{TypeResolution, TypeResolutionError};

use crate::diagnostic::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::ContainsSpan;
use log::trace;
use std::collections::HashMap;
use std::rc::Rc;

mod check {
    use super::NodeType;
    use crate::diagnostic::*;
    use crate::library::*;
    use crate::parsing::*;
    use crate::source::ContainsSpan;

    pub fn type_mismatch<T: ContainsSpan>(
        span: &T,
        given: &NodeType,
        expected: &NodeType,
    ) -> Diagnostic {
        let message = format!("Expected {}, got {}", expected, given);
        Diagnostic::error(span, &message)
    }

    pub fn ensure_no_amibguity(expr: &Expr, node_type: &NodeType) -> DiagnosticResult<()> {
        if node_type.contains_ambiguity() {
            Err(Diagnostic::error(expr, "Cannot infer type"))
        } else {
            Ok(())
        }
    }

    pub fn check_type_match(
        expr: &Expr,
        given: &NodeType,
        expected: &NodeType,
    ) -> DiagnosticResult<()> {
        if given.matches(expected) {
            Ok(())
        } else {
            Err(type_mismatch(expr, given, expected))
        }
    }

    pub fn type_accessible(lib: &Lib, metadata: &TypeMetadata) -> bool {
        if metadata.is_public {
            true
        } else {
            symbol_accessible(lib, &metadata.symbol)
        }
    }

    pub fn func_accessible(lib: &Lib, metadata: &FunctionMetadata) -> bool {
        if metadata.is_public {
            true
        } else {
            symbol_accessible(lib, &metadata.symbol)
        }
    }

    pub fn symbol_accessible(lib: &Lib, symbol: &Symbol) -> bool {
        symbol.lib_component() == Symbol::lib_root(lib).lib_component()
    }
}

#[derive(Clone)]
pub enum ScopeType {
    TopLevel,
    InsideType(TypeMetadata),
    InsideMetatype(TypeMetadata),
    InsideTraitImpl(TypeMetadata, TraitMetadata),
    InsideFunction(FunctionMetadata),
}

pub struct Scope {
    id: Symbol,
    variable_types: HashMap<Symbol, NodeType>,
    scope_type: ScopeType,
}

impl Scope {
    fn new(id: Symbol, scope_type: ScopeType) -> Self {
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
        let root_symbol = Symbol::lib_root(lib.as_ref());
        ContextTracker {
            lib,
            scopes: vec![Scope::new(root_symbol, ScopeType::TopLevel)],
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

    fn push_type_scope(&mut self, name: &SymbolicToken) -> (Symbol, TypeMetadata) {
        let symbol = Symbol::new(self.current_symbol(), &name.token);
        let metadata = self.lib.type_metadata(&symbol).unwrap().clone();
        self.push_scope(symbol.clone(), ScopeType::InsideType(metadata.clone()));
        (symbol, metadata)
    }

    fn push_function_scope(&mut self, name: &SymbolicToken) -> (Symbol, FunctionMetadata) {
        let symbol = Symbol::new(self.current_symbol(), &name.token);
        let metadata = self.lib.function_metadata(&symbol).unwrap().clone();
        self.push_scope(symbol.clone(), ScopeType::InsideFunction(metadata.clone()));
        (symbol, metadata)
    }

    // fn push_trait_impl_scope(&mut self, type_name: &ResolvedToken, trait_name: &Symbol) -> Symbol {
    //     let type_symbol = Symbol::new(self.current_symbol(), &type_name.token);
    //     let type_metadata = self.lib.type_metadata(&type_symbol).unwrap();
    //     let trait_metadata = self.lib.trait_metadata_symbol(trait_name).unwrap();

    //     self.push_scope(type_symbol.clone(), ScopeType::InsideTraitImpl(type_metadata, trait_metadata));
    //     type_symbol
    // }

    fn push_scope(&mut self, id: Symbol, scope_type: ScopeType) {
        trace!(target: "type_checker", "Pushing scope -- {}", id);
        self.scopes.push(Scope::new(id, scope_type));
    }

    fn pop_scope(&mut self) {
        let popped = self.scopes.pop().unwrap();
        trace!(target: "type_checker", "Popped scope {}", popped.id);
    }

    pub fn symbolic_context(&self) -> Vec<Symbol> {
        self.scopes.iter().map(|s| s.id.clone()).collect()
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

    pub fn enclosing_type(&self) -> Option<&TypeMetadata> {
        match &self.current_scope_immut().scope_type {
            ScopeType::InsideType(t) | ScopeType::InsideTraitImpl(t, _) => Some(t),
            ScopeType::InsideFunction(_) if self.scopes.len() > 1 => {
                let grandparent = &self.scopes[self.scopes.len() - 2].scope_type;
                if let ScopeType::InsideType(t) | ScopeType::InsideTraitImpl(t, _) = grandparent {
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

    pub fn enclosing_trait_impl(&self) -> Option<&TraitMetadata> {
        if let ScopeType::InsideTraitImpl(_, trait_met) = &self.current_scope_immut().scope_type {
            Some(trait_met)
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

        let new_symbol = Symbol::new(&self.current_scope().id, &name.token);

        trace!(target: "type_checker", "Defining {} (symbol = {}) as {}", name.span().lexeme(), new_symbol, var_type);

        self.current_scope()
            .variable_types
            .insert(new_symbol.clone(), var_type.clone());

        name.set_symbol(new_symbol);
        name.set_type(var_type.clone());
    }

    pub fn resolve_var(&self, name: &str) -> Option<(Symbol, NodeType)> {
        for scope in self.scopes.iter().rev() {
            let possible_symbol = Symbol::new_str(&scope.id, name);
            if let Some(node_type) = scope.type_of(&possible_symbol) {
                return Some((possible_symbol, node_type.clone()));
            }
        }

        if let Some(metadata) = self.lib.top_level_function_named(name) {
            Some((metadata.symbol.clone(), metadata.node_type()))
        } else {
            None
        }
    }

    fn resolve_type(&self, explicit_type: &ExplicitType) -> DiagnosticResult<NodeType> {
        let context = self.symbolic_context();
        let enclosing_func = self.enclosing_function().map(|f| &f.symbol);
        let resolver = TypeResolution::new(&self.lib, &self.lib.symbols, &context, enclosing_func);
        match resolver.resolve(explicit_type) {
            Ok(resolved_type) => Ok(resolved_type),
            Err(error) => Err(match error {
                TypeResolutionError::IncorrectlySpecialized(diag)
                | TypeResolutionError::Inaccessible(diag) => diag,
                TypeResolutionError::NotFound => Diagnostic::error(explicit_type, "Type not found"),
            }),
        }
    }

    fn resolve_token_as_type(
        &self,
        token: &SpecializedToken,
    ) -> Result<NodeType, TypeResolutionError> {
        let context = self.symbolic_context();
        let enclosing_func = self.enclosing_function().map(|f| &f.symbol);
        let resolver = TypeResolution::new(&self.lib, &self.lib.symbols, &context, enclosing_func);
        resolver.resolve_simple(token)
    }
}

use crate::diagnostic::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::ContainsSpan;
use log::trace;
use std::collections::HashMap;
use super::check;
use crate::lexing::Token;

#[derive(Clone, Debug)]
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
    ExplicitType(DiagnosticResult<NodeType>),
    // GenericType(GenericParameterMetadata)
}

pub struct Scope {
    pub id: Symbol,
    definitions: HashMap<String, ScopeDefinition>,
    pub scope_type: ScopeType,
    pub generic_restrictions: HashMap<Symbol, Vec<Symbol>>,
}

impl Scope {
    fn new(id: Symbol, scope_type: ScopeType) -> Self {
        Scope {
            id,
            definitions: HashMap::new(),
            scope_type,
            generic_restrictions: HashMap::new()
        }
    }
}

pub struct ContextTracker {
    scopes: Vec<Scope>,
}

impl ContextTracker {
    pub fn new(lib: Symbol) -> Self {
        ContextTracker {
            scopes: vec![Scope::new(lib, ScopeType::TopLevel)],
        }
    }

    pub fn push_subscope(&mut self) {
        let symbol = self.current_symbol().clone();
        let copied = self.current_scope().scope_type.clone();
        self.push_scope(symbol, copied);
    }

    pub fn push_meta_scope(&mut self) {
        let symbol = Symbol::meta_symbol(self.current_symbol());
        self.push_scope(symbol, ScopeType::InsideMetatype);
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

    // Variables

    pub fn put_in_scope(&mut self, name: String, definition: ScopeDefinition) {
        self.current_scope().definitions.insert(name, definition);
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

    pub fn symbol_resolver<'a>(&'a self, lib_syms: &'a SymbolTable, dep_syms: &'a SymbolStore) -> SymbolResolution<'a> {
        SymbolResolution {
            scopes: &self.scopes,
            lib_syms,
            dependencies: dep_syms,
        }
    }
}

pub struct SymbolResolution<'a> {
    pub scopes: &'a [Scope],
    lib_syms: &'a SymbolTable,
    dependencies: &'a SymbolStore,
}

impl<'a> SymbolResolution<'a> {
    pub fn resolve_token(&self, token: &SpecializedToken) -> Option<ScopeDefinition> {
        // Check for local variable
        for scope in self.scopes.iter().rev() {
            if let Some(definition) = scope.definitions.get(token.token.lexeme()) {
                return Some(definition.clone());
            }
        }

        // Check for top level function
        let this_lib = self.lib_syms.lib.child(token.token.lexeme());
        if self.lib_syms.get_func_metadata(&this_lib).is_some() {
            return Some(ScopeDefinition::Function(this_lib));
        } else if let Some(metadata) = self.dependencies.top_level_function_named(token.token.lexeme()) {
            return Some(ScopeDefinition::Function(metadata.symbol.clone()));
        }

        // Check for explicit type
        match self.resolve_specialized_token(token) {
            TypeResolutionResult::Found(resolved_type) => {
                Some(ScopeDefinition::ExplicitType(Ok(resolved_type)))
            }
            TypeResolutionResult::Error(diag) => Some(ScopeDefinition::ExplicitType(Err(diag))),
            TypeResolutionResult::NotFound => None,
        }
    }

    pub fn resolve_generic(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            let generics = match scope.scope_type {
                ScopeType::InsideFunction => {
                    let function = self.lib_syms.get_func_metadata(&scope.id);
                    function.map(|f| f.generics.as_slice()).unwrap_or(&[])
                }
                ScopeType::InsideType => {
                    let ty = self.lib_syms.get_type_metadata(&scope.id).unwrap();
                    &ty.generics
                }
                ScopeType::InsideTrait | ScopeType::TopLevel | ScopeType::InsideMetatype => continue,
            };

            if generics.iter().any(|g| g == name) {
                return Some(scope.id.child(name));
            }
        }
        None
    }
    
    pub fn resolve_explicit_type_or_err(&self, explicit_type: &ExplicitType) -> DiagnosticResult<NodeType> {
        match self.resolve_explicit_type(explicit_type) {
            TypeResolutionResult::Found(resolved_type) => Ok(resolved_type),
            TypeResolutionResult::NotFound => {
                Err(Diagnostic::error(explicit_type, "Type not found"))
            }
            TypeResolutionResult::Error(diag) => Err(diag),
        }
    }

    pub fn resolve_explicit_type(&self, explicit_type: &ExplicitType) -> TypeResolutionResult {
        let node_type = match &explicit_type.kind {
            ExplicitTypeKind::Simple(token) => return self.resolve_specialized_token(token),
            ExplicitTypeKind::Pointer(to) => match self.resolve_explicit_type(to.as_ref()) {
                TypeResolutionResult::Found(t) => NodeType::Pointer(Box::new(t)),
                other => return other,
            },
            ExplicitTypeKind::Array(of, count_token) => match self.resolve_explicit_type(of.as_ref()) {
                TypeResolutionResult::Found(t) => {
                    let count = count_token.lexeme().parse::<usize>().ok().unwrap();
                    NodeType::Array(Box::new(t), count)
                }
                other => return other,
            },
        };

        TypeResolutionResult::Found(node_type)
    }

    pub fn resolve_specialized_token(&self, token: &SpecializedToken) -> TypeResolutionResult {
        let mut resolved_spec = Vec::new();
        for explicit_spec in &token.specialization {
            match self.resolve_explicit_type(explicit_spec) {
                TypeResolutionResult::Found(t) => resolved_spec.push(t),
                other => return other,
            }
        }

        if let Some(primitive) = NodeType::primitive(token.token.lexeme()) {
            if resolved_spec.is_empty() {
                TypeResolutionResult::Found(primitive)
            } else {
                let diagnostic = Diagnostic::error(token, "Cannot specialize a primitive");
                TypeResolutionResult::Error(diagnostic)
            }
        } else {
            self.search_for_type_token(&token.token, resolved_spec)
        }
    }

    fn search_for_type_token(
        &self,
        token: &Token,
        specialization: Vec<NodeType>,
    ) -> TypeResolutionResult {
        trace!(target: "symbol_table", "Trying to find symbol for {} -- ({})", token.lexeme(), token.span.entire_line().0);

        if let Some(sym) = self.resolve_generic(token.lexeme()) {
            trace!(target: "symbol_table", "Resolving {} as generic {}", token.lexeme(), sym);
            return self.create_gen_instance(token, &sym, specialization);
        }

        let top_level = self.scopes[0].id.child(token.lexeme());
        if let Some(ty) = self.lib_syms.get_type_metadata(&top_level) {
            trace!(target: "symbol_table", "Resolving {} as {}", token.lexeme(), top_level);
            return self.create_instance(token, &ty, specialization);
        }

        let dep_type = self.dependencies.type_metadata_named(token.lexeme());
        if let Some(dep_type) = dep_type {
            trace!(target: "symbol_table", "Resolving {} as other lib {}", token.lexeme(), dep_type.symbol);
            return self.create_instance(token, &dep_type, specialization);
        } else {
            TypeResolutionResult::NotFound
        }
    }

    fn create_gen_instance(&self,
        token: &Token,
        name: &Symbol,
        specialization: Vec<NodeType>,
    ) -> TypeResolutionResult {
        if specialization.len() != 0 {
            let message = format!(
                "Expected 0 specializations, got {}",
                specialization.len()
            );
            return TypeResolutionResult::Error(Diagnostic::error(token, &message));
        }

        let instance_type = NodeType::Instance(name.clone(), GenericSpecialization::empty());

        TypeResolutionResult::Found(instance_type)
    }

    fn create_instance(
        &self,
        token: &Token,
        type_metadata: &TypeMetadata,
        specialization: Vec<NodeType>,
    ) -> TypeResolutionResult {
        if specialization.len() != type_metadata.generics.len() {
            let message = format!(
                "Expected {} specializations, got {}",
                type_metadata.generics.len(),
                specialization.len()
            );
            return TypeResolutionResult::Error(Diagnostic::error(token, &message));
        }

        if check::type_accessible(type_metadata, &self.lib_syms.lib) == false {
            return TypeResolutionResult::Error(Diagnostic::error(token, "Type is private"));
        }

        let specialization = GenericSpecialization::new(
            &type_metadata.symbol,
            &type_metadata.generics,
            specialization,
        );

        let instance_type = NodeType::Instance(type_metadata.symbol.clone(), specialization);

        TypeResolutionResult::Found(instance_type)
    }
}

pub enum TypeResolutionResult {
    Found(NodeType),
    Error(Diagnostic),
    NotFound,
}

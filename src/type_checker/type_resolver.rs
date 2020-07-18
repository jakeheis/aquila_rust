use super::check;
use super::scope::ContextTracker;
use crate::diagnostic::*;
use crate::lexing::Token;
use crate::library::*;
use crate::parsing::{ExplicitType, ExplicitTypeKind, SpecializedToken};
use log::trace;

pub enum TypeResolutionResult {
    Found(NodeType),
    Error(Diagnostic),
    NotFound,
}

pub struct TypeResolution<'a> {
    context: &'a ContextTracker,
    symbols: &'a SymbolTable,
    enclosing_function: Option<&'a Symbol>,
}

impl<'a> TypeResolution<'a> {
    pub fn new(
        context: &'a ContextTracker,
        symbols: &'a SymbolTable,
        enclosing_function: Option<&'a Symbol>,
    ) -> Self {
        TypeResolution {
            context,
            symbols,
            enclosing_function,
        }
    }

    pub fn resolve(&self, explicit_type: &ExplicitType) -> TypeResolutionResult {
        let node_type = match &explicit_type.kind {
            ExplicitTypeKind::Simple(token) => return self.resolve_simple(token),
            ExplicitTypeKind::Pointer(to) => match self.resolve(to.as_ref()) {
                TypeResolutionResult::Found(t) => NodeType::Pointer(Box::new(t)),
                other => return other,
            },
            ExplicitTypeKind::Array(of, count_token) => match self.resolve(of.as_ref()) {
                TypeResolutionResult::Found(t) => {
                    let count = count_token.lexeme().parse::<usize>().ok().unwrap();
                    NodeType::Array(Box::new(t), count)
                }
                other => return other,
            },
        };

        TypeResolutionResult::Found(node_type)
    }

    pub fn resolve_simple(&self, token: &SpecializedToken) -> TypeResolutionResult {
        let mut resolved_spec = Vec::new();
        for explicit_spec in &token.specialization {
            match self.resolve(explicit_spec) {
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

        for parent in self.context.scopes.iter().rev() {
            let non_top_level_symbol = parent.id.child_token(token);
            if let Some(type_metadata) = self.symbols.get_type_metadata(&non_top_level_symbol) {
                trace!(target: "symbol_table", "Resolving {} as {}", token.lexeme(), non_top_level_symbol);
                return self.create_instance(token, type_metadata, specialization);
            }
        }

        let found_metadata = self.context.lib.type_metadata_named(token.lexeme());
        if let Some(type_metadata) = found_metadata {
            trace!(target: "symbol_table", "Resolving {} as other lib {}", token.lexeme(), type_metadata.symbol);
            return self.create_instance(token, &type_metadata, specialization);
        } else {
            TypeResolutionResult::NotFound
        }
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

        if check::type_accessible(self.context.lib.as_ref(), type_metadata) == false {
            return TypeResolutionResult::Error(Diagnostic::error(token, "Type is private"));
        }

        let specialization = GenericSpecialization::new(
            &type_metadata.symbol,
            &type_metadata.generics,
            specialization,
        );

        if let Some(enclosing_func) = self.enclosing_function {
            self.context
                .lib
                .specialization_tracker
                .add_required_type_spec(
                    enclosing_func.clone(),
                    type_metadata.symbol.clone(),
                    specialization.clone(),
                );
        }

        let instance_type = NodeType::Instance(type_metadata.symbol.clone(), specialization);

        TypeResolutionResult::Found(instance_type)
    }
}

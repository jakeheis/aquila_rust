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
}

impl<'a> TypeResolution<'a> {
    pub fn new(
        context: &'a ContextTracker,
        symbols: &'a SymbolTable,
    ) -> Self {
        TypeResolution {
            context,
            symbols,
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

        if let Some(sym) = self.context.resolve_generic(token.lexeme(), self.symbols) {
            trace!(target: "symbol_table", "Resolving {} as generic {}", token.lexeme(), sym);
            return self.create_gen_instance(token, &sym, specialization);
        }

        let top_level = self.context.scopes[0].id.child(token.lexeme());
        if let Some(ty) = self.symbols.get_type_metadata(&top_level) {
            trace!(target: "symbol_table", "Resolving {} as {}", token.lexeme(), top_level);
            return self.create_instance(token, &ty, specialization);
        }

        let found_metadata = self.context.lib.type_metadata_named(token.lexeme());
        if let Some(type_metadata) = found_metadata {
            trace!(target: "symbol_table", "Resolving {} as other lib {}", token.lexeme(), type_metadata.symbol);
            return self.create_instance(token, &type_metadata, specialization);
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

        let specialization = GenericSpecialization::empty();

        // if let Some(enclosing_func) = self.enclosing_function {
            // self.context
            //     .lib
            //     .specialization_tracker
            //     .add_required_type_spec(
            //         enclosing_func.clone(),
            //         name.clone(),
            //         specialization.clone(),
            //     );
        // }

        let instance_type = NodeType::Instance(name.clone(), specialization);

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

        if check::type_accessible(self.context.lib.as_ref(), type_metadata) == false {
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

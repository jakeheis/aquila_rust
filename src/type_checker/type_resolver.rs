use super::check;
use crate::diagnostic::*;
use crate::lexing::Token;
use crate::library::*;
use crate::parsing::{ExplicitType, ExplicitTypeKind, SpecializedToken};
use log::trace;

pub enum TypeResolutionError {
    IncorrectlySpecialized(Diagnostic),
    Inaccessible(Diagnostic),
    NotFound,
}

pub type TypeResolutionResult = Result<NodeType, TypeResolutionError>;

pub struct TypeResolution<'a> {
    lib: &'a Lib,
    symbols: &'a SymbolTable,
    context: &'a [Symbol],
    enclosing_function: Option<&'a Symbol>,
}

impl<'a> TypeResolution<'a> {
    pub fn new(
        lib: &'a Lib,
        symbols: &'a SymbolTable,
        context: &'a [Symbol],
        enclosing_function: Option<&'a Symbol>,
    ) -> Self {
        TypeResolution {
            lib,
            symbols,
            context,
            enclosing_function,
        }
    }

    pub fn resolve(&self, explicit_type: &ExplicitType) -> TypeResolutionResult {
        let node_type = match &explicit_type.kind {
            ExplicitTypeKind::Simple(token) => self.resolve_simple(token)?,
            ExplicitTypeKind::Pointer(to) => {
                let inner = self.resolve(to.as_ref())?;
                NodeType::Pointer(Box::new(inner))
            }
            ExplicitTypeKind::Array(of, count_token) => {
                let inner = self.resolve(of.as_ref())?;
                let count = count_token.lexeme().parse::<usize>().ok().unwrap();
                NodeType::Array(Box::new(inner), count)
            }
        };

        explicit_type.cached_type.replace(Some(node_type.clone()));
        Ok(node_type)
    }

    pub fn resolve_simple(&self, token: &SpecializedToken) -> TypeResolutionResult {
        let mut resolved_spec = Vec::new();
        for explicit_spec in &token.specialization {
            resolved_spec.push(self.resolve(explicit_spec)?);
        }

        if let Some(primitive) = NodeType::primitive(token.token.lexeme()) {
            if resolved_spec.is_empty() {
                Ok(primitive)
            } else {
                let diagnostic = Diagnostic::error(token, "Cannot specialize a primitive");
                Err(TypeResolutionError::IncorrectlySpecialized(diagnostic))
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

        for parent in self.context.iter().rev() {
            let non_top_level_symbol = parent.child_token(token);
            if let Some(type_metadata) = self.symbols.get_type_metadata(&non_top_level_symbol) {
                trace!(target: "symbol_table", "Resolving {} as {}", token.lexeme(), non_top_level_symbol);
                return self.create_instance(token, type_metadata, specialization);
            }
        }

        let found_metadata = self.lib.type_metadata_named(token.lexeme());
        if let Some(type_metadata) = found_metadata {
            trace!(target: "symbol_table", "Resolving {} as other lib {}", token.lexeme(), type_metadata.symbol);
            return self.create_instance(token, &type_metadata, specialization);
        } else {
            Err(TypeResolutionError::NotFound)
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
            return Err(TypeResolutionError::IncorrectlySpecialized(
                Diagnostic::error(token, &message),
            ));
        }

        if check::type_accessible(self.lib, type_metadata) == false {
            return Err(TypeResolutionError::Inaccessible(Diagnostic::error(
                token,
                "Type is private",
            )));
        }

        let specialization = GenericSpecialization::new(&type_metadata.symbol, &type_metadata.generics, specialization);

        if let Some(enclosing_func) = self.enclosing_function {
            self.lib.specialization_tracker.add_required_type_spec(
                enclosing_func.clone(),
                type_metadata.symbol.clone(),
                specialization.clone(),
            );
        }

        let instance_type = NodeType::Instance(type_metadata.symbol.clone(), specialization);

        Ok(instance_type)
    }
}

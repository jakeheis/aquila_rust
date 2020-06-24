use crate::lexing::Token;
use crate::library::*;
use crate::parsing::{ExplicitType, ExplicitTypeKind, ResolvedToken};
use log::trace;

pub struct TypeResolution {}

impl TypeResolution {
    pub fn resolve_with_lib(et: &ExplicitType, lib: &Lib, context: &[Symbol]) -> Option<NodeType> {
        TypeResolution::resolve(et, &lib.symbols, &lib.dependencies, context)
    }

    pub fn resolve(
        et: &ExplicitType,
        table: &SymbolTable,
        deps: &[Lib],
        context: &[Symbol],
    ) -> Option<NodeType> {
        let result = TypeResolution::deduce_from(et, table, deps, context);
        et.cached_type.replace(result.clone());
        result
    }

    pub fn deduce_from(
        explicit_type: &ExplicitType,
        table: &SymbolTable,
        deps: &[Lib],
        context: &[Symbol],
    ) -> Option<NodeType> {
        let node_type = match &explicit_type.kind {
            ExplicitTypeKind::Simple(token) => {
                TypeResolution::deduce_from_simple_explicit(token, table, deps, context)?
            }
            ExplicitTypeKind::Pointer(to) => {
                let inner = TypeResolution::deduce_from(to.as_ref(), table, deps, context)?;
                NodeType::Pointer(Box::new(inner))
            }
            ExplicitTypeKind::Array(of, count_token) => {
                let inner = TypeResolution::deduce_from(of.as_ref(), table, deps, context)?;
                let count = count_token.lexeme().parse::<usize>().ok().unwrap();
                NodeType::Array(Box::new(inner), count)
            }
        };

        Some(node_type)
    }

    pub fn deduce_from_simple_explicit(
        token: &ResolvedToken,
        table: &SymbolTable,
        deps: &[Lib],
        context: &[Symbol],
    ) -> Option<NodeType> {
        let mut resolved_spec = Vec::new();
        for explicit_spec in &token.specialization {
            resolved_spec.push(TypeResolution::resolve(
                explicit_spec,
                table,
                deps,
                context,
            )?);
        }

        if let Some(primitive) = NodeType::primitive(token.token.lexeme()) {
            Some(primitive)
        } else {
            TypeResolution::search_for_type_token(&token.token, table, deps, context, resolved_spec)
        }
    }

    fn search_for_type_token(
        token: &Token,
        table: &SymbolTable,
        deps: &[Lib],
        context: &[Symbol],
        specialization: Vec<NodeType>,
    ) -> Option<NodeType> {
        trace!(target: "symbol_table", "Trying to find symbol for {} -- ({})", token.lexeme(), token.span.entire_line().0);

        for parent in context.iter().rev() {
            let non_top_level_symbol = Symbol::new(Some(parent), token);
            if let Some(type_metadata) = table.get_type_metadata(&non_top_level_symbol) {
                let instance_type = NodeType::Instance(
                    non_top_level_symbol.clone(),
                    GenericSpecialization::new(&type_metadata.generics, specialization),
                );
                trace!(target: "symbol_table", "Resolving {} as {}", token.lexeme(), instance_type);
                return Some(instance_type);
            }
        }

        let top_level_symbol = Symbol::new(None, token);
        if let Some(type_metadata) = table.get_type_metadata(&top_level_symbol) {
            let spec = GenericSpecialization::new(&type_metadata.generics, specialization);
            let instance_type = NodeType::Instance(top_level_symbol.clone(), spec);

            trace!(target: "symbol_table", "Resolving {} as Type({})", token.lexeme(), instance_type);
            Some(instance_type)
        } else {
            for dep in deps {
                if let Some(type_metadata) = dep.type_metadata(&top_level_symbol) {
                    let instance_type = NodeType::Instance(
                        top_level_symbol.clone(),
                        GenericSpecialization::new(&type_metadata.generics, specialization),
                    );
                    trace!(target: "symbol_table", "Resolving {} as other lib Type({})", token.lexeme(), instance_type);
                    return Some(instance_type);
                }
            }
            None
        }
    }
}

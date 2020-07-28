mod expr_checker;
mod scope;
mod symbol_table_builder;
mod type_checker;

pub use symbol_table_builder::SymbolTableBuilder;
pub use type_checker::TypeChecker;

mod check {
    use crate::diagnostic::*;
    use crate::library::*;
    use crate::parsing::*;

    pub fn ensure_no_amibguity(expr: &Expr, node_type: &NodeType) -> DiagnosticResult<()> {
        if node_type.contains_ambiguity() {
            Err(Diagnostic::error(expr, "Cannot infer type"))
        } else {
            Ok(())
        }
    }

    pub fn check_type_match<T: ContainsSpan>(
        span: &T,
        given: &NodeType,
        expected: &NodeType,
    ) -> DiagnosticResult<()> {
        if given.matches(expected) {
            Ok(())
        } else {
            let message = format!("Expected {}, got {}", expected, given);
            Err(Diagnostic::error(span, &message))
        }
    }

    pub fn type_accessible(metadata: &TypeMetadata, from_lib: &Symbol) -> bool {
        if metadata.is_public {
            true
        } else {
            symbol_accessible(&metadata.symbol, from_lib)
        }
    }

    pub fn func_accessible(metadata: &FunctionMetadata, from_lib: &Symbol) -> bool {
        if metadata.is_public {
            true
        } else {
            symbol_accessible(&metadata.symbol, from_lib)
        }
    }

    pub fn symbol_accessible(symbol: &Symbol, from_lib: &Symbol) -> bool {
        symbol.lib() == from_lib.lib()
    }
}

mod generic_inference {

    use crate::library::{FunctionMetadata, NodeType, Symbol, GenericSpecialization};
    use std::collections::HashMap;
    use crate::diagnostic::*;
    use crate::parsing::Expr;

    pub fn infer_from_args(
        metadata: &FunctionMetadata,
        arg_types: &[NodeType],
        requirements: &GenericSpecialization,
        expr: &Span,
        args: &[Expr],
    ) -> DiagnosticResult<GenericSpecialization> {
        let mut spec_map: HashMap<Symbol, NodeType> = HashMap::new();
        for gen in &metadata.generics {
            let sym = metadata.symbol.child(&gen);
            spec_map.insert(sym, NodeType::Ambiguous);
        }

        let iterator = metadata.parameters.iter().zip(arg_types).rev().enumerate();
        for (index, (param, arg_type)) in iterator {
            if let Some((sym, node_type)) = infer_generic_type(&param.var_type, arg_type) {
                if let Some(requirement ) = requirements.type_for(&sym) {
                    super::check::check_type_match(&args[index], &node_type, requirement)?;
                }
                spec_map.insert(sym, node_type);
            }
        }
        for (symbol, spec) in spec_map.iter() {
            if spec.contains_ambiguity() {
                let message = format!(
                    "Couldn't infer generic type {}",
                    symbol.name()
                );
                return Err(Diagnostic::error(expr, &message));
            }
        }

        Ok(GenericSpecialization { map: spec_map })
    }

    fn infer_generic_type(
        param: &NodeType,
        arg: &NodeType,
    ) -> Option<(Symbol, NodeType)> {
        match (param, arg) {
            (NodeType::GenericInstance(symbol), arg) => Some((symbol.clone(), arg.clone())),
            (NodeType::Pointer(param_to), NodeType::Pointer(arg_to)) => {
                infer_generic_type(param_to, arg_to)
            }
            (NodeType::Reference(param_to), NodeType::Reference(arg_to)) => {
                infer_generic_type(param_to, arg_to)
            }
            _ => None,
        }
    }

}

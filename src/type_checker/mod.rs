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

    pub fn check_type_match(
        expr: &Expr,
        given: &NodeType,
        expected: &NodeType,
    ) -> DiagnosticResult<()> {
        if given.matches(expected) {
            Ok(())
        } else {
            let message = format!("Expected {}, got {}", expected, given);
            Err(Diagnostic::error(expr, &message))
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

    pub fn infer_from_args(
        metadata: &FunctionMetadata,
        arg_types: &[NodeType],
    ) -> Result<GenericSpecialization, Symbol> {
        let mut spec_map: HashMap<Symbol, NodeType> = HashMap::new();
        for gen in &metadata.generics {
            let sym = metadata.symbol.child(&gen);
            spec_map.insert(sym, NodeType::Ambiguous);
        }

        for (param, arg_type) in metadata.parameters.iter().zip(arg_types).rev() {
            if let Some(inferred) = infer_generic_type(&param.var_type, arg_type) {
                spec_map.insert(inferred.0, inferred.1);
            }
        }
        for (symbol, spec) in spec_map.iter() {
            if spec.contains_ambiguity() {
                return Err(symbol.clone());
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

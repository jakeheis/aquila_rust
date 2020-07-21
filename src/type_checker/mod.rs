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

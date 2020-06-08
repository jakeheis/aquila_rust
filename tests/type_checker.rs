use aquila::analysis::*;
use aquila::parsing::*;

mod common;
use common::*;

#[test]
fn cant_infer_type() -> TestResult {
    assert_failure(
        Stmt::variable_decl(test_token::var_name(), None, None),
        &[Diagnostic::error(&test_span::new(0, 8), "Can't infer type")],
    )
}

#[test]
fn explicit_type() -> TestResult {
    let (_, tokens) = test_token::join(&[
        test_token::var_name(),
        test_token::semicolon(),
        test_token::var_name(),
    ]);

    assert_failure(
        Stmt::variable_decl(tokens[0].clone(), Some(tokens[2].clone()), None),
        &[Diagnostic::error(&test_span::new(9, 8), "Not a type")],
    )?;

    let (_, tokens) = test_token::join(&[
        test_token::var_name(),
        test_token::semicolon(),
        test_token::type_name(),
    ]);

    assert_failure(
        Stmt::variable_decl(tokens[0].clone(), Some(tokens[2].clone()), None),
        &[Diagnostic::error(&test_span::new(9, 6), "Undefined type")],
    )?;

    assert_success(Stmt::variable_decl(
        test_token::var_name(),
        Some(test_token::int_type()),
        None,
    ))?;

    assert_success_stmts(vec![
        test_ast::window_type(),
        Stmt::variable_decl(
            test_token::var_name(),
            Some(test_token::type_name()),
            None,
        ),
    ])
}

#[test]
fn complex_expression() -> TestResult {
    assert_success(Stmt::expression(Expr::binary(
        Expr::binary(
            Expr::literal(&test_token::four()),
            test_token::greater_than(),
            Expr::literal(&test_token::five()),
        ),
        test_token::equal_equal(),
        Expr::unary(
            test_token::bang(),
            Expr::literal(&test_token::true_keyword()),
        ),
    )))
}

#[test]
fn field_lookup() -> TestResult {
    assert_success_stmts(vec![
        test_ast::window_type(),
        test_ast::window_instance(),
        Stmt::variable_decl(
            test_token::var_name(),
            Some(test_token::int_type()),
            Some(
                Expr::field(
                    Expr::variable(test_token::window_instance(), None),
                    &test_token::property_name(),
                )
            )
        ),
    ])?;

    let (_, prop_access) = test_token::join(&[
        test_token::window_instance(),
        test_token::period(),
        test_token::property_name(),
    ]);

    assert_failure_stmts(vec![
        test_ast::window_type(),
        test_ast::window_instance(),
        Stmt::variable_decl(
            test_token::var_name(),
            Some(test_token::bool_type()),
            Some(
                Expr::field(
                    Expr::variable(prop_access[0].clone(), None),
                    &prop_access[2],
                )
            )
        ),
    ], &[Diagnostic::error(&test_span::new(0, 8 + 1 + 8), "Expected Bool, got Int")],)
}

//
// Helpers
//

fn assert_success(stmt: Stmt) -> TestResult {
    assert_success_stmts(vec![stmt])
}

fn assert_success_stmts(stmts: Vec<Stmt>) -> TestResult {
    let diagnostics = test_typecheck(stmts);
    let diagnostics: &[Diagnostic] = &diagnostics;
    assert_slices_equal(
        "diagnostics",
        diagnostics,
        &[],
        |lhs, rhs| lhs == rhs,
        &diagnostics.diagnostic_string(),
    )
}

fn assert_failure(stmt: Stmt, expected: &[Diagnostic]) -> TestResult {
    assert_failure_stmts(vec![stmt], expected)
}

fn assert_failure_stmts(stmts: Vec<Stmt>, expected: &[Diagnostic]) -> TestResult {
    let diagnostics = test_typecheck(stmts);
    let diagnostics: &[Diagnostic] = &diagnostics;
    assert_slices_equal(
        "diagnostics",
        diagnostics,
        expected,
        |lhs, rhs| lhs == rhs,
        &diagnostics.diagnostic_string(),
    )
}

fn test_typecheck(stmts: Vec<Stmt>) -> Vec<Diagnostic> {
    let program = ParsedProgram {
        source: test_source::new(),
        statements: stmts,
    };

    let (reporter, mut diagnostics) = TestReporter::new();
    TypeChecker::check(program, reporter);
    diagnostics.unwrap()
}

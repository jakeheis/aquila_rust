use aquila::diagnostic::*;
use aquila::lexing::*;
use aquila::library::Lib;
use aquila::parsing::*;

mod common;
use common::*;

#[test]
fn precedence() -> TestResult {
    let tokens = vec![
        test_token::five(),
        test_token::plus(),
        test_token::four(),
        test_token::star(),
        test_token::six(),
    ];

    assert_expr_success(
        tokens,
        Expr::binary(
            Expr::literal(&test_token::five()),
            test_token::plus(),
            Expr::binary(
                Expr::literal(&test_token::four()),
                test_token::star(),
                Expr::literal(&test_token::six()),
            ),
        ),
    )
}

#[test]
fn invalid_assignment() -> TestResult {
    assert_failure(
        vec![
            test_token::five(),
            test_token::plus(),
            test_token::six(),
            test_token::equals(),
            test_token::four(),
        ],
        &[Diagnostic::error(
            &test_span::new(0, 3),
            "Invalid assignment target",
        )],
    )
}

//
// Helpers
//

fn assert_expr_success(tokens: Vec<Token>, expr: Expr) -> TestResult {
    let (lib, diagnostics) = test_parse(tokens);
    let diagnostics: &[Diagnostic] = &diagnostics;

    if !diagnostics.is_empty() {
        let message = format!(
            "Expected no diagnostics, got: {}",
            diagnostics.diagnostic_string()
        );
        return Err(message);
    }

    let mut got_printer = ASTPrinter::collect();
    got_printer.print_stmts(&lib.main);

    let mut expected_printer = ASTPrinter::collect();
    Stmt::expression(expr).accept(&mut expected_printer);

    if got_printer.collected() == expected_printer.collected() {
        Ok(())
    } else {
        println!();
        println!("Expected:");
        println!("{}", expected_printer.collected().join("\n"));
        println!("Got:");
        println!("{}", got_printer.collected().join("\n"));
        println!();
        Err(String::from("Parse failed"))
    }
}

fn assert_failure(tokens: Vec<Token>, expected: &[Diagnostic]) -> TestResult {
    let (_, got) = test_parse(tokens);
    let got: &[Diagnostic] = &got;

    assert_slices_equal(
        "diagnostics",
        &got,
        expected,
        |lhs, rhs| lhs == rhs,
        &got.diagnostic_string(),
    )
}

fn test_parse(mut tokens: Vec<Token>) -> (Lib, Vec<Diagnostic>) {
    tokens.push(test_token::semicolon());
    let (_, combined) = test_token::join(&tokens);
    let (reporter, mut diagnostics) = TestReporter::new();
    let parser = Parser::new(combined, reporter);
    (parser.parse("test"), diagnostics.unwrap())
}

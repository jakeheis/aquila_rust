use aquila::diagnostic::*;
use aquila::lexing::*;
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

fn assert_expr_success(tokens: Vec<Token>, expr: Expr) -> TestResult {
    let (program, diagnostics) = test_parse(tokens);
    let diagnostics: &[Diagnostic] = &diagnostics;

    if !diagnostics.is_empty() {
        let message = format!(
            "Expected no diagnostics, got: {}",
            diagnostics.diagnostic_string()
        );
        return Err(message);
    }

    let mut got_printer = ASTPrinter::collect();
    got_printer.print(&program);

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

fn test_parse(mut tokens: Vec<Token>) -> (ParsedProgram, Vec<Diagnostic>) {
    tokens.push(test_token::semicolon());
    let (source, combined) = test_token::combine_tokens(&tokens);
    let program = LexedProgram {
        source: source,
        tokens: combined,
    };
    let (reporter, mut diagnostics) = TestReporter::new();
    let parser = Parser::new(program, reporter);
    let parsed = parser.parse();
    (parsed, diagnostics.unwrap())
}

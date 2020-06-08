use aquila::diagnostic::*;
use aquila::lexing::*;

mod common;
use common::*;

#[test]
fn math() -> TestResult {
    assert_success(
        "4 + 5",
        &[test_token::four(), test_token::plus(), test_token::five()],
    )
}

#[test]
fn numbers() -> TestResult {
    assert_success("4", &[test_token::four()])?;
    assert_success("0.01", &[test_token::test(TokenKind::Number, "0.01")])
}

#[test]
fn comment() -> TestResult {
    assert_success("4 // asdfa 43 if", &[test_token::four()])
}

#[test]
fn illegal_char() -> TestResult {
    assert_failure(
        "4$3",
        &[Diagnostic::error(
            &test_span::new(1, 1),
            "unrecognized character '$'",
        )],
    )
}

// Helpers

fn assert_success(text: &str, expected: &[Token]) -> TestResult {
    let (tokens, diagnostics) = test_lex(text);
    let tokens: &[Token] = &tokens;
    let diagnostics: &[Diagnostic] = &diagnostics;

    if diagnostics.is_empty() {
        assert_slices_equal(
            "tokens",
            &tokens,
            &test_token::combine_tokens(&expected).1,
            |lhs, rhs| lhs.lexeme() == rhs.lexeme(),
            &tokens.token_string(),
        )
    } else {
        let message = format!(
            "Expected no diagnostics, got: {}",
            diagnostics.diagnostic_string()
        );
        Err(message)
    }
}

fn assert_failure(text: &str, expected: &[Diagnostic]) -> TestResult {
    let (_, diagnostics) = test_lex(text);
    let got: &[Diagnostic] = &diagnostics;

    assert_slices_equal(
        "diagnostics",
        got,
        expected,
        |lhs, rhs| lhs == rhs,
        &got.diagnostic_string(),
    )
}

fn test_lex(text: &str) -> (Vec<Token>, Vec<Diagnostic>) {
    let test_source = aquila::source::text(text);

    let (reporter, mut diagnostics) = TestReporter::new();

    let lexer = Lexer::new(test_source, reporter);
    let program = lexer.lex();

    return (program.tokens, diagnostics.unwrap());
}

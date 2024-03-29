pub use aquila::diagnostic::*;
use std::cell::RefCell;
use std::rc::Rc;

#[allow(dead_code)]
pub type TestResult = std::result::Result<(), String>;

#[allow(dead_code)]
pub mod test_span {

    use aquila::source::{self, Span};

    pub fn new(index: usize, length: usize) -> Span {
        Span {
            source: source::text(""),
            index,
            length,
            line: 1,
        }
    }
}

#[allow(dead_code)]
pub mod test_token {

    use aquila::lexing::*;
    use aquila::source::{self, Source, Span};

    pub fn test(kind: TokenKind, text: &str) -> Token {
        let span = Span::new(&source::text(text), 0, text.chars().count(), 1);
        Token::new(kind, span)
    }

    pub fn four() -> Token {
        test(TokenKind::Int, "4")
    }

    pub fn five() -> Token {
        test(TokenKind::Int, "5")
    }

    pub fn six() -> Token {
        test(TokenKind::Int, "6")
    }

    pub fn star() -> Token {
        test(TokenKind::Star, "*")
    }

    pub fn plus() -> Token {
        test(TokenKind::Plus, "+")
    }

    pub fn bang() -> Token {
        test(TokenKind::Bang, "!")
    }

    pub fn semicolon() -> Token {
        test(TokenKind::Semicolon, ";")
    }

    pub fn period() -> Token {
        test(TokenKind::Period, ".")
    }

    pub fn left_brace() -> Token {
        test(TokenKind::LeftBrace, "{")
    }

    pub fn right_brace() -> Token {
        test(TokenKind::RightBrace, "}")
    }

    pub fn left_paren() -> Token {
        test(TokenKind::LeftParen, "(")
    }

    pub fn right_paren() -> Token {
        test(TokenKind::RightParen, ")")
    }

    pub fn equals() -> Token {
        test(TokenKind::Equal, "=")
    }

    pub fn equal_equal() -> Token {
        test(TokenKind::EqualEqual, "==")
    }

    pub fn greater_than() -> Token {
        test(TokenKind::Greater, ">")
    }

    pub fn less_than() -> Token {
        test(TokenKind::Less, "<")
    }

    pub fn var_name() -> Token {
        test(TokenKind::Identifier, "var_name")
    }

    pub fn func_name() -> Token {
        test(TokenKind::Identifier, "add")
    }

    pub fn type_name() -> Token {
        test(TokenKind::Identifier, "Window")
    }

    pub fn property_name() -> Token {
        test(TokenKind::Identifier, "property")
    }

    pub fn window_instance() -> Token {
        test(TokenKind::Identifier, "instance")
    }

    pub fn int_type() -> Token {
        test(TokenKind::Identifier, "int")
    }

    pub fn bool_type() -> Token {
        test(TokenKind::Identifier, "bool")
    }

    pub fn type_keyword() -> Token {
        test(TokenKind::Type, "type")
    }

    pub fn def_keyword() -> Token {
        test(TokenKind::Def, "def")
    }

    pub fn true_keyword() -> Token {
        test(TokenKind::True, "true")
    }

    pub fn return_keyword() -> Token {
        test(TokenKind::Return, "return")
    }

    pub fn join(tokens: &[Token]) -> (Source, Vec<Token>) {
        let combined = tokens
            .iter()
            .map(|t| t.span.source.all_content())
            .fold(String::new(), |acc, c| acc + c);
        let new_source = source::text(&combined);
        let mut index = 0;
        let mut tokens: Vec<Token> = tokens
            .iter()
            .map(|t| {
                let new_t = Token::new(t.kind, Span::new(&new_source, index, t.span.length, 1));
                index += t.span.length;
                new_t
            })
            .collect();
        tokens.push(Token::new(
            TokenKind::EOF,
            Span::new(&new_source, index, 0, 1),
        ));
        (new_source, tokens)
    }
}

pub struct DiagnosticCapture {
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>,
}

impl DiagnosticCapture {
    pub fn unwrap(&mut self) -> Vec<Diagnostic> {
        let captured = RefCell::new(Vec::new());
        self.diagnostics.swap(&captured);
        captured.into_inner()
    }
}

pub struct TestReporter {
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>,
}

impl TestReporter {
    pub fn new() -> (Rc<dyn Reporter>, DiagnosticCapture) {
        let diagnostics = Rc::new(RefCell::new(Vec::new()));
        (
            Rc::new(TestReporter {
                diagnostics: Rc::clone(&diagnostics),
            }),
            DiagnosticCapture { diagnostics },
        )
    }
}

impl Reporter for TestReporter {
    fn report(&self, diagnostic: Diagnostic) {
        self.diagnostics.borrow_mut().push(diagnostic);
    }
    fn has_errored(&self) -> bool {
        !self.diagnostics.borrow().is_empty()
    }
}

#[allow(dead_code)]
pub fn assert_slices_equal<T, U>(
    kind: &str,
    got: &[T],
    expected: &[T],
    test: U,
    list: &str,
) -> TestResult
where
    T: std::fmt::Display,
    U: Fn(&T, &T) -> bool,
{
    if got.iter().count() != expected.iter().count() {
        let one_line = format!(
            "Expected {} {}, got {}",
            expected.iter().count(),
            kind,
            got.iter().count(),
        );
        println!("{}\n{}", &one_line, list);
        return Err(one_line);
    }

    for (lhs, rhs) in got.iter().zip(expected) {
        if !test(lhs, rhs) {
            println!("Expected:\n  {}\nGot:\n  {}", rhs, lhs);
            return Err(String::from("Unexpected item"));
        }
    }

    Ok(())
}

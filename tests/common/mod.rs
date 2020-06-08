pub use aquila::diagnostic::*;
use std::rc::Rc;
use std::cell::RefCell;

pub type TestResult = std::result::Result<(), String>;

pub mod test_source {

    use aquila::source::SourceImpl;

    pub fn new() -> SourceImpl {
        SourceImpl {
            name: String::from("<stdin>"),
            content: String::new(),
        }
    }
}

pub mod test_span {

    use aquila::source::*;
    use super::test_source;

    pub fn new(index: usize, length: usize) -> Span {
        Span {
            source: std::rc::Rc::new(test_source::new()),
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
        let source = source::text(text);
        let span = Span::new(&source, 0, text.chars().count(), 0);
        Token::new(kind, span)
    }

    pub fn four() -> Token {
        test(TokenKind::Number, "4")
    }

    pub fn five() -> Token {
        test(TokenKind::Number, "5")
    }

    pub fn six() -> Token {
        test(TokenKind::Number, "6")
    }

    pub fn star() -> Token {
        test(TokenKind::Star, "*")
    }

    pub fn plus() -> Token {
        test(TokenKind::Plus, "+")
    }

    pub fn semicolon() -> Token {
        test(TokenKind::Semicolon, ";")
    }

    pub fn equals() -> Token {
        test(TokenKind::Equal, "=")
    }

    pub fn combine_tokens(tokens: &[Token]) -> (Source, Vec<Token>) {
        let combined = tokens
            .iter()
            .map(|t| &t.span.source.content)
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
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>
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
        (Rc::new(TestReporter {
            diagnostics: Rc::clone(&diagnostics),
        }), DiagnosticCapture { diagnostics  })
    }
}

impl Reporter for TestReporter {
    fn report(&self, diagnostic: Diagnostic) {
        self.diagnostics.borrow_mut().push(diagnostic);
    }
}

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
        if test(lhs, rhs) {
            println!("Expected:\n  {}\nGot:\n  {}", rhs, lhs);
            return Err(String::from("Unexpected item"))
        }
    }

    Ok(())
}

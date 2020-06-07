use crate::source::*;
use colored::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(PartialEq)]
enum Severity {
    Error,
    // Warning,
}

pub type DiagnosticResult<T> = Result<T, Diagnostic>;

pub struct Diagnostic {
    severity: Severity,
    span: Span,
    message: String,
}

impl Diagnostic {
    pub fn error<T: ContainsSpan>(span: &T, message: &str) -> Self {
        Diagnostic {
            severity: Severity::Error,
            span: span.span().clone(),
            message: String::from(message),
        }
    }
}

impl ReplaceableSpan for Diagnostic {
    fn replace_span<U: ContainsSpan>(self, new_span: &U) -> Diagnostic {
        Diagnostic {
            severity: self.severity,
            span: new_span.span().clone(),
            message: self.message,
        }
    }
}

impl<T> ReplaceableSpan for DiagnosticResult<T> {
    fn replace_span<U: ContainsSpan>(self, new_span: &U) -> DiagnosticResult<T> {
        self.map_err(|e| e.replace_span(new_span))
    }
}

pub trait Reporter {
    fn report(&self, diagnostic: Diagnostic);
}

pub struct DefaultReporter {}

impl DefaultReporter {
    pub fn new() -> Rc<Self> {
        Rc::new(DefaultReporter {})
    }
}

impl Reporter for DefaultReporter {
    fn report(&self, diagnostic: Diagnostic) {
        let header = if diagnostic.severity == Severity::Error {
            "• Error:".red().bold()
        } else {
            "• Warning:".yellow().bold()
        };
        println!("\n{} {}\n", header, diagnostic.message);
        let (line, offset) = diagnostic.span.entire_line();
        println!("  {}", line);

        let offset = (0..offset).map(|_| " ").collect::<String>();
        let underline = (0..diagnostic.span.length).map(|_| "^").collect::<String>();
        let colored_outline = if diagnostic.severity == Severity::Error {
            underline.red()
        } else {
            underline.yellow()
        };
        println!("  {}{}", offset, colored_outline);
        println!("  {}\n", diagnostic.span.location());
    }
}

pub struct TestReporter {
    diagnostics: RefCell<Vec<Diagnostic>>,
}

impl TestReporter {
    pub fn new() -> Self {
        TestReporter {
            diagnostics: RefCell::new(Vec::new()),
        }
    }
}

impl Reporter for TestReporter {
    fn report(&self, diagnostic: Diagnostic) {
        self.diagnostics.borrow_mut().push(diagnostic);
    }
}

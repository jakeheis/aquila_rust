pub use crate::source::*;
use colored::*;
use std::cell::Cell;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Severity {
    Error,
    Warning,
}

pub type DiagnosticResult<T> = Result<T, Diagnostic>;

#[derive(Clone, PartialEq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub span: Span,
    pub message: String,
}

impl Diagnostic {
    pub fn error<T: ContainsSpan>(span: &T, message: &str) -> Self {
        Diagnostic {
            severity: Severity::Error,
            span: span.span().clone(),
            message: String::from(message),
        }
    }

    pub fn warning<T: ContainsSpan>(span: &T, message: &str) -> Self {
        Diagnostic {
            severity: Severity::Warning,
            span: span.span().clone(),
            message: String::from(message),
        }
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Diagnostic({:#?}, '{}' ({}))",
            self.severity, self.message, self.span
        )
    }
}

pub trait DiagnosticString {
    fn diagnostic_string(&self) -> String;
}

impl DiagnosticString for &[Diagnostic] {
    fn diagnostic_string(&self) -> String {
        self.iter()
            .fold(String::from("["), |acc, diag| acc + &diag.to_string())
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

    fn has_errored(&self) -> bool;
}

pub struct DefaultReporter {
    errored: Cell<bool>,
}

impl DefaultReporter {
    pub fn new() -> Rc<Self> {
        Rc::new(DefaultReporter {
            errored: Cell::new(false),
        })
    }
}

impl Reporter for DefaultReporter {
    fn report(&self, diagnostic: Diagnostic) {
        let header = if diagnostic.severity == Severity::Error {
            self.errored.replace(true);
            "• Error:".red().bold()
        } else {
            "• Warning:".yellow().bold()
        };
        eprintln!("\n{} {}\n", header, diagnostic.message);
        let (line, offset) = diagnostic.span.entire_line();
        eprintln!("  {}", line);

        let offset = (0..offset).map(|_| " ").collect::<String>();
        let underline = (0..diagnostic.span.length).map(|_| "^").collect::<String>();
        let colored_outline = if diagnostic.severity == Severity::Error {
            underline.red()
        } else {
            underline.yellow()
        };
        eprintln!("  {}{}", offset, colored_outline);
        eprintln!("  {}\n", diagnostic.span.location());
    }

    fn has_errored(&self) -> bool {
        self.errored.get()
    }
}

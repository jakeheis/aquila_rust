use crate::lexing::token::*;
use crate::parsing::*;
use crate::source::*;
use colored::*;
use std::rc::Rc;

#[derive(PartialEq)]
enum Severity {
    Error,
    // Warning,
}

type DiagnosticResult<T> = Result<T, Diagnostic>;

pub struct Diagnostic {
    severity: Severity,
    span: Span,
    message: String,
}

impl Diagnostic {
    pub fn error_span(span: Span, message: &str) -> Self {
        Diagnostic {
            severity: Severity::Error,
            span: span,
            message: String::from(message),
        }
    }

    pub fn error_token(token: &Token, message: &str) -> Self {
        Diagnostic::error_span(token.span.clone(), message)
    }

    pub fn error_expr(expr: &Expr, message: &str) -> Self {
        Diagnostic::error_span(expr.span.clone(), message)
    }
}

impl ReplaceableSpan for Diagnostic {
    fn replace_span(self, new_span: &Span) -> Diagnostic {
        Diagnostic {
            severity: self.severity,
            span: new_span.clone(),
            message: self.message,
        }
    }
}

impl<T> ReplaceableSpan for DiagnosticResult<T> {
    fn replace_span(self, new_span: &Span) -> DiagnosticResult<T> {
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

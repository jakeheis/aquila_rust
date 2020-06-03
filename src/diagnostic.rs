use crate::lexing::token::*;
use crate::parsing::*;
use crate::source::Span;
use colored::*;
use std::rc::Rc;

#[derive(PartialEq)]
enum Severity {
    Error,
    // Warning,
}

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
        println!("  {}{}", offset, underline);
        println!("  {}\n", diagnostic.span.location());
    }
}

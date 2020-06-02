use std::{fmt};
use crate::source::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    Number,
    Identifier,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Ampersand,
    AmpersandAmpersand,
    Bar,
    BarBar,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Keywords
    True,
    False,
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn lexeme(&self) -> &str {
        self.span.lexeme()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token(kind: {:#?}, lexeme: {})",
            self.kind,
            self.lexeme()
        )
    }
}

pub trait TokenString {
    fn token_string(&self) -> String;
}

impl TokenString for Vec<Token> {
    fn token_string(&self) -> String {
        let start = self[0].to_string();
        let toks = self
            .iter()
            .skip(1)
            .fold(start, |c, t| c + ", " + &t.to_string());
        String::from("Vec(") + &toks + ")"
    }
}

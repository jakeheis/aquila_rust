use crate::source::*;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    Int,
    Double,
    StringLiteral,
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
    Bang,
    BangEqual,

    // Groupings
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    PeriodLeftBracket,
    RightBracket,
    GenericOpen,
    GenericClose,

    // Keywords
    True,
    False,
    Type,
    Trait,
    Def,
    Let,
    If,
    Else,
    Return,
    Print,
    Ptr,
    Builtin,
    Meta,
    While,
    For,
    In,
    Cast,
    SelfKeyword,

    // Other
    Comma,
    Period,
    Semicolon,
    Colon,
    EOF,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }

    pub fn lexeme(&self) -> &str {
        self.span.lexeme()
    }
}

impl ContainsSpan for Token {
    fn span(&self) -> &Span {
        &self.span
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

impl TokenString for &[Token] {
    fn token_string(&self) -> String {
        let start = self[0].to_string();
        let toks = self
            .iter()
            .skip(1)
            .fold(start, |c, t| c + ", " + &t.to_string());
        String::from("Vec(") + &toks + ")"
    }
}

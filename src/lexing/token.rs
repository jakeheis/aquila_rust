use crate::source::*;
use std::fmt;

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
    Bang,
    BangEqual,

    // Groupings
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,

    // Keywords
    True,
    False,
    Type,
    Def,
    Let,
    If,
    Else,

    // Other
    Comma,
    Period,
    Semicolon,
    Colon,
    EOF,
}

#[derive(Clone)]
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

#[cfg(test)]
impl Token {
    pub fn test(kind: TokenKind, text: &str) -> Self {
        let source = crate::source::text(text);
        let span = Span::new(&source, 0, text.chars().count(), 0);
        Token::new(kind, span)
    }

    pub fn four() -> Self {
        Token::test(TokenKind::Number, "4")
    }

    pub fn five() -> Self {
        Token::test(TokenKind::Number, "5")
    }

    pub fn six() -> Self {
        Token::test(TokenKind::Number, "6")
    }

    pub fn star() -> Self {
        Token::test(TokenKind::Star, "*")
    }

    pub fn plus() -> Self {
        Token::test(TokenKind::Plus, "+")
    }

    pub fn semicolon() -> Self {
        Token::test(TokenKind::Semicolon, ";")
    }

    pub fn combine_tokens(tokens: &[Token]) -> (Source, Vec<Token>) {
        let combined = tokens
            .iter()
            .map(|t| &t.span.source.content)
            .fold(String::new(), |acc, c| acc + c);
        let new_source = crate::source::text(&combined);
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

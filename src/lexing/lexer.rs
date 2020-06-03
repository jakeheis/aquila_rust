use super::token::*;
use crate::diagnostic;
use crate::source::*;
use std::rc::Rc;

pub struct Lexer {
    source: Source,
    start: usize,
    current: usize,
    line: usize,
    reporter: Rc<dyn diagnostic::Reporter>,
}

impl Lexer {
    pub fn new(source: Source, reporter: Rc<dyn diagnostic::Reporter>) -> Self {
        Lexer {
            source,
            start: 0,
            current: 0,
            line: 1,
            reporter,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.start = self.current;
            let new_token = self.token();
            if let Some(new) = new_token {
                tokens.push(new);
            }
        }

        self.start = self.current;
        tokens.push(self.make_token(TokenKind::EOF).unwrap());

        tokens
    }

    fn token(&mut self) -> Option<Token> {
        let character = self.advance();
        match character {
            '+' => self.make_token(TokenKind::Plus),
            '-' => self.make_token(TokenKind::Minus),
            '*' => self.make_token(TokenKind::Star),
            '/' => {
                if self.consume('/') {
                    self.comment()
                } else {
                    self.make_token(TokenKind::Slash)
                }
            }
            '&' => self.conditional_make_token(
                '&',
                TokenKind::AmpersandAmpersand,
                TokenKind::Ampersand,
            ),
            '|' => self.conditional_make_token('|', TokenKind::BarBar, TokenKind::Bar),
            '=' => self.conditional_make_token('=', TokenKind::EqualEqual, TokenKind::Equal),
            '>' => self.conditional_make_token('=', TokenKind::GreaterEqual, TokenKind::Greater),
            '<' => self.conditional_make_token('=', TokenKind::LessEqual, TokenKind::Less),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' => self.identifier(),
            ';' => self.make_token(TokenKind::Semicolon),
            ' ' => None,
            '\n' => {
                self.line += 1;
                None
            }
            _ => {
                self.error(&format!("unrecognized character '{}'", character));
                None
            }
        }
    }

    fn comment(&mut self) -> Option<Token> {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
        None
    }

    fn number(&mut self) -> Option<Token> {
        while !self.is_at_end() {
            if let '0'..='9' = self.peek() {
                self.advance();
            } else {
                break;
            }
        }
        self.make_token(TokenKind::Number)
    }

    fn identifier(&mut self) -> Option<Token> {
        while !self.is_at_end() {
            if let '0'..='9' | 'a'..='z' | 'A'..='Z' = self.peek() {
                self.advance();
            } else {
                break;
            }
        }

        let token = self.make_token(TokenKind::Identifier).unwrap();
        match token.lexeme() {
            "true" => self.make_token(TokenKind::True),
            "false" => self.make_token(TokenKind::False),
            _ => Some(token),
        }
    }

    fn conditional_make_token(
        &mut self,
        character: char,
        kind1: TokenKind,
        kind2: TokenKind,
    ) -> Option<Token> {
        if self.consume(character) {
            self.make_token(kind1)
        } else {
            self.make_token(kind2)
        }
    }

    fn make_token(&self, kind: TokenKind) -> Option<Token> {
        let span = self.current_span();
        Some(Token { kind, span })
    }

    fn error(&self, message: &str) {
        self.reporter.report(diagnostic::Diagnostic::error_span(
            self.current_span(),
            message,
        ));
    }

    fn current_span(&self) -> Span {
        Span::new(
            &self.source,
            self.start,
            self.current - self.start,
            self.line,
        )
    }

    fn consume(&mut self, character: char) -> bool {
        if self.peek() == character {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> char {
        let character = self.peek();
        self.current += 1;
        character
    }

    fn peek(&self) -> char {
        self.source.character(self.current)
    }

    fn is_at_end(&self) -> bool {
        self.current == self.source.length()
    }
}

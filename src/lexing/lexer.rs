use super::token::*;
use crate::diagnostic::*;
use crate::source::*;
use std::rc::Rc;

pub struct Lexer {
    source: Source,
    start: usize,
    current: usize,
    line: usize,
    reporter: Rc<dyn Reporter>,
}

impl Lexer {
    pub fn new(source: Source, reporter: Rc<dyn Reporter>) -> Self {
        Lexer {
            source,
            start: 0,
            current: 0,
            line: 1,
            reporter,
        }
    }

    pub fn lex(mut self) -> Vec<Token> {
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
                if self.matches('/') {
                    self.comment()
                } else if self.matches('*') {
                    self.block_comment()
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
            '!' => self.conditional_make_token('=', TokenKind::BangEqual, TokenKind::Bang),
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' => self.identifier(),
            '"' => self.string(),
            ',' => self.make_token(TokenKind::Comma),
            '.' => self.make_token(TokenKind::Period),
            ';' => self.make_token(TokenKind::Semicolon),
            ':' => self.make_token(TokenKind::Colon),
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

    fn block_comment(&mut self) -> Option<Token> {
        while !self.is_at_end() {
            if self.matches('\n') {
                self.line += 1;
            } else if self.matches('*') {
                if self.matches('/') {
                    break;
                }
            } else {
                self.advance();
            }
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
        if self.matches('.') {
            while !self.is_at_end() {
                if let '0'..='9' = self.peek() {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.make_token(TokenKind::Number)
    }

    fn string(&mut self) -> Option<Token> {
        while !self.is_at_end() && self.peek() != '"' {
            self.advance();
        }
        if self.peek() == '"' {
            self.advance();
        }
        self.make_token(TokenKind::StringLiteral)
    }

    fn identifier(&mut self) -> Option<Token> {
        while !self.is_at_end() {
            if let '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' = self.peek() {
                self.advance();
            } else {
                break;
            }
        }

        let token = self.make_token(TokenKind::Identifier).unwrap();
        match token.lexeme() {
            "true" => self.make_token(TokenKind::True),
            "false" => self.make_token(TokenKind::False),
            "type" => self.make_token(TokenKind::Type),
            "let" => self.make_token(TokenKind::Let),
            "if" => self.make_token(TokenKind::If),
            "else" => self.make_token(TokenKind::Else),
            "def" => self.make_token(TokenKind::Def),
            "return" => self.make_token(TokenKind::Return),
            "print" => self.make_token(TokenKind::Print),
            "ptr" => self.make_token(TokenKind::Ptr),
            "builtin" => self.make_token(TokenKind::Builtin),
            "meta" => self.make_token(TokenKind::Meta),
            _ => Some(token),
        }
    }

    fn conditional_make_token(
        &mut self,
        character: char,
        kind1: TokenKind,
        kind2: TokenKind,
    ) -> Option<Token> {
        if self.matches(character) {
            self.make_token(kind1)
        } else {
            self.make_token(kind2)
        }
    }

    fn make_token(&self, kind: TokenKind) -> Option<Token> {
        let span = self.current_span();
        Some(Token::new(kind, span))
    }

    fn error(&self, message: &str) {
        self.reporter
            .report(Diagnostic::error(&self.current_span(), message));
    }

    fn current_span(&self) -> Span {
        Span::new(
            &self.source,
            self.start,
            self.current - self.start,
            self.line,
        )
    }

    fn matches(&mut self, character: char) -> bool {
        if self.is_at_end() {
            return false;
        }
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

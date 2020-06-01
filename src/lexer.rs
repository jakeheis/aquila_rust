use super::Source;
use std::fmt;
use std::rc::Rc;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind {
    NUMBER,
    PLUS,
    MINUS,
    STAR,
    SLASH,
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub index: usize,
    pub length: usize,
    pub source: Rc<Source>,
}

impl Token {
    pub fn lexeme(&self) -> &str {
        self.source.lexeme(self.index, self.length)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token(kind: {}, lexeme: {})",
            self.kind as i32,
            self.lexeme()
        )
    }
}

pub struct Lexer {
    source: Rc<Source>,
    start: usize,
    current: usize,
}

impl Lexer {
    pub fn new(source: Source) -> Self {
        Lexer {
            source: Rc::new(source),
            start: 0,
            current: 0,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            while ' ' == self.peek() {
                self.advance();
            }

            self.start = self.current;
            let new_token = self.token();
            if let Some(new) = new_token {
                tokens.push(new);
            } else {
                break;
            }
        }

        tokens
    }

    fn token(&mut self) -> Option<Token> {
        let character = self.advance();
        match character {
            '+' => Some(self.make_token(TokenKind::PLUS)),
            '-' => Some(self.make_token(TokenKind::MINUS)),
            '*' => Some(self.make_token(TokenKind::STAR)),
            '/' => Some(self.make_token(TokenKind::SLASH)),
            '0'..='9' => Some(self.number()),
            _ => None,
        }
    }

    fn number(&mut self) -> Token {
        while !self.is_at_end() {
            if let '0'..='9' = self.peek() {
                self.advance();
            } else {
                break;
            }
        }
        self.make_token(TokenKind::NUMBER)
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token {
            source: Rc::clone(&self.source),
            kind,
            index: self.start,
            length: self.current - self.start,
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

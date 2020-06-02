use std::fmt;
use std::rc::Rc;
use crate::source::Source;

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

    // Keywords

    True,
    False,
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
        let toks = self.iter().skip(1).fold(start, |c, t| c + ", " + &t.to_string());
        String::from("Vec(") + &toks + ")"
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
            '+' => self.make_token(TokenKind::Plus),
            '-' => self.make_token(TokenKind::Minus),
            '*' => self.make_token(TokenKind::Star),
            '/' => self.make_token(TokenKind::Slash),
            '&' => if self.consume('&') {
                self.make_token(TokenKind::AmpersandAmpersand) 
            } else { 
                self.make_token(TokenKind::Ampersand) 
            },
            '|' => if self.consume('|') {
                self.make_token(TokenKind::BarBar) 
            } else { 
                self.make_token(TokenKind::Bar)
            },
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' => self.identifier(),
            _ => None,
        }
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
            _ => Some(token)
        }
    }

    fn make_token(&self, kind: TokenKind) -> Option<Token> {
        Some(Token {
            source: Rc::clone(&self.source),
            kind,
            index: self.start,
            length: self.current - self.start,
        })
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

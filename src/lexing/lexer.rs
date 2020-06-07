use super::token::*;
use crate::diagnostic::*;
use crate::program::*;
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

    pub fn lex(mut self) -> LexedProgram {
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

        LexedProgram {
            source: self.source,
            tokens,
        }
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
            "type" => self.make_token(TokenKind::Type),
            "let" => self.make_token(TokenKind::Let),
            "if" => self.make_token(TokenKind::If),
            "else" => self.make_token(TokenKind::Else),
            "def" => self.make_token(TokenKind::Def),
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

#[cfg(test)]
mod tests {

    type Result = std::result::Result<(), String>;

    use super::*;
    use crate::source;

    #[test]
    fn math() -> Result {
        assert_success("4 + 5", &[Token::four(), Token::plus(), Token::five()])
    }

    #[test]
    fn numbers() -> Result {
        assert_success("4", &[Token::four()])?;
        assert_success("0.01", &[Token::test(TokenKind::Number, "0.01")])
    }

    #[test]
    fn comment() -> Result {
        assert_success("4 // asdfa 43 if", &[Token::four()])
    }

    fn assert_success(text: &str, tokens: &[Token]) -> Result {
        let test_source = source::text(text);
        let reporter: Rc<dyn Reporter> = Rc::new(TestReporter::new());

        let lexer = Lexer::new(test_source, Rc::clone(&reporter));
        let program = lexer.lex();

        reporter.assert_no_diagnostics()?;

        assert_tokens(&program.tokens, Token::combine_tokens(tokens).1)
    }

    fn assert_tokens(got: &[Token], expected: Vec<Token>) -> Result {
        if got.iter().count() != expected.iter().count() {
            return Err(format!(
                "Expected {} tokens, got {} -- {}",
                expected.iter().count(),
                got.iter().count(),
                got.token_string()
            ));
        }

        for (lhs, rhs) in got.iter().zip(expected) {
            if lhs.lexeme() != rhs.lexeme() {
                return Err(format!(
                    "Expected {}, got {} -- {}",
                    rhs,
                    lhs,
                    got.token_string()
                ));
            }
        }

        Ok(())
    }
}

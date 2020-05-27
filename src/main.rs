
#[derive(Debug)]
enum TokenKind {
    NUMBER,
    PLUS, MINUS, STAR, SLASH
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    index: usize,
    length: usize
}

impl Token {
    fn lexeme<'a>(&self, lexer: &'a Lexer) -> &'a str {
        let end = self.index + self.length;
        &lexer.contents[self.index..(end)]
    }
}

struct Lexer {
    contents: String,
    start: usize,
    current: usize
}

impl Lexer {
    fn new(contents: &str) -> Self {
        Lexer { contents: String::from(contents), start: 0, current: 0 }
    }

    fn lex(&mut self) -> Vec<Token> {
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
                break
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
                break
            }
        }
        self.make_token(TokenKind::NUMBER)
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token { kind, index: self.start, length: self.current - self.start }
    }

    fn advance(&mut self) -> char {
        let character = self.peek();
        self.current += 1;
        character
    }

    fn peek(&self) -> char {
        self.contents.chars().nth(self.current).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.current == self.contents.chars().count()
    }

}

fn main() {
    let text = "4 + 5 * 7";

    let mut lexer = Lexer::new(text);
    let tokens = lexer.lex();
    for token in tokens {
        println!("Token: {}", token.lexeme(&lexer))
    }
}

pub mod lexer;

pub fn run(text: &str) {
    let mut lexer = lexer::Lexer::new(text);
    let tokens = lexer.lex();
    for token in tokens {
        println!("Token: {}", token.lexeme(&lexer))
    }
}

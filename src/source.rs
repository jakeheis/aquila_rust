use std::fs;

pub struct Source {
    content: String,
}

impl Source {
    pub fn load(file: &str) -> Self {
        let content = fs::read_to_string(file).unwrap();
        Source { content }
    }

    pub fn text(text: &str) -> Self {
        Source {
            content: String::from(text),
        }
    }

    pub fn character(&self, number: usize) -> char {
        self.content.chars().nth(number).unwrap()
    }

    pub fn length(&self) -> usize {
        self.content.chars().count()
    }

    pub fn lexeme(&self, index: usize, length: usize) -> &str {
        let end = index + length;
        &self.content[index..end]
    }
}
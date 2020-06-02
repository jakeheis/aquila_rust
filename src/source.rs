use std::fs;
use std::rc::Rc;

pub struct Source {
    content: String,
}

impl Source {
    pub fn load(file: &str) -> Rc<Self> {
        let content = fs::read_to_string(file).unwrap();
        Rc::new(Source { content })
    }

    pub fn text(text: &str) -> Rc<Self> {
        Rc::new(Source {
            content: String::from(text),
        })
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

#[derive(Clone)]
pub struct Span {
    source: Rc<Source>,
    index: usize,
    length: usize,
}

impl Span {
    pub fn new(source: &Rc<Source>, index: usize, length: usize) -> Span {
        Span {
            source: Rc::clone(source),
            index,
            length
        }
    }

    pub fn span(lhs: &Span, rhs: &Span) -> Span {
        Span {
            source: Rc::clone(&lhs.source),
            index: lhs.index,
            length: rhs.length + rhs.index - lhs.index
        }
    }

    pub fn lexeme(&self) -> &str {
        let end = self.index + self.length;
        &self.source.content[self.index..end]
    }
}

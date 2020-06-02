use std::fs;
use std::rc::Rc;

pub type Source = Rc<SourceImpl>;

pub fn file(file: &str) -> Source {
    let content = fs::read_to_string(file).unwrap();
    Rc::new(SourceImpl { content })
}

pub fn text(text: &str) -> Source {
    Rc::new(SourceImpl {
        content: String::from(text),
    })
}

pub struct SourceImpl {
    content: String,
}

impl SourceImpl {
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
    source: Source,
    index: usize,
    length: usize,
}

impl Span {
    pub fn new(source: &Source, index: usize, length: usize) -> Span {
        Span {
            source: Rc::clone(source),
            index,
            length,
        }
    }

    pub fn span(lhs: &Span, rhs: &Span) -> Span {
        Span {
            source: Rc::clone(&lhs.source),
            index: lhs.index,
            length: rhs.length + rhs.index - lhs.index,
        }
    }

    pub fn lexeme(&self) -> &str {
        let end = self.index + self.length;
        &self.source.content[self.index..end]
    }
}

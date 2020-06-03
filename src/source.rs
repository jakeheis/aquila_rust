use std::fs;
use std::rc::Rc;

pub type Source = Rc<SourceImpl>;

pub fn file(file: &str) -> Source {
    let content = fs::read_to_string(file).unwrap();
    Rc::new(SourceImpl {
        name: String::from(fs::canonicalize(file).unwrap().to_str().unwrap()),
        content,
    })
}

pub fn text(text: &str) -> Source {
    Rc::new(SourceImpl {
        name: String::from("<stdin>"),
        content: String::from(text),
    })
}

pub struct SourceImpl {
    name: String,
    content: String,
}

impl SourceImpl {
    pub fn name(&self) -> &str {
        &self.name
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
    source: Source,
    index: usize,
    pub length: usize,
    line: usize,
}

impl Span {
    pub fn new(source: &Source, index: usize, length: usize, line: usize) -> Span {
        Span {
            source: Rc::clone(source),
            index,
            length,
            line,
        }
    }

    pub fn span(lhs: &Span, rhs: &Span) -> Span {
        Span {
            source: Rc::clone(&lhs.source),
            index: lhs.index,
            length: rhs.length + rhs.index - lhs.index,
            line: lhs.line,
        }
    }

    pub fn location(&self) -> String {
        format!("{}:{}", self.source.name(), self.line)
    }

    pub fn lexeme(&self) -> &str {
        let end = self.index + self.length;
        &self.source.content[self.index..end]
    }

    pub fn entire_line(&self) -> (&str, usize) {
        let mut start = self.index;
        while start > 0 && self.source.character(start - 1) != '\n' {
            start -= 1;
        }
        let mut end = self.index + self.length;
        while end + 1 < self.source.length() && self.source.character(end + 1) != '\n' {
            end += 1;
        }
        (&self.source.content[start..end], self.index - start)
    }
}

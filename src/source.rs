use std::fs;
use std::rc::Rc;

pub type Source = Rc<SourceImpl>;

pub fn file(file: &str) -> Source {
    let content = fs::read_to_string(file).unwrap();
    Rc::new(SourceImpl {
        name: String::from(fs::canonicalize(file).unwrap().file_stem().unwrap().to_str().unwrap()),
        content,
    })
}

pub fn text(text: &str) -> Source {
    Rc::new(SourceImpl {
        name: String::from("<stdin>"),
        content: String::from(text),
    })
}

#[derive(Debug)]
pub struct SourceImpl {
    pub name: String,
    pub content: String,
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

#[derive(Clone, Debug)]
pub struct Span {
    pub source: Source,
    pub index: usize,
    pub length: usize,
    pub line: usize,
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

    pub fn empty() -> Self {
        let source = SourceImpl {
            name: String::from("<none>"),
            content: String::from(""),
        };
        Span {
            source: Rc::new(source),
            index: 0,
            length: 0,
            line: 1,
        }
    }

    pub fn join<T, U>(lhs: &T, rhs: &U) -> Span
    where
        T: ContainsSpan,
        U: ContainsSpan,
    {
        let lhs = lhs.span();
        let rhs = rhs.span();
        Span {
            source: Rc::clone(&lhs.source),
            index: lhs.index,
            length: rhs.length + rhs.index - lhs.index,
            line: lhs.line,
        }
    }

    pub fn join_opt<T, U>(lhs: &T, rhs: &Option<U>) -> Span
    where
        T: ContainsSpan,
        U: ContainsSpan,
    {
        if let Some(rhs) = rhs {
            Span::join(lhs, rhs)
        } else {
            lhs.span().clone()
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
        while end + 1 < self.source.length() && self.source.character(end) != '\n' {
            end += 1;
        }
        (&self.source.content[start..end], self.index - start)
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Span(in: {}, index: {}, length: {}, line: {})",
            self.source.name(),
            self.index,
            self.length,
            self.line
        )
    }
}

impl PartialEq for Span {
    fn eq(&self, other: &Span) -> bool {
        self.source.name() == other.source.name()
            && self.index == other.index
            && self.length == other.length
            && self.line == other.line
    }
}

// Span traits

pub trait ContainsSpan {
    fn span(&self) -> &Span;
}

impl ContainsSpan for Span {
    fn span(&self) -> &Span {
        self
    }
}

pub trait ReplaceableSpan {
    fn replace_span<T: ContainsSpan>(self, new_span: &T) -> Self;
}

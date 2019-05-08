use crate::expression::Ref;

pub struct SourceSpan {
    span: Span,
    source: Ref<SourceCode>,
}

#[derive(Copy, Clone)]
struct Pos {
    idx: usize,
}

#[derive(Copy, Clone)]
struct Span {
    start: Pos,
    end: Pos,
}

struct SourceCode {
    buffer: String,
    lines: Vec<usize>,
}

impl SourceCode {
    pub fn new() -> Self {
        SourceCode {
            buffer: String::new(),
            lines: vec![],
        }
    }

    pub fn from_string(s: String) -> Self {
        SourceCode {
            lines: std::iter::once(0)
                .chain(
                    s.chars()
                        .enumerate()
                        .filter(|(i, ch)| *ch == '\n')
                        .map(|(i, _)| i + 1)
                        .filter(|&i| i < s.len()),
                )
                .collect(),
            buffer: s,
        }
    }

    pub fn append(&mut self, other: SourceCode) -> Span {
        let start = Pos {
            idx: self.buffer.len(),
        };
        self.buffer.push_str(&other.buffer);
        self.lines
            .extend(other.lines.into_iter().map(|i| i + start.idx));
        let end = Pos {
            idx: self.buffer.len(),
        };
        Span { start, end }
    }

    pub fn line_start(&self, pos: Pos) -> Pos {
        let mut idx = 0;
        for &l in &self.lines {
            if l > pos.idx {
                break;
            }
            idx = l;
        }
        Pos { idx }
    }

    pub fn line_end(&self, pos: Pos) -> Pos {
        let mut idx = 0;
        for &l in &self.lines {
            if l > pos.idx {
                return Pos { idx: l };
            }
            idx = l;
        }
        Pos { idx }
    }

    pub fn chars<'a>(&'a self, span: Span) -> impl Iterator<Item = char> + 'a {
        self.buffer[span.start.idx..span.end.idx].chars()
    }

    pub fn char_indices<'a>(&'a self, span: Span) -> impl Iterator<Item = (usize, char)> + 'a {
        self.buffer[span.start.idx..span.end.idx]
            .char_indices()
            .map(move |(i, ch)| (i + span.start.idx, ch))
    }
}

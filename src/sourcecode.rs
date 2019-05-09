pub use source_impl::*;

#[cfg(not(feature = "source-tracking"))]
mod source_impl {
    pub struct SourceView;
    pub struct Source;

    impl From<String> for Source {
        fn from(_: String) -> Self {
            Source
        }
    }

    impl Source {
        pub fn view(&self, _: usize, _: usize) -> SourceView {
            SourceView
        }
    }
}

#[cfg(feature = "source-tracking")]
mod source_impl {
    use crate::expression::Ref;

    #[derive(Debug, Copy, Clone)]
    struct Pos {
        pub idx: usize,
    }

    #[derive(Debug, Copy, Clone)]
    struct Span {
        pub start: Pos,
        pub end: Pos,
    }

    #[derive(Debug, Clone)]
    pub struct SourceView {
        span: Span,
        source: Ref<SourceCode>,
    }

    impl SourceView {
        pub fn pre_span(&self) -> &str {
            &self.source.buffer[..self.span.start.idx]
        }

        pub fn in_span(&self) -> &str {
            &self.source.buffer[self.span.start.idx..self.span.end.idx]
        }

        pub fn post_span(&self) -> &str {
            &self.source.buffer[self.span.end.idx..]
        }
    }

    impl std::fmt::Display for SourceView {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "{}", &self.in_span())
        }
    }

    #[derive(Debug, Clone)]
    pub struct Source {
        source: Ref<SourceCode>,
    }

    impl Source {
        pub fn view(&self, start: usize, end: usize) -> SourceView {
            SourceView {
                span: Span {
                    start: Pos { idx: start },
                    end: Pos { idx: end },
                },
                source: self.source.clone(),
            }
        }
    }

    #[derive(Debug)]
    struct SourceCode {
        buffer: String,
        lines: Vec<usize>,
    }

    impl SourceCode {
        pub fn len(&self) -> usize {
            self.buffer.len()
        }

        pub fn from_string(s: String) -> Self {
            SourceCode {
                lines: SourceCode::lines(&s),
                buffer: s,
            }
        }

        fn lines(s: &str) -> Vec<usize> {
            std::iter::once(0)
                .chain(
                    s.chars()
                        .enumerate()
                        .filter(|(_, ch)| *ch == '\n')
                        .map(|(i, _)| i + 1)
                        .filter(|&i| i < s.len()),
                )
                .collect()
        }
    }

    impl From<String> for Source {
        fn from(s: String) -> Self {
            SourceCode::from_string(s).into()
        }
    }

    impl From<SourceCode> for Source {
        fn from(s: SourceCode) -> Self {
            Source {
                source: Ref::new(s),
            }
        }
    }

    impl From<Source> for SourceView {
        fn from(s: Source) -> Self {
            let span = Span {
                start: Pos { idx: 0 },
                end: Pos {
                    idx: s.source.len(),
                },
            };
            SourceView {
                span,
                source: s.source,
            }
        }
    }
}

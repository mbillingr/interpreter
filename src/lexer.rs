use crate::errors::*;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Token {
    ListOpen,
    ListClose,
    String(String),
    Symbol(String),
    Quote,
    BackQuote,
    Dot,
    Comma,
    Char(char),
    EOF,
}

impl Token {
    pub fn is_symbol(&self) -> bool {
        if let Token::Symbol(_) = self {
            true
        } else {
            false
        }
    }
}

impl From<char> for Token {
    fn from(ch: char) -> Self {
        match ch {
            '(' => Token::ListOpen,
            ')' => Token::ListClose,
            '\'' => Token::Quote,
            '`' => Token::BackQuote,
            '.' => Token::Dot,
            ',' => Token::Comma,
            _ => panic!("Invalid token: {}", ch),
        }
    }
}

impl From<Token> for String {
    fn from(token: Token) -> Self {
        match token {
            Token::ListOpen => "(".to_string(),
            Token::ListClose => ")".to_string(),
            Token::Quote => "'".to_string(),
            Token::BackQuote => "`".to_string(),
            Token::Dot => ".".to_string(),
            Token::Comma => ",".to_string(),
            Token::EOF => "<EOF>".to_string(),
            Token::String(s) => format!("{:?}", s),
            Token::Symbol(s) => s,
            Token::Char(ch) => ch.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct PositionalToken {
    pub start_idx: usize,
    pub end_idx: usize,
    pub token: Token,
}

impl PositionalToken {
    pub fn eof() -> Self {
        PositionalToken {
            token: Token::EOF,
            start_idx: std::usize::MAX,
            end_idx: std::usize::MAX,
        }
    }
}

impl From<PositionalToken> for String {
    fn from(token: PositionalToken) -> Self {
        token.token.into()
    }
}

impl From<PositionalToken> for Token {
    fn from(token: PositionalToken) -> Self {
        token.token
    }
}

impl From<(usize, char)> for PositionalToken {
    fn from((i, ch): (usize, char)) -> Self {
        PositionalToken {
            start_idx: i,
            end_idx: i + 1,
            token: ch.into(),
        }
    }
}

type CharI = <std::str::CharIndices<'static> as Iterator>::Item;

pub struct Lexer {
    comment_level: usize,
    token_stream: Vec<PositionalToken>,
    list_level: isize,
    source: String,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            comment_level: 0,
            list_level: 0,
            token_stream: vec![],
            source: String::new(),
        }
    }

    pub fn is_balanced(&self) -> bool {
        self.list_level <= 0
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn take(&mut self) -> Vec<PositionalToken> {
        std::mem::replace(&mut self.token_stream, vec![])
        //.into_iter()
        //.collect()
    }

    pub fn take_pos(&mut self) -> Vec<PositionalToken> {
        std::mem::replace(&mut self.token_stream, vec![])
    }

    pub fn tokenize(&mut self, input: &str) -> Result<&mut Self> {
        let mut chars = input.char_indices().peekable();
        while self.skip_whitespace(&mut chars) {
            if self.comment_level > 0 {
                self.read_block_comment(&mut chars)?;
            } else {
                let token = self.read_token(&mut chars)?;
                self.token_stream.extend(token);
            }
        }
        self.source.push_str(input);
        Ok(self)
    }

    /// read whitespace from char iterator and return true if
    fn skip_whitespace(&mut self, chars: &mut Peekable<impl Iterator<Item = CharI>>) -> bool {
        loop {
            match chars.peek() {
                Some((_, ch)) if ch.is_whitespace() => {
                    chars.next();
                }
                Some(_) => return true,
                None => return false,
            }
        }
    }

    fn read_token(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = CharI>>,
    ) -> Result<Option<PositionalToken>> {
        match chars.peek().unwrap().1 {
            '\'' | '`' | '.' | ',' => Ok(chars.next().map(PositionalToken::from)),
            '(' => {
                self.list_level += 1;
                Ok(chars.next().map(PositionalToken::from))
            }
            ')' => {
                self.list_level -= 1;
                Ok(chars.next().map(PositionalToken::from))
            }
            '"' => self.read_string(chars),
            '#' => self.read_hash(chars),
            ';' => self.read_line_comment(chars),
            _ => self.read_symbol(chars),
        }
    }

    // read characters until the first character (=delimiter) is read again.
    fn read_string(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = CharI>>,
    ) -> Result<Option<PositionalToken>> {
        let (start_idx, delimiter) = chars.next().unwrap();
        let mut buf = String::new();
        while let Some((idx, mut ch)) = chars.next() {
            if ch == '\\' {
                match chars.next() {
                    None => break,
                    Some((_, 'n')) => ch = '\n',
                    _ => return Err("Illegal character in escape sequence")?,
                }
            }

            if ch == delimiter {
                return Ok(Some(PositionalToken {
                    start_idx,
                    end_idx: idx,
                    token: Token::String(buf),
                }));
            }
            buf.push(ch)
        }
        Err(ErrorKind::UndelimitedString)?
    }

    fn read_symbol(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = CharI>>,
    ) -> Result<Option<PositionalToken>> {
        let mut buf = String::new();
        let (start_idx, _) = *chars.peek().ok_or_else(|| ErrorKind::UnexpectedToken {
            found: "<EOF>".into(),
            expected: "<identifier>".into(),
        })?;
        let mut last_idx = start_idx;
        let end_idx = loop {
            match chars.peek() {
                None => break last_idx + 1,
                Some(&(idx, ch)) if is_delimiter(ch) => break idx,
                Some(&(idx, _)) => {
                    last_idx = idx;
                    buf.push(chars.next().unwrap().1);
                }
            }
        };
        Ok(Some(PositionalToken {
            start_idx,
            end_idx,
            token: Token::Symbol(buf),
        }))
    }

    fn read_hash(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = CharI>>,
    ) -> Result<Option<PositionalToken>> {
        let (start_idx, ch) = chars.next().unwrap();
        assert_eq!('#', ch);
        match chars.next() {
            None => Err(ErrorKind::UnexpectedEof)?,
            Some((_, '!')) => {
                self.comment_level += 1;
                Ok(None)
            }
            Some((_, '\\')) => self.read_char(start_idx, chars),
            Some((end_idx, ch)) => Ok(Some(PositionalToken {
                start_idx,
                end_idx,
                token: Token::Symbol(format!("#{}", ch)),
            })),
        }
    }

    fn read_char(
        &mut self,
        start_idx: usize,
        chars: &mut Peekable<impl Iterator<Item = CharI>>,
    ) -> Result<Option<PositionalToken>> {
        let mut buf = String::new();
        let mut last_idx = start_idx;
        let end_idx = loop {
            match chars.peek() {
                None => break last_idx + 1,
                Some(&(idx, ch)) if is_delimiter(ch) => break idx,
                Some(&(idx, _)) => {
                    last_idx = idx;
                    buf.push(chars.next().unwrap().1);
                }
            }
        };

        let chr = match buf.as_str() {
            "" => ' ',
            _ if buf.chars().count() == 1 => buf.chars().next().unwrap(),
            // named characters required by R7RS
            "alarm" => '\u{0007}',
            "backspace" => '\u{0008}',
            "delete" => '\u{007F}',
            "escape" => '\u{001B}',
            "newline" => '\u{000A}',
            "null" => '\u{0000}',
            "return" => '\u{000D}',
            "space" => ' ',
            "tab" => '\u{0009}',
            _ => Err(ErrorKind::GenericError(
                "Not implemented: parsing character from hex code".to_string(),
            ))?,
        };

        Ok(Some(PositionalToken {
            start_idx,
            end_idx,
            token: Token::Char(chr),
        }))
    }

    fn read_block_comment(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = CharI>>,
    ) -> Result<()> {
        let mut last = (0, ' ');
        for ich in chars {
            if last.1 == '!' && ich.1 == '#' {
                self.comment_level -= 1;
                break;
            }
            last = ich;
        }
        Ok(())
    }

    fn read_line_comment(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = CharI>>,
    ) -> Result<Option<PositionalToken>> {
        for (_, ch) in chars {
            if ch == '\n' {
                break;
            }
        }
        Ok(None)
    }
}

fn is_special_char(ch: char) -> bool {
    match ch {
        '(' | ')' => true,
        _ => false,
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || is_special_char(ch)
}

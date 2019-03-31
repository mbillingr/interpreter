use crate::errors::*;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Token {
    ListOpen,
    ListClose,
    String(String),
    Symbol(String),
}

impl From<char> for Token {
    fn from(ch: char) -> Self {
        match ch {
            '(' => Token::ListOpen,
            ')' => Token::ListClose,
            _ => panic!("Invalid token: {}", ch),
        }
    }
}

impl From<Token> for String {
    fn from(token: Token) -> Self {
        match token {
            Token::ListOpen => "(".to_string(),
            Token::ListClose => ")".to_string(),
            Token::String(s) => format!("{:?}", s),
            Token::Symbol(s) => s,
        }
    }
}

pub struct Lexer {
    comment_level: usize,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer { comment_level: 0 }
    }

    pub fn tokenize(&mut self, input: String) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        let mut chars = input.chars().peekable();
        while self.skip_whitespace(&mut chars) {
            if self.comment_level > 0 {
                self.read_block_comment(&mut chars)?;
            } else {
                tokens.extend(self.read_token(&mut chars)?);
            }
        }
        Ok(tokens)
    }

    /// read whitespace from char iterator and return true if
    fn skip_whitespace(&mut self, chars: &mut Peekable<impl Iterator<Item = char>>) -> bool {
        loop {
            match chars.peek() {
                Some(ch) if ch.is_whitespace() => {
                    chars.next();
                }
                Some(_) => return true,
                None => return false,
            }
        }
    }

    fn read_token(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    ) -> Result<Option<Token>> {
        match chars.peek().unwrap() {
            '(' | ')' => Ok(chars.next().map(Token::from)),
            '"' => self.read_string(chars),
            '#' => self.read_hash(chars),
            ';' => self.read_line_comment(chars),
            _ => self.read_symbol(chars),
        }
    }

    // read characters until the first character (=delimiter) is read again.
    fn read_string(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    ) -> Result<Option<Token>> {
        let delimiter = chars.next().unwrap();
        let mut buf = String::new();
        for ch in chars {
            if ch == delimiter {
                return Ok(Some(Token::String(buf)));
            }
            buf.push(ch)
        }
        Err(ErrorKind::UndelimitedString)?
    }

    fn read_symbol(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    ) -> Result<Option<Token>> {
        let mut buf = String::new();
        loop {
            match chars.peek() {
                None => break,
                Some(ch) if ch.is_whitespace() => break,
                Some(ch) if is_special_char(*ch) => break,
                Some(_) => buf.push(chars.next().unwrap()),
            }
        }
        Ok(Some(Token::Symbol(buf)))
    }

    fn read_hash(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    ) -> Result<Option<Token>> {
        assert_eq!(Some('#'), chars.next());
        match chars.next() {
            None => Err(ErrorKind::UnexpectedEof)?,
            Some('!') => {
                self.comment_level += 1;
                Ok(None)
            }
            Some(ch) => Ok(Some(Token::Symbol(format!("#{}", ch)))),
        }
    }

    fn read_block_comment(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    ) -> Result<()> {
        let mut last = ' ';
        for ch in chars {
            if last == '!' && ch == '#' {
                self.comment_level -= 1;
                break;
            }
            last = ch;
        }
        Ok(())
    }

    fn read_line_comment(
        &mut self,
        chars: &mut Peekable<impl Iterator<Item = char>>,
    ) -> Result<Option<Token>> {
        for ch in chars {
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

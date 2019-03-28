use crate::errors::*;
use std::collections::VecDeque;
use std::io;
use utf8::{BufReadDecoder, BufReadDecoderError};

#[derive(Debug)]
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

pub struct Lexer<R: io::BufRead> {
    input: BufReadDecoder<R>,
    char_buffer: VecDeque<char>,
}

impl<R: io::BufRead> Lexer<R> {
    pub fn new(input: R) -> Self {
        Lexer {
            input: BufReadDecoder::new(input),
            char_buffer: VecDeque::new(),
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace()?;
        match self.peek()? {
            '(' | ')' => self.read_char().map(Token::from),
            '"' => self.read_string().map(Token::String),
            _ => self.read_symbol().map(Token::Symbol),
        }
    }

    pub fn read_char(&mut self) -> Result<char> {
        if self.char_buffer.is_empty() {
            self.read_next()?;
        }
        self.char_buffer
            .pop_front()
            .ok_or_else(|| ErrorKind::UnexpectedEof.into())
    }

    fn read_next(&mut self) -> Result<()> {
        match self.input.next_strict() {
            None => {}
            Some(Ok(s)) => self.char_buffer.extend(s.chars()),
            Some(Err(BufReadDecoderError::InvalidByteSequence(_))) => {
                return Err(ErrorKind::Utf8Error.into());
            }
            Some(Err(BufReadDecoderError::Io(e))) => return Err(e.into()),
        }
        Ok(())
    }

    fn peek(&mut self) -> Result<char> {
        if self.char_buffer.is_empty() {
            self.read_next()?;
        }
        self.char_buffer
            .front()
            .cloned()
            .ok_or_else(|| ErrorKind::UnexpectedEof.into())
    }

    fn skip_whitespace(&mut self) -> Result<()> {
        loop {
            match self.peek()? {
                ch if ch.is_whitespace() => {
                    self.read_char()?;
                }
                _ => break,
            }
        }
        Ok(())
    }

    fn expect(&mut self, ex: char) -> Result<()> {
        match self.read_char()? {
            ch if ch == ex => Ok(()),
            ch => Err(ErrorKind::UnexpectedCharacter(ex, ch).into()),
        }
    }

    fn read_string(&mut self) -> Result<String> {
        self.expect('"')?;
        let mut s = String::new();
        loop {
            match self.read_char()? {
                '"' => return Ok(s),
                ch => s.push(ch),
            }
        }
    }

    fn read_symbol(&mut self) -> Result<String> {
        let mut s = String::new();
        loop {
            match self.peek()? {
                '(' | ')' => return Ok(s),
                ch if ch.is_whitespace() => return Ok(s),
                _ => s.push(self.read_char().unwrap()),
            }
        }
    }
}

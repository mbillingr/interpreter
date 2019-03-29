use crate::errors::*;
use crate::io::LineReader;
use std::collections::VecDeque;
use std::io;

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

pub fn tokenize(input: String) -> impl Iterator<Item=Result<Token>> {
    let mut lexer = Lexer::new(input);
    (0..)
        .map(move |_| if lexer.pending() {Some(lexer.next_token())} else {None})
        .take_while(Option::is_some)
        .map(Option::unwrap)
}

pub struct Lexer {
    char_buffer: VecDeque<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer {
            char_buffer: input.chars().collect(),
        }
    }

    pub fn pending(&self) -> bool {
        !self.char_buffer.is_empty()
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
        self.char_buffer
            .pop_front()
            .ok_or_else(|| ErrorKind::UnexpectedEof.into())
    }

    fn peek(&mut self) -> Result<char> {
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

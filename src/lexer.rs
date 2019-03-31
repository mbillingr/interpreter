use crate::errors::*;
use std::iter::Peekable;

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

/*pub fn tokenize(input: String) -> impl Iterator<Item=Result<Token>> {
    let mut lexer = Lexer::new(input);
    (0..)
        .map(move |_| if lexer.pending() {Some(lexer.next_token())} else {None})
        .take_while(Option::is_some)
        .map(Option::unwrap)
}*/

pub fn tokenize(input: String) -> Result<Vec<Token>> {
    let mut tokens = vec![];
    let mut chars = input.chars().peekable();
    while skip_whitespace(&mut chars) {
        tokens.push(read_token(&mut chars)?);
    }
    Ok(tokens)
}

/// read whitespace from char iterator and return true if
fn skip_whitespace(chars: &mut Peekable<impl Iterator<Item = char>>) -> bool {
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

fn is_special_char(ch: char) -> bool {
    match ch {
        '(' | ')' => true,
        _ => false,
    }
}

fn read_token(chars: &mut Peekable<impl Iterator<Item = char>>) -> Result<Token> {
    match chars.peek().unwrap() {
        '(' | ')' => Ok(chars.next().map(Token::from).unwrap()),
        '"' => read_string(chars),
        _ => read_symbol(chars),
    }
}

// read characters until the first character (=delimiter) is read again.
fn read_string(chars: &mut Peekable<impl Iterator<Item = char>>) -> Result<Token> {
    let delimiter = chars.next().unwrap();
    let mut buf = String::new();
    while let Some(ch) = chars.next() {
        if ch == delimiter {
            return Ok(Token::String(buf));
        }
        buf.push(ch)
    }
    Err(ErrorKind::UndelimitedString)?
}

fn read_symbol(chars: &mut Peekable<impl Iterator<Item = char>>) -> Result<Token> {
    let mut buf = String::new();
    loop {
        match chars.peek() {
            None => break,
            Some(ch) if ch.is_whitespace() => break,
            Some(ch) if is_special_char(*ch) => break,
            Some(_) => buf.push(chars.next().unwrap()),
        }
    }
    Ok(Token::Symbol(buf))
}

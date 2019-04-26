use crate::errors::*;
use crate::expression::{Expression, Ref};
use crate::lexer::Lexer;
use crate::lexer::Token;
use std::fs;
use std::iter::Peekable;
use std::path::Path;

pub fn parse_file(path: impl AsRef<Path>) -> Result<Expression> {
    let mut lexer = Lexer::new();

    let mut result = Expression::Nil;
    let mut cursor = &mut result;

    for expr in parse(lexer.tokenize(fs::read_to_string(path)?)?.take())? {
        *cursor = Expression::cons(expr, Expression::Nil);
        cursor = cursor.cdr_mut()?;
    }

    Ok(result)
}

pub fn parse(input: impl IntoIterator<Item = Token>) -> Result<Vec<Expression>> {
    let mut input = input.into_iter().peekable();
    let mut output = vec![];
    loop {
        if let Token::EOF = peek_token(&mut input) {
            break;
        }
        let expr = parse_expression(&mut input)?;
        output.push(expr);
    }
    Ok(output)
}

fn parse_expression(input: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression> {
    match next_token(input) {
        Token::String(s) => Ok(Expression::String(Ref::new(s))),
        Token::Symbol(s) => Ok(Expression::from_literal(s)),
        Token::ListOpen => parse_list_open(input),
        Token::Quote => Ok(scheme!(quote, @parse_expression(input)?)),
        t => Err(ErrorKind::UnexpectedToken(t.into(), "<expression>".into()).into()),
    }
}

fn peek_token(input: &mut Peekable<impl Iterator<Item = Token>>) -> &Token {
    input.peek().unwrap_or(&Token::EOF)
}

fn next_token(input: &mut Peekable<impl Iterator<Item = Token>>) -> Token {
    input.next().unwrap_or(Token::EOF)
}

fn expect_token(token: Token, input: &mut Peekable<impl Iterator<Item = Token>>) -> Result<()> {
    match next_token(input) {
        ref t if t == &token => Ok(()),
        t => Err(ErrorKind::UnexpectedToken(t.into(), token.into()).into()),
    }
}

/*fn parse_list(input: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression> {
    expect_token(Token::ListOpen, input)?;
    parse_list_open(input)
}*/

fn parse_list_open(input: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression> {
    let mut list = Expression::Nil;
    let mut cursor = &mut list;
    loop {
        match peek_token(input) {
            Token::ListClose => break,
            Token::Dot => {
                expect_token(Token::Dot, input).unwrap();
                *cursor = parse_expression(input)?;
                break;
            }
            _ => {
                *cursor = Expression::cons(parse_expression(input)?, Expression::Nil);
                cursor = cursor.cdr_mut().unwrap();
            }
        }
    }
    expect_token(Token::ListClose, input)?;
    Ok(list)
}

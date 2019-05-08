use crate::errors::*;
use crate::expression::{Expression, Ref};
use crate::lexer::Lexer;
use crate::lexer::{PositionalToken, Token};
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

pub fn parse(input: impl IntoIterator<Item = PositionalToken>) -> Result<Vec<Expression>> {
    let mut input = input.into_iter().peekable();
    let mut output = vec![];
    loop {
        if peek_token(&mut input).is_none() {
            break;
        }
        let expr = parse_expression(&mut input)?;
        output.push(expr);
    }
    Ok(output)
}

fn parse_expression(
    input: &mut Peekable<impl Iterator<Item = PositionalToken>>,
) -> Result<Expression> {
    if let Some(pt) = peek_token(input) {
        if pt.token == Token::ListOpen {
            return parse_list(input);
        }
    }
    match next_token(input).token {
        Token::String(s) => Ok(Expression::String(Ref::new(s))),
        Token::Symbol(s) => Ok(Expression::from_literal(s)),
        Token::Quote => Ok(scheme!(quote, @parse_expression(input)?)),
        t => Err(ErrorKind::UnexpectedToken(t.into(), "<expression>".into()).into()),
    }
}

fn peek_token(
    input: &mut Peekable<impl Iterator<Item = PositionalToken>>,
) -> Option<&PositionalToken> {
    input.peek()
}

fn next_token(input: &mut Peekable<impl Iterator<Item = PositionalToken>>) -> PositionalToken {
    input.next().unwrap_or(PositionalToken::eof())
}

fn expect_token(
    token: Token,
    input: &mut Peekable<impl Iterator<Item = PositionalToken>>,
) -> Result<()> {
    match next_token(input) {
        ref t if t.token == token => Ok(()),
        t => Err(ErrorKind::UnexpectedToken(t.into(), token.into()).into()),
    }
}

fn parse_list(input: &mut Peekable<impl Iterator<Item = PositionalToken>>) -> Result<Expression> {
    expect_token(Token::ListOpen, input)?;

    let mut list = Expression::Nil;
    let mut cursor = &mut list;
    loop {
        if let Some(pt) = peek_token(input) {
            match pt.token {
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
        } else {
            Err(ErrorKind::UnexpectedEof)?
        }
    }

    expect_token(Token::ListClose, input)?;
    Ok(list)
}

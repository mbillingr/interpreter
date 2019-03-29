use crate::errors::*;
use crate::expression::{Expression, List};
use crate::lexer::{Lexer, Token};
use std::io;

pub fn parse<R: io::BufRead>(input: &mut Lexer<R>) -> Result<Expression> {
    fn read_ahead<R: io::BufRead>(token: Token, input: &mut Lexer<R>) -> Result<Expression> {
        match token {
            Token::String(s) => Ok(Expression::String(s)),
            Token::Symbol(s) => Ok(s.into()),
            Token::ListOpen => {
                let mut list = Vec::new();
                loop {
                    let token = input.next_token()?;
                    match token {
                        Token::ListClose => return Ok(Expression::List(list)),
                        _ => list.push(read_ahead(token, input)?),
                    }
                }
            }
            Token::ListClose => Err(ErrorKind::UnexpectedToken(token.into()).into()),
        }
    }

    let token = input.next_token()?;
    let expr = read_ahead(token, input)?;
    transform(expr)
}

// convert some syntactic forms, expand macros(?), check errors, ... (mostly to-do)
fn transform(expr: Expression) -> Result<Expression> {
    use Expression::*;
    match expr {
        List(l) => match l.first() {
            Some(Symbol(s)) if s == "define" => transform_define(l),
            _ => l
                .into_iter()
                .map(transform)
                .collect::<Result<_>>()
                .map(List),
        },
        _ => Ok(expr),
    }
}

fn transform_define(list: List) -> Result<Expression> {
    if list.len() < 3 {
        return Err(ErrorKind::ArgumentError.into());
    }

    let mut list: Vec<_> = list.into_iter().map(transform).collect::<Result<_>>()?;

    if list[1].is_symbol() {
        Ok(Expression::List(list))
    } else if list[1].is_list() {
        let mut new_body = vec![];
        while list.len() > 2 {
            new_body.push(list.pop().unwrap());
        }
        new_body.push(Expression::Symbol("begin".to_string()));
        new_body.reverse();
        list.push(Expression::List(new_body));
        Ok(Expression::List(list))
    } else {
        Err(ErrorKind::TypeError(format!("Cannot use {} as signature.", list[1])).into())
    }
}

fn transform_if(list: List) -> Result<Expression> {
    if list.len() < 3 || list.len() > 4 {
        return Err(ErrorKind::ArgumentError.into());
    }

    let mut list = list
        .into_iter()
        .map(transform)
        .collect::<Result<Vec<_>>>()?;

    if list.len() == 3 {
        list.push(Expression::Undefined);
    }

    Ok(Expression::List(list))
}

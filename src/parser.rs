use crate::errors::*;
use crate::expression::{Expression, Pair, Ref};
use crate::lexer::Lexer;
use crate::lexer::{PositionalToken, Token};
use crate::sourcecode::Source;
use std::fs;
use std::iter::Peekable;
use std::path::Path;

trait ParserInput {
    fn peek_token(&mut self) -> Option<&PositionalToken>;
    fn next_token(&mut self) -> PositionalToken;
    fn source(&self) -> &Source;
}

struct SourceIter<I>
where
    I: Iterator<Item = PositionalToken>,
{
    iter: Peekable<I>,
    src: Source,
}

impl<I: Iterator<Item = PositionalToken>> ParserInput for SourceIter<I> {
    fn peek_token(&mut self) -> Option<&PositionalToken> {
        self.iter.peek()
    }

    fn next_token(&mut self) -> PositionalToken {
        self.iter.next().unwrap_or_else(PositionalToken::eof)
    }

    fn source(&self) -> &Source {
        &self.src
    }
}

pub fn parse_file(path: impl AsRef<Path>) -> Result<Expression> {
    let src = fs::read_to_string(path)?;
    let mut lexer = Lexer::new();

    let result = parse(lexer.tokenize(&src)?.take(), src)?
        .into_iter()
        .collect();

    Ok(result)
}

pub fn parse(
    input: impl IntoIterator<Item = PositionalToken>,
    src: String,
) -> Result<Vec<Expression>> {
    let mut input = SourceIter {
        iter: input.into_iter().peekable(),
        src: src.into(),
    };

    let mut output = vec![];
    loop {
        if input.peek_token().is_none() {
            break;
        }
        let expr = parse_expression(&mut input)?;
        output.push(expr);
    }
    Ok(output)
}

/*pub fn read_str(src: String) -> Result<Expression> {
    let mut lexer = Lexer::new();
    lexer.tokenize(&src)?;
    read_lex(&mut lexer)
}*/

pub fn read_lex(lexer: &mut Lexer) -> Result<Expression> {
    let mut input = SourceIter {
        iter: lexer.take().into_iter().peekable(),
        src: lexer.source().to_owned().into(),
    };

    parse_expression(&mut input)
}

fn parse_expression(input: &mut impl ParserInput) -> Result<Expression> {
    if let Some(pt) = input.peek_token() {
        if pt.token == Token::ListOpen {
            return parse_list(input);
        }
    }
    match input.next_token().token {
        Token::String(s) => Ok(Expression::String(Ref::new(s))),
        Token::Symbol(s) => Ok(Expression::from_literal(s)),
        Token::Quote => Ok(scheme!(quote, @parse_expression(input)?)),
        Token::BackQuote => Ok(scheme!(quasiquote, @parse_expression(input)?)),
        Token::Comma => Ok(scheme!(unquote, @parse_expression(input)?)),
        Token::Char(ch) => Ok(Expression::Char(ch)),
        t => Err(ErrorKind::UnexpectedToken {
            found: t.into(),
            expected: "<expression>".into(),
        }
        .into()),
    }
}

fn expect_token(token: Token, input: &mut impl ParserInput) -> Result<PositionalToken> {
    let t = input.next_token();
    if t.token == token {
        Ok(t)
    } else {
        Err(ErrorKind::UnexpectedToken {
            found: t.into(),
            expected: token.into(),
        }
        .into())
    }
}

fn parse_list(input: &mut impl ParserInput) -> Result<Expression> {
    let start = expect_token(Token::ListOpen, input)?.start_idx;

    let mut list = Expression::Nil;
    let mut cursor = &mut list;
    loop {
        if let Some(pt) = input.peek_token() {
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

    let end = expect_token(Token::ListClose, input)?.end_idx;

    if let Expression::Pair(pair) = list {
        let pair = Ref::try_unwrap(pair).unwrap();
        list = Expression::Pair(Pair::new_sourced(
            pair.car,
            pair.cdr,
            Some(input.source().view(start, end)),
        ))
    }

    Ok(list)
}

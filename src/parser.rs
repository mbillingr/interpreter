use crate::errors::*;
use crate::expression::{Expression, Ref};
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::symbol;
use std::fs;
use std::iter::Peekable;
use std::path::{Path, PathBuf};

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
        output.push(transform(&expr)?);
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

fn parse_list(input: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression> {
    expect_token(Token::ListOpen, input)?;
    parse_list_open(input)
}

fn parse_list_open(input: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression> {
    let mut list = Expression::Nil;
    let mut cursor = &mut list;
    loop {
        match peek_token(input) {
            Token::ListClose => break,
            _ => {
                *cursor = Expression::cons(parse_expression(input)?, Expression::Nil);
                cursor = cursor.cdr_mut().unwrap();
            }
        }
    }
    expect_token(Token::ListClose, input)?;
    Ok(list)
}
/*
enum ParserState {
    List(Expression),
    Quote,
}

pub struct Parser {
    list_stack: Vec<ParserState>,
}

impl Parser {
    pub fn new() -> Self {
        Parser { list_stack: vec![] }
    }

    pub fn push_token(&mut self, token: Token) -> Result<Option<Expression>> {
        self.parse_expression(token)
            .and_then(|o| o.map(|e| transform(&e)).transpose())
    }

    fn parse_expression(&mut self, token: Token) -> Result<Option<Expression>> {
        let mut expr = match token {
            Token::String(s) => Expression::String(Ref::new(s)),
            Token::Symbol(s) => Expression::from_literal(s),
            Token::ListOpen => {
                self.list_stack.push(ParserState::List(Expression::Nil));
                return Ok(None);
            }
            Token::ListClose => match self.list_stack.pop() {
                Some(ParserState::List(list)) => list,
                _ => return Err(ErrorKind::UnexpectedToken(token.into(), "whatever".into()))?,
            },
            Token::Quote => {
                self.list_stack.push(ParserState::Quote);
                return Ok(None);
            }
            Token::Dot => {return  Ok(None)}
            Token::EOF => unimplemented!(),
        };

        loop {
            match self.list_stack.pop() {
                Some(ParserState::List(list)) => {
                    self.list_stack.push(ParserState::List(
                        list.append(Expression::cons(expr, Expression::Nil))?,
                    ));
                    return Ok(None);
                }
                Some(ParserState::Quote) => expr = scheme!(quote, @expr),
                None => return Ok(Some(expr)),
            }
        }
    }
}
*/

// convert some syntactic forms, expand macros(?), check errors, ... (mostly to-do)
fn transform(expr: &Expression) -> Result<Expression> {
    use Expression::*;
    match expr {
        Pair(ref car, _) => match **car {
            Symbol(s) if s == symbol::AND => transform_and(expr),
            Symbol(s) if s == symbol::COND => transform_cond(expr),
            Symbol(s) if s == symbol::DEFINE => transform_define(expr),
            Symbol(s) if s == symbol::IF => transform_if(expr),
            Symbol(s) if s == symbol::INCLUDE => transform_include(expr),
            Symbol(s) if s == symbol::LAMBDA => transform_lambda(expr),
            Symbol(s) if s == symbol::LET => transform_let(expr),
            Symbol(s) if s == symbol::OR => transform_or(expr),
            _ => expr.map_list(|e| transform(&e)),
        },
        _ => Ok(expr.clone()),
    }
}

fn transform_define(list: &Expression) -> Result<Expression> {
    assert_eq!(&scheme!(define), list.car()?);
    let (signature, body) = list.cdr()?.decons().map_err(|_| ErrorKind::ArgumentError)?;

    if signature.is_symbol() {
        if body.cdr()? != &Expression::Nil {
            return Err(ErrorKind::ArgumentError)?;
        }
        let value = body.car()?;
        Ok(scheme!(define, @signature.clone(), @transform(&value)?))
    } else if signature.is_list() {
        let (name, signature) = signature.decons().map_err(|_| ErrorKind::ArgumentError)?;

        let lambda = scheme!(lambda, @signature.clone(), ...body.clone());
        let lambda = transform_lambda(&lambda)?;

        Ok(scheme!(define, @name.clone(), @lambda))
    } else {
        Err(ErrorKind::TypeError(format!("invalid signature: {:?}", signature)).into())
    }
}

fn transform_lambda(list: &Expression) -> Result<Expression> {
    assert_eq!(&scheme!(lambda), list.car()?);
    let (signature, body) = list.cdr()?.decons().map_err(|_| ErrorKind::ArgumentError)?;

    if body.cdr().unwrap() == &Expression::Nil {
        Ok(scheme!(lambda, @signature.clone(), @transform(body.car()?)?))
    } else {
        let body = Expression::cons(scheme!(begin), body.map_list(|e| transform(&e))?);
        Ok(scheme!(lambda, @signature.clone(), @body))
    }
}

fn transform_cond(list: &Expression) -> Result<Expression> {
    assert_eq!(&scheme!(cond), list.car()?);
    let body = list.cdr()?.map_list(|row| {
        let row = match row {
            Expression::Pair(car, cdr) => {
                let car = if let Expression::Symbol(s) = &**car {
                    if *s == symbol::ELSE {
                        Ref::new(Expression::True)
                    } else {
                        car.clone()
                    }
                } else {
                    car.clone()
                };
                Expression::Pair(car, cdr.clone())
            }
            row => row.clone(),
        };
        transform(&row)
    })?;
    Ok(Expression::cons(scheme!(cond), body))
}

fn transform_if(list: &Expression) -> Result<Expression> {
    let mut list = list.iter_list();

    assert_eq!(Some(&scheme!(if)), list.next_expr()?);
    let cond = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
    let if_ = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
    let else_ = list.next_expr()?.unwrap_or(&Expression::Undefined);

    Ok(scheme!(if, @transform(cond)?, @transform(if_)?, @transform(else_)?))
}

fn transform_let(list: &Expression) -> Result<Expression> {
    let mut list = list.iter_list();

    assert_eq!(Some(&scheme!(let)), list.next_expr()?);

    // need to get the tail first, because next_expr() advances the iterator into the tail
    let body = list.tail()?;
    let assignments = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;

    let mut vars = Expression::Nil;
    let mut exps = Expression::Nil;

    let mut var_cursor = &mut vars;
    let mut exp_cursor = &mut exps;

    for vx in assignments.iter_list() {
        let mut vx = vx?.iter_list();
        let var = vx.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
        let exp = vx.next_expr()?.ok_or(ErrorKind::ArgumentError)?;

        *var_cursor = Expression::cons(var.clone(), Expression::Nil);
        var_cursor = var_cursor.cdr_mut().unwrap();

        *exp_cursor = Expression::cons(exp.clone(), Expression::Nil);
        exp_cursor = exp_cursor.cdr_mut().unwrap();
    }

    let lambda_form = scheme!(lambda, @vars, ...body.clone());
    exps = Expression::cons(lambda_form, exps);
    transform(&exps)
}

fn transform_or(list: &Expression) -> Result<Expression> {
    let (cmd, args) = list.decons()?;
    assert_eq!(&scheme!(or), cmd);
    let mapped = args.map_list(|x| Ok(scheme!((@x.clone()))))?;
    let mapped = mapped.append(scheme!(((#t, #f))))?;
    transform_cond(&scheme!(cond, ...mapped))
}

fn transform_and(list: &Expression) -> Result<Expression> {
    let (cmd, args) = list.decons()?;
    assert_eq!(&scheme!(and), cmd);

    match args {
        Expression::Nil => Ok(Expression::True),
        Expression::Pair(car, cdr) if **cdr == Expression::Nil => Ok((**car).clone()),
        Expression::Pair(car, cdr) => transform_if(
            &scheme!(if, @(**car).clone(), @transform_and(&scheme!(and, ...(**cdr).clone()))?, #f),
        ),
        _ => unreachable!(),
    }
}

fn transform_include(list: &Expression) -> Result<Expression> {
    let mut list = list.iter_list();
    assert_eq!(Some(&scheme!(include)), list.next_expr()?);

    let mut result = scheme!((begin));

    for filename in list {
        let filename = filename?.try_as_str()?;
        let path =
            find_file(filename).ok_or_else(|| ErrorKind::FileNotFoundError(filename.to_owned()))?;
        result = result.append(parse_file(path)?)?;
    }

    Ok(result)
}

/// super primitive implementation that does not attempt any search path and file extension magic.
fn find_file(path: impl AsRef<Path>) -> Option<PathBuf> {
    let path = path.as_ref();
    if path.is_file() {
        Some(path.to_owned())
    } else {
        None
    }
}

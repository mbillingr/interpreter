use crate::errors::*;
use crate::expression::{Expression, Ref};
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::symbol;
use std::fs;
use std::iter::Peekable;
use std::path::{Path, PathBuf};
use crate::environment::EnvRef;

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
        output.push(expand(&expr)?);
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

// convert some syntactic forms, expand macros(?), check errors, ... (mostly to-do)
fn expand(expr: &Expression, env: &EnvRef) -> Result<Expression> {
    use Expression::*;
    match expr {
        Pair(ref car, _) => {
            let syntax_macro = if let Symbol(s) = car {
                match env.borrow().lookup(s) {
                    Some(Expression::Macro(m)) => Some(m),
                    _ => None,
                }
            } else {
                None
            };
            match **car {
                Symbol(s) if syntax_macro.is_some() => syntax_macro.unwrap().expand(expr),
                Symbol(s) if s == symbol::AND => expand_and(expr),
                Symbol(s) if s == symbol::COND => expand_cond(expr),
                Symbol(s) if s == symbol::DEFINE => expand_define(expr),
                Symbol(s) if s == symbol::IF => expand_if(expr),
                Symbol(s) if s == symbol::INCLUDE => expand_include(expr),
                Symbol(s) if s == symbol::LAMBDA => expand_lambda(expr),
                Symbol(s) if s == symbol::LET => expand_let(expr),
                Symbol(s) if s == symbol::OR => expand_or(expr),
                Symbol(s) if s == symbol::QUOTE => Ok(expr.clone()),
                _ => expr.map_list(|e| expand(&e)),
            }
        },
        _ => Ok(expr.clone()),
    }
}

fn expand_define(list: &Expression) -> Result<Expression> {
    assert_eq!(&scheme!(define), list.car()?);
    let (signature, body) = list.cdr()?.decons().map_err(|_| ErrorKind::ArgumentError)?;

    if signature.is_symbol() {
        if body.cdr()? != &Expression::Nil {
            return Err(ErrorKind::ArgumentError)?;
        }
        let value = body.car()?;
        Ok(scheme!(define, @signature.clone(), @expand(&value)?))
    } else if signature.is_pair() {
        let (name, signature) = signature.decons().map_err(|_| ErrorKind::ArgumentError)?;

        let lambda = scheme!(lambda, @signature.clone(), ...body.clone());
        let lambda = expand_lambda(&lambda)?;

        Ok(scheme!(define, @name.clone(), @lambda))
    } else {
        Err(ErrorKind::TypeError(format!("invalid signature: {:?}", signature)).into())
    }
}

fn expand_lambda(list: &Expression) -> Result<Expression> {
    assert_eq!(&scheme!(lambda), list.car()?);
    let (signature, body) = list.cdr()?.decons().map_err(|_| ErrorKind::ArgumentError)?;

    if body.cdr().unwrap() == &Expression::Nil {
        Ok(scheme!(lambda, @signature.clone(), @expand(body.car()?)?))
    } else {
        let body = Expression::cons(scheme!(begin), body.map_list(|e| expand(&e))?);
        Ok(scheme!(lambda, @signature.clone(), @body))
    }
}

fn expand_cond(list: &Expression) -> Result<Expression> {
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
        expand(&row)
    })?;
    Ok(Expression::cons(scheme!(cond), body))
}

fn expand_if(list: &Expression) -> Result<Expression> {
    let mut list = list.iter_list();

    assert_eq!(Some(&scheme!(if)), list.next_expr()?);
    let cond = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
    let if_ = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
    let else_ = list.next_expr()?.unwrap_or(&Expression::Undefined);

    Ok(scheme!(if, @expand(cond)?, @expand(if_)?, @expand(else_)?))
}

fn expand_let(list: &Expression) -> Result<Expression> {
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
    expand(&exps)
}

fn expand_or(list: &Expression) -> Result<Expression> {
    let (cmd, args) = list.decons()?;
    assert_eq!(&scheme!(or), cmd);
    let mapped = args.map_list(|x| Ok(scheme!((@x.clone()))))?;
    let mapped = mapped.append(scheme!(((#t, #f))))?;
    expand_cond(&scheme!(cond, ...mapped))
}

fn expand_and(list: &Expression) -> Result<Expression> {
    let (cmd, args) = list.decons()?;
    assert_eq!(&scheme!(and), cmd);

    match args {
        Expression::Nil => Ok(Expression::True),
        Expression::Pair(car, cdr) if **cdr == Expression::Nil => Ok((**car).clone()),
        Expression::Pair(car, cdr) => expand_if(
            &scheme!(if, @(**car).clone(), @expand_and(&scheme!(and, ...(**cdr).clone()))?, #f),
        ),
        _ => unreachable!(),
    }
}

fn expand_include(list: &Expression) -> Result<Expression> {
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

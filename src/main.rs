#[macro_use]
extern crate error_chain;

mod environment;
mod errors;
mod expression;
mod lexer;

use environment::{default_env, EnvRef, Environment};
use error_chain::ChainedError;
use errors::*;
use expression::{Expression, List, Procedure};
use lexer::{Lexer, Token};
use std::io::{self, Write};

fn parse<R: io::BufRead>(input: &mut Lexer<R>) -> Result<Expression> {
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
            _ => l.into_iter().map(transform).collect::<Result<_>>().map(List)
        }
        _ => Ok(expr)
    }
}

/// simple version without tail calls
fn eval(mut expr: Expression, mut env: EnvRef) -> Result<Expression> {
    use Expression::*;
    loop {
        match expr {
            Symbol(s) => return env
                .borrow()
                .lookup(&s)
                .ok_or_else(|| ErrorKind::Undefined(s).into()),
            Undefined | Nil | Integer(_) | Float(_) | String(_) | True | False | Procedure(_) => {
                return Ok(expr)
            }
            Native(_) => return Ok(expr),
            List(l) => match l.first() {
                None => return Ok(Nil),
                Some(Symbol(s)) if s == "begin" => expr = begin(l, env.clone())?,
                Some(Symbol(s)) if s == "cond" => match cond(l, env.clone())? {
                    Return::RetVal(ex) => return Ok(ex),
                    Return::TailCall(ex) => expr = ex,
                }
                Some(Symbol(s)) if s == "define" => return define(l, env.clone()),
                Some(Symbol(s)) if s == "if" => expr = if_form(l, env.clone())?,
                Some(_) => {
                    let mut items = l.into_iter();
                    let proc = eval(items.next().unwrap(), env.clone())?;
                    let args: Vec<_> =
                        items.map(|arg| eval(arg, env.clone())).collect::<Result<_>>()?;
                    match proc {
                        Procedure(p) => {
                            let local_env = Environment::new(env.clone());
                            local_env.borrow_mut().set_vars(p.params.as_slice(), args)?;
                            expr = p.body_ex();
                            env = local_env;
                        }
                        Native(func) => return func(args),
                        _ => return Err(ErrorKind::TypeError("not callable".to_string()).into()),
                    }
                }
            },
        }
    }
}

fn begin(mut list: List, env: EnvRef) -> Result<Expression> {
    if list.len() < 2 {
        return Err(ErrorKind::ArgumentError.into())
    }
    let last = list.pop().unwrap();
    for expr in list.into_iter().skip(1) {
        eval(expr, env.clone())?;
    }
    Ok(last)
}

fn transform_define(mut list: List) -> Result<Expression> {
    if list.len() < 3 {
        return Err(ErrorKind::ArgumentError.into())
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

fn define(mut list: List, env: EnvRef) -> Result<Expression> {
    assert_eq!(3, list.len());
    let body = list.pop().unwrap();
    let signature = list.pop().unwrap();

    match signature {
        Expression::Symbol(s) => {
            let value = eval(body, env.clone())?;
            env.borrow_mut().insert(s, value);
        }
        Expression::List(sig) => {
            let proc = Procedure::build(sig, body)?;
            env.borrow_mut()
                .insert(proc.name.clone().unwrap(), Expression::Procedure(proc));
        }
        _ => {
            return Err(
                ErrorKind::TypeError(format!("Cannot use {} as signature.", signature)).into(),
            );
        }
    }

    Ok(Expression::Undefined)
}

enum Return {
    RetVal(Expression),
    TailCall(Expression),
}

fn cond(list: List, env: EnvRef) -> Result<Return> {
    for pair in list.into_iter().skip(1) {
        let mut row = pair.try_into_list()?;
        let last = if row.len() > 1 {
            row.pop()
        } else {
            None
        };
        let mut row = row.into_iter();
        let cond = row.next().ok_or(ErrorKind::ArgumentError)?;
        let mut result = eval(cond, env.clone())?;
        if result.is_true() {
            if last.is_none() {
                return Ok(Return::RetVal(result))
            }
            for action in row {
                eval(action, env.clone())?;
            }
            return Ok(Return::TailCall(last.unwrap()))
        }
    }
    Ok(Return::RetVal(Expression::Undefined))
}

fn transform_if(list: List) -> Result<Expression> {
    if list.len() < 3 || list.len() > 4 {
        return Err(ErrorKind::ArgumentError.into())
    }

    let mut list = list.into_iter().map(transform).collect::<Result<Vec<_>>>()?;

    if list.len() == 3 {
        list.push(Expression::Undefined);
    }

    Ok(Expression::List(list))
}

fn if_form(list: List, env: EnvRef) -> Result<Expression> {
    let mut list = list.into_iter().skip(1);

    let cond = list.next().unwrap();
    let then = list.next().unwrap();
    let otherwise = list.next().unwrap();

    if eval(cond, env.clone())?.is_true() {
        Ok(then)
    } else {
        Ok(otherwise)
    }
}

fn repl<R: io::BufRead>(input: &mut Lexer<R>, global: EnvRef) -> Result<()> {
    print!(">> ");
    io::stdout().flush().unwrap();
    let expr = parse(input)?;
    match eval(expr, global)? {
        Expression::Undefined => println!(),
        res => println!("{}", res),
    }
    Ok(())
}

fn main() {
    let mut src = Lexer::new(io::BufReader::new(io::stdin()));
    let global = default_env();
    loop {
        match repl(&mut src, global.clone()) {
            Ok(_) => {}
            Err(Error(ErrorKind::UnexpectedEof, _)) => {
                println!("EOF");
                break;
            }
            Err(e) => report_error(e),
        }
    }
}

fn report_error(e: Error) {
    eprintln!("{}", e);
    eprintln!("{}", e.display_chain().to_string());
}

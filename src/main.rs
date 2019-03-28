#[macro_use]
extern crate error_chain;

mod environment;
mod errors;
mod expression;
mod lexer;

use environment::{default_env, EnvTmpRef, Environment};
use error_chain::ChainedError;
use errors::*;
use expression::Expression;
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
    read_ahead(token, input)
}

/// simple version without tail calls
fn eval(expr: Expression, env: EnvTmpRef) -> Result<Expression> {
    use Expression::*;
    println!("eval: {}", expr);
    match expr {
        Symbol(s) => env
            .borrow()
            .lookup(&s)
            .cloned()
            .ok_or_else(|| ErrorKind::Undefined(s).into()),
        Undefined | Nil | Integer(_) | Float(_) | String(_) | True | False | Procedure(_) => {
            Ok(expr)
        }
        Native(_) => Ok(expr),
        List(l) => match l.first() {
            None => Ok(Nil),
            Some(Symbol(s)) if s == "define" => env.borrow_mut().define(l),
            Some(_) => {
                println!("{:?}", l);
                let mut items = l.into_iter();
                let proc = eval(items.next().unwrap(), env)?;
                println!("proc: {:?}", proc);
                let args: Vec<Expression> =
                    items.map(|arg| eval(arg, env)).collect::<Result<_>>()?;
                match proc {
                    Procedure(p) => {
                        let local_env = Environment::new(p.env.clone());
                        local_env.borrow_mut().set_vars(p.params.as_slice(), args);
                        eval(p.body_ex(), env)
                    }
                    Native(func) => func(args),
                    _ => Err(ErrorKind::TypeError("not callable".to_string()).into()),
                }
            }
        },
    }
}

fn repl<R: io::BufRead>(input: &mut Lexer<R>, global: EnvTmpRef) -> Result<()> {
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
    let mut global = default_env();
    loop {
        match repl(&mut src, &mut global) {
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

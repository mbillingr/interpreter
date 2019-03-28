#[macro_use]
extern crate error_chain;

mod environment;
mod errors;
mod expression;
mod lexer;

use environment::Environment;
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
fn eval(expr: Expression, env: &mut Environment) -> Result<Expression> {
    match expr {
        Expression::Symbol(s) => env
            .lookup(&s)
            .cloned()
            .ok_or_else(|| ErrorKind::Undefined(s).into()),
        Expression::Undefined
        | Expression::Nil
        | Expression::Integer(_)
        | Expression::Float(_)
        | Expression::String(_)
        | Expression::True
        | Expression::False
        | Expression::Native(_) => Ok(expr),
        Expression::List(l) => match l.first() {
            None => Ok(Expression::Nil),
            Some(_) => {
                let mut items = l.into_iter();
                let proc = eval(items.next().unwrap(), env)?;
                let args: Vec<Expression> =
                    items.map(|arg| eval(arg, env)).collect::<Result<_>>()?;
                proc.call(args)
            }
        },
    }
}

fn repl<R: io::BufRead>(input: &mut Lexer<R>, global: &mut Environment) -> Result<()> {
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
    let mut global = Environment::default();
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

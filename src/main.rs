#[macro_use]
extern crate error_chain;

mod errors;
mod expression;
mod lexer;

use error_chain::ChainedError;
use errors::*;
use expression::Expression;
use lexer::{Lexer, Token};
use std::collections::HashMap;
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

type Environment = HashMap<String, Expression>;

fn default_env() -> Environment {
    let mut env = HashMap::new();
    env.insert("+".to_string(), Expression::Native(native_add));
    env
}

fn native_add(args: Vec<Expression>) -> Result<Expression> {
    args.into_iter().sum()
}

/// simple version without tail calls
fn eval(expr: Expression, env: &mut Environment) -> Result<Expression> {
    match expr {
        Expression::Symbol(s) => env
            .get(&s)
            .cloned()
            .ok_or_else(|| ErrorKind::Undefined(s).into()),
        Expression::Integer(_)
        | Expression::Float(_)
        | Expression::String(_)
        | Expression::True
        | Expression::False
        | Expression::Native(_) => Ok(expr),
        Expression::List(l) => match l.first() {
            None => panic!("empty list"),
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
    let res = eval(expr, global)?;
    println!("{}", res);
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

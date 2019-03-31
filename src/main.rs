#[macro_use]
extern crate error_chain;

mod environment;
mod errors;
mod expression;
mod interpreter;
mod io;
mod lexer;
mod parser;

use crate::io::LineReader;
use environment::{default_env, EnvRef};
use error_chain::ChainedError;
use errors::*;
use expression::Expression;
use interpreter::eval;
use lexer::tokenize;
use parser::Parser;
use std::env;

fn repl(input: &mut impl LineReader, env: EnvRef) -> Result<()> {
    let mut parser = Parser::new();

    loop {
        for token in tokenize(input.read_line()?)? {
            if let Some(expr) = parser.push_token(token)? {
                match eval(expr, env.clone())? {
                    Expression::Undefined => {}
                    res => println!("{}", res),
                }
            }
        }
    }
}

fn run_file(input: &mut impl LineReader, env: EnvRef) -> Result<()> {
    let mut parser = Parser::new();

    while !input.is_eof() {
        for token in tokenize(input.read_line()?)? {
            println!("{:?}", token);
            if let Some(expr) = parser.push_token(token)? {
                match eval(expr, env.clone())? {
                    Expression::Undefined => {}
                    res => println!("{}", res),
                }
            }
        }
    }
    Ok(())
}

fn main() {
    let global = default_env();

    for arg in env::args().skip(1) {
        match arg {
            _ => {
                let mut file = io::FileInput::new(&arg).unwrap();
                run_file(&mut file, global.clone()).unwrap();
            }
        }
    }

    let mut input = io::ReplInput::new();

    loop {
        match repl(&mut input, global.clone()) {
            Ok(_) => {}
            Err(Error(ErrorKind::ReadlineError(rustyline::error::ReadlineError::Eof), _)) => {
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

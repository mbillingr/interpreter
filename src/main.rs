#[macro_use]
extern crate error_chain;

mod environment;
mod errors;
mod expression;
mod interpreter;
mod io;
mod lexer;
mod parser;

use environment::{default_env, EnvRef};
use error_chain::ChainedError;
use errors::*;
use expression::Expression;
use interpreter::eval;
use lexer::tokenize;
use parser::Parser;
use std::env;
use crate::io::LineReader;

fn repl(env: EnvRef) -> Result<()> {
    let mut parser = Parser::new();

    let mut input = io::ReplInput::new();

    for token in tokenize(input.read_line()?) {
        let token = token?;
        loop {
            let expr = loop {
                match parser.push_token(token)? {
                    Some(expr) => break expr,
                    None => {}
                }
            };

            match eval(expr, env.clone())? {
                Expression::Undefined => {},
                res => println!("{}", res),
            }
        }
    }
    Ok(())
}

fn main() {
    let global = default_env();

    for arg in env::args().skip(1) {
    }

    loop {
        match repl(global.clone()) {
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

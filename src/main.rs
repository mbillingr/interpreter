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

fn repl(env: EnvRef) -> Result<()> {
    let mut parser = Parser::new();

    let mut input = io::ReplInput::new();

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

fn main() {
    let global = default_env();

    for arg in env::args().skip(1) {}

    loop {
        match repl(global.clone()) {
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

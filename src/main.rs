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
use lexer::Lexer;
use parser::Parser;
use std::env;

const LINE_PROMPT: &str = ">> ";
const MULTI_PROMPT: &str = " ... ";

fn repl(input: &mut impl LineReader, env: EnvRef) -> Result<()> {
    let mut lexer = Lexer::new();
    let mut parser = Parser::new();

    loop {
        for token in lexer.tokenize(input.read_line()?)? {
            if let Some(expr) = parser.push_token(token)? {
                match eval(expr, env.clone())? {
                    Expression::Undefined => {}
                    res => println!("{}", res),
                }
                input.set_prompt(LINE_PROMPT);
            } else {
                input.set_prompt(MULTI_PROMPT);
            }
        }
    }
}

fn run_file(input: &mut impl LineReader, env: EnvRef) -> Result<()> {
    let mut lexer = Lexer::new();
    let mut parser = Parser::new();

    while !input.is_eof() {
        let line = input.read_line()?;
        for token in lexer.tokenize(line)? {
            if let Some(expr) = parser.push_token(token)? {
                eval(expr, env.clone())?;
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
                match run_file(&mut file, global.clone()) {
                    Err(e) => {
                        report_error(e);
                        return;
                    }
                    Ok(_) => {}
                }
            }
        }
    }

    let mut input = io::ReplInput::new(LINE_PROMPT);

    loop {
        input.set_prompt(LINE_PROMPT);
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

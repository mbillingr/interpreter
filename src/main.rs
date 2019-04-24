#[macro_use]
extern crate error_chain;
#[macro_use]
mod scheme;

#[cfg(test)]
mod tests;

mod completer;
mod destructure;
mod environment;
mod errors;
mod expression;
mod interpreter;
mod io;
mod lexer;
mod libraries;
mod parser;
mod symbol;
mod tracer;

use crate::environment::Environment;
use crate::io::LineReader;
use crate::libraries::import_library;
use environment::EnvRef;
use error_chain::ChainedError;
use errors::*;
use expression::{Expression, Ref};
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
            if let Some(expr) = parser.push_token(token.into())? {
                match eval(&expr, env.clone())? {
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
            if let Some(expr) = parser.push_token(token.into())? {
                eval(&expr, env.clone())?;
            }
        }
    }
    Ok(())
}

fn main() {
    let global: EnvRef = Environment::new(None).into();

    for arg in env::args().skip(1) {
        match arg {
            _ => {
                let mut file = io::FileInput::new(&arg).unwrap();
                if let Err(e) = run_file(&mut file, global.clone()) {
                    report_error(e);
                    return;
                }
            }
        }
    }

    // make the core library available to the repl
    import_library(&scheme! {((builtin, core))}, &global).unwrap();

    let mut input = io::ReplInput::new(LINE_PROMPT);
    input.set_env(Ref::downgrade(&global));

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

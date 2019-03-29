#[macro_use]
extern crate error_chain;

mod environment;
mod errors;
mod expression;
mod interpreter;
mod lexer;
mod parser;

use environment::{default_env, EnvRef};
use error_chain::ChainedError;
use errors::*;
use expression::{Expression};
use interpreter::eval;
use lexer::Lexer;
use std::io::{self, Write};
use parser::parse;



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

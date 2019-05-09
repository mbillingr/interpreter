#[macro_use]
extern crate error_chain;
#[macro_use]
mod scheme;

#[cfg(test)]
mod tests;

#[cfg(feature = "source-tracking")]
mod sourcecode;

mod completer;
mod destructure;
mod environment;
mod envref;
mod errors;
mod expression;
mod interpreter;
mod io;
mod lexer;
mod libraries;
mod macros;
mod parser;
mod symbol;
mod syntax;
mod tracer;

use crate::environment::Environment;
use crate::io::LineReader;
use crate::libraries::import_library;
use crate::parser::parse_file;
use crate::syntax::expand;
use environment::EnvRef;
use error_chain::ChainedError;
use errors::*;
use expression::Expression;
use interpreter::eval;
use lexer::Lexer;
use parser::parse;
use std::env;
use std::path::Path;

const LINE_PROMPT: &str = ">> ";
const MULTI_PROMPT: &str = " ... ";

fn repl(input: &mut impl LineReader, env: &EnvRef) -> Result<()> {
    let mut source = String::new();

    input.set_prompt(LINE_PROMPT);
    loop {
        source.push_str(&input.read_line()?);

        let mut lexer = Lexer::new();
        lexer.tokenize(&source)?;

        if lexer.is_balanced() {
            let mut result = Expression::Undefined;
            for expr in parse(lexer.take(), source)? {
                let expr = expand(&expr, env)?;
                result = eval(&expr, env.clone())?;
            }
            match result {
                Expression::Undefined => {}
                res => println!("{}", res),
            }
            return Ok(());
        } else {
            input.set_prompt(MULTI_PROMPT);
        }
    }
}

fn run_file(path: impl AsRef<Path>, env: &EnvRef) -> Result<()> {
    let expr = parse_file(path)?;
    let expr = expand(&expr, env)?;
    eval(&expr, env.clone())?;
    Ok(())
}

fn main() {
    let global: EnvRef = Environment::new(None).into();

    for arg in env::args().skip(1) {
        match arg {
            _ => {
                if let Err(e) = run_file(arg, &global) {
                    report_error(e);
                    return;
                }
            }
        }
    }

    // make the core library available to the repl
    import_library(&scheme! {((builtin, core))}, &global).unwrap();

    let mut input = io::ReplInput::new(LINE_PROMPT);
    input.set_env(global.downgrade());

    loop {
        input.set_prompt(LINE_PROMPT);
        match repl(&mut input, &global) {
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

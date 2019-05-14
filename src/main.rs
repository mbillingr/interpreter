#[macro_use]
extern crate error_chain;
#[macro_use]
mod scheme;

#[cfg(test)]
mod tests;

mod completer;
mod debugger;
mod debugger_imgui_frontend;
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
mod sourcecode;
mod symbol;
mod syntax;

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
            let expr = parse(lexer.take(), source)?.into_iter().collect();
            match run_program(&expr, env)? {
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
    let prog = parse_file(path)?;
    run_program(&prog, env)?;
    Ok(())
}

fn run_program(mut prog: &Expression, env: &EnvRef) -> Result<Expression> {
    while !prog.is_nil() {
        let decl = prog.car()?;
        match decl.car() {
            Ok(Expression::Symbol(s)) if *s == symbol::IMPORT => {
                import_library(decl.cdr()?, &env)?;
                prog = prog.cdr().unwrap();
            }
            _ => break,
        }
    }

    if prog.is_nil() {
        return Ok(Expression::Undefined);
    }

    let prog = Expression::cons(symbol::BEGIN, prog.clone());
    let prog = expand(&prog, env)?;
    eval(&prog, env.clone())
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

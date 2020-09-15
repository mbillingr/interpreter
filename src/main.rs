#![recursion_limit = "128"]

#[macro_use]
mod destructure;
#[macro_use]
mod scheme;

#[cfg(test)]
mod tests;

mod completer;
mod debugger;
#[cfg(feature = "debugging")]
mod debugger_imgui_frontend;
mod environment;
mod envref;
mod errors;
mod expression;
#[cfg(feature = "debugging")]
mod global_thread_state;
mod integer;
mod interpreter;
mod io;
mod lexer;
mod libraries;
mod macros;
mod native_closure;
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
use errors::*;
use expression::Expression;
use interpreter::eval;
use lexer::Lexer;
use parser::parse;
use std::env;
use std::path::{Path, PathBuf};

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
            match run_program(&expr, env, None)? {
                Expression::Undefined => {}
                res => println!("{}", res),
            }
            return Ok(());
        } else {
            input.set_prompt(MULTI_PROMPT);
        }
    }
}

fn run_file(path: &impl AsRef<Path>, env: &EnvRef) -> Result<()> {
    let prog = parse_file(path)?;
    run_program(&prog, env, Some(path.as_ref().to_path_buf()))?;
    Ok(())
}

fn run_program(
    mut prog: &Expression,
    env: &EnvRef,
    current_file: Option<PathBuf>,
) -> Result<Expression> {
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

    let mut state = syntax::State::default();
    if let Some(file) = current_file {
        state = state.with_file(file);
    }

    let prog = Expression::cons(symbol::BEGIN, prog.clone());
    let prog = expand(&prog, env, &state)?;
    eval(&prog, env.clone())
}

fn main() {
    let global: EnvRef = Environment::new(None).into();

    // make the core library available to the repl
    import_library(&scheme! {((builtin, core))}, &global).unwrap();

    for arg in env::args().skip(1) {
        match arg {
            _ => {
                if let Err(e) = run_file(&arg, &global) {
                    report_error(e);
                    return;
                }
            }
        }
    }

    let mut input = io::ReplInput::new(LINE_PROMPT);
    input.set_env(global.downgrade());

    loop {
        input.set_prompt(LINE_PROMPT);
        match repl(&mut input, &global) {
            Ok(_) => {}
            Err(e) => {
                if let ErrorKind::ReadlineError(rustyline::error::ReadlineError::Eof) = e.kind() {
                    println!("EOF");
                    break;
                }
                report_error(e)
            }
        }
    }
}

fn report_error(e: Error) {
    eprintln!("{}", e);
}

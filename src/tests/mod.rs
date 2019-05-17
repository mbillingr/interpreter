mod r7rs_6_2_equivalence_predicates;
mod r7rs_procedure_apply;

use crate::environment::default_env;
use crate::envref::EnvRef;
use crate::errors::Result;
use crate::eval;
use crate::parser::parse;
use crate::syntax::{self, expand};
use crate::Expression;
use crate::Lexer;

fn run(src: &str) -> Result<Expression> {
    run_in_env(src, &default_env())
}

fn run_in_env(src: &str, env: &EnvRef) -> Result<Expression> {
    let mut lexer = Lexer::new();
    let tokens = lexer
        .tokenize(src)?
        .take()
        .into_iter()
        .map(|ptoken| ptoken.into());

    parse(tokens, src.to_string())?
        .into_iter()
        .map(|expr| expand(&expr, env, &syntax::State::default()))
        .map(|expr| eval(&expr?, env.clone()))
        .last()
        .unwrap()
}

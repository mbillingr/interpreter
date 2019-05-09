mod r7rs_6_2_equivalence_predicates;

use crate::environment::default_env;
use crate::errors::Result;
use crate::eval;
use crate::parser::parse;
use crate::syntax::expand;
use crate::Expression;
use crate::Lexer;

fn run(src: &str) -> Result<Expression> {
    let mut lexer = Lexer::new();
    let env = default_env();
    let tokens = lexer
        .tokenize(src)?
        .take()
        .into_iter()
        .map(|ptoken| ptoken.into());

    parse(tokens, src.to_string())?
        .into_iter()
        .map(|expr| expand(&expr, &env))
        .map(|expr| eval(&expr?, env.clone()))
        .last()
        .unwrap()
}

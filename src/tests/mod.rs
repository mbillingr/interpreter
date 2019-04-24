mod r7rs_6_2_equivalence_predicates;

use crate::environment::default_env;
use crate::errors::Result;
use crate::eval;
use crate::parser::parse;
use crate::Expression;
use crate::Lexer;

fn run<T: ToString>(src: T) -> Result<Expression> {
    let mut lexer = Lexer::new();
    let env = default_env();
    let tokens = lexer
        .tokenize(src.to_string())?
        .take()
        .into_iter()
        .map(|ptoken| ptoken.into());

    parse(tokens)?
        .into_iter()
        .map(|expr| eval(&expr, env.clone()))
        .last()
        .unwrap()
}

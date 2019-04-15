mod r7rs_6_2_equivalence_predicates;

use crate::default_env;
use crate::errors::Result;
use crate::eval;
use crate::Expression;
use crate::Lexer;
use crate::Parser;

fn run<T: ToString>(src: T) -> Result<Expression> {
    let mut lexer = Lexer::new();
    let mut parser = Parser::new();
    let env = default_env();
    lexer
        .tokenize(src.to_string())?
        .into_iter()
        .map(|ptoken| ptoken.into())
        .filter_map(|token| parser.push_token(token).transpose())
        .map(|expr| eval(&expr?, env.clone()))
        .last()
        .unwrap()
}

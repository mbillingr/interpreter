use crate::errors::*;
use crate::expression::Expression;
use crate::symbol::{self, Symbol};
use rand::distributions::Exp;
use std::collections::HashMap;

macro_rules! hashmap(
    { $($key:expr => $value:expr),* } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )*
            m
        }
     };
);

#[derive(Clone)]
pub struct Macro {
    name: Symbol,
}

impl Macro {
    pub fn name(&self) -> Symbol {
        self.name
    }
}

struct TransformerSpec {
    literals: Vec<Symbol>,
    ellipsis: Symbol,
    rules: Vec<SyntaxRule>,
}

struct SyntaxRule {
    pattern: Pattern,
    template: Template,
}

enum Pattern {
    Literal(Symbol),
    Identifier(Symbol),
    Constant(Expression),
    List(Vec<Pattern>),
    ImproperList(Vec<Pattern>, Box<Pattern>),
}

impl Pattern {
    fn match_expr(&self, expr: &Expression) -> Option<HashMap<Symbol, Expression>> {
        match self {
            Pattern::Constant(x) if x.equal(expr) => Some(hashmap! {}),
            Pattern::Constant(_) => None,
            Pattern::Literal(s) => expr
                .try_as_symbol()
                .ok()
                .filter(|&x| x == s)
                .map(|_| hashmap! {}),
            Pattern::Identifier(s) => {
                if *s != symbol::UNDERSCORE {
                    Some(hashmap! {*s => expr.clone()})
                } else {
                    Some(hashmap! {})
                }
            }
            Pattern::List(ps) => match Pattern::match_list(ps, expr) {
                Some((bindings, tail)) => {
                    if tail == Expression::Nil {
                        Some(bindings)
                    } else {
                        None
                    }
                }
                _ => None,
            },
            Pattern::ImproperList(ps, p_tail) => match Pattern::match_list(ps, expr) {
                Some((mut bindings, tail)) => p_tail.match_expr(&tail).map(|b| {
                    bindings.extend(b);
                    bindings
                }),
                _ => None,
            },
        }
    }

    fn match_list(
        patterns: &[Pattern],
        mut expr: &Expression,
    ) -> Option<(HashMap<Symbol, Expression>, Expression)> {
        let mut bindings = HashMap::new();
        for p in patterns {
            match p.match_expr(expr.car().ok()?) {
                None => return None,
                Some(b) => bindings.extend(b.into_iter()),
            }
            expr = expr.cdr().ok()?;
        }
        Some((bindings, expr.clone()))
    }
}

enum Template {
    Identifier,
    Constant,
    List(Vec<Pattern>),
}

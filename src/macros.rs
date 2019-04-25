use crate::errors::*;
use crate::expression::Expression;
use crate::symbol::{self, Symbol};
use rand::distributions::Exp;
use std::collections::HashMap;

macro_rules! hashmap(
    { $($key:ident => $value:ident),* } => {
        {
            let mut m = ::std::collections::HashMap::<Symbol, Expression>::new();
            $(
                m.insert(stringify!($key).into(), Symbol::new(stringify!($value)).into());
            )*
            m
        }
     };
    { $($key:ident => $value:expr),* } => {
        {
            let mut m = ::std::collections::HashMap::<Symbol, Expression>::new();
            $(
                m.insert(stringify!($key).into(), $value.into());
            )*
            m
        }
     };

    { $($key:expr => $value:expr),* } => {
        {
            let mut m = ::std::collections::HashMap::<Symbol, Expression>::new();
            $(
                m.insert($key.into(), $value.into());
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn match_constant() {
        assert_eq!(
            Some(hashmap! {}),
            Pattern::Constant(42.into()).match_expr(&scheme!(42))
        );
        assert_eq!(None, Pattern::Constant(5.into()).match_expr(&scheme!(9)));
    }

    #[test]
    fn match_literal() {
        assert_eq!(
            Some(hashmap! {}),
            Pattern::Literal(Symbol::new("abc")).match_expr(&scheme!(abc))
        );
        assert_eq!(
            None,
            Pattern::Literal(Symbol::new("abc")).match_expr(&scheme!(xyz))
        );
        assert_eq!(
            None,
            Pattern::Literal(Symbol::new("abc")).match_expr(&scheme!(42))
        );
    }

    #[test]
    fn match_identifier() {
        assert_eq!(
            Some(hashmap! {abc => 5}),
            Pattern::Identifier(Symbol::new("abc")).match_expr(&scheme!(5))
        );
        assert_eq!(
            Some(hashmap! {abc => xyz}),
            Pattern::Identifier(Symbol::new("abc")).match_expr(&scheme!(xyz))
        );
        assert_eq!(
            Some(hashmap! {abc => scheme!(uvw, xyz)}),
            Pattern::Identifier(Symbol::new("abc")).match_expr(&scheme!(uvw, xyz))
        );
    }

    #[test]
    fn match_list() {
        let pat = Pattern::List(vec![
            Pattern::Identifier(Symbol::new("abc")),
            Pattern::Identifier(Symbol::new("xyz")),
        ]);

        assert_eq!(
            Some(hashmap! {abc => 5, xyz => 6}),
            pat.match_expr(&scheme!(5, 6))
        );
        assert_eq!(None, pat.match_expr(&scheme!(5, 6, 7)));
        assert_eq!(None, pat.match_expr(&scheme!(5)));
    }

    #[test]
    fn match_improper() {
        let pat = Pattern::ImproperList(
            vec![
                Pattern::Identifier("a".into()),
                Pattern::Identifier("b".into()),
            ],
            Box::new(Pattern::Identifier("tail".into())),
        );

        assert_eq!(None, pat.match_expr(&scheme!(1)));
        assert_eq!(
            Some(hashmap! {a => 1, b => 2, tail => Expression::Nil}),
            pat.match_expr(&scheme!(1, 2))
        );
        assert_eq!(
            Some(hashmap! {a => 1, b => 2, tail => scheme!((3))}),
            pat.match_expr(&scheme!(1, 2, 3))
        );
        assert_eq!(
            Some(hashmap! {a => 1, b => 2, tail => scheme!((3, 4))}),
            pat.match_expr(&scheme!(1, 2, 3, 4))
        );
        assert_eq!(
            Some(hashmap! {a => 1, b => 2, tail => 3}),
            pat.match_expr(&Expression::cons(1, Expression::cons(2, 3)))
        );
        assert_eq!(
            Some(hashmap! {a => 1, b => 2, tail => Expression::cons(3, 4)}),
            pat.match_expr(&Expression::cons(
                1,
                Expression::cons(2, Expression::cons(3, 4))
            ))
        );
    }
}

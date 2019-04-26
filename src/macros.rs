use crate::errors::*;
use crate::expression::Expression;
use crate::symbol::{self, Symbol};
use std::collections::HashMap;
use std::rc::Rc;

macro_rules! hashmap(
    { } => {
        {
            ::std::collections::HashMap::<Symbol, Expression>::new()
        }
     };

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

#[derive(Debug, Clone)]
pub struct Macro {
    name: Symbol,
    spec: Rc<TransformerSpec>,
}

impl Macro {
    pub fn name(&self) -> Symbol {
        self.name
    }

    pub fn parse(expr: &Expression) -> Result<Self> {
        let (name, rules) = expr.decons()?;
        let rules = rules.car()?;
        match rules.car()? {
            Expression::Symbol(s) if *s != symbol::SYNTAX_RULES => {
                return Err(ErrorKind::GenericError("expected syntax-rules".into()).into())
            }
            _ => {}
        }

        let spec = TransformerSpec::parse(rules.cdr()?)?;

        Ok(Macro {
            name: *name.try_as_symbol()?,
            spec: Rc::new(spec),
        })
    }

    pub fn expand(&self, expr: &Expression) -> Result<Expression> {
        self.spec.expand(expr)
    }
}

#[derive(Debug)]
struct TransformerSpec {
    literals: Vec<Symbol>,
    ellipsis: Symbol,
    rules: Vec<SyntaxRule>,
}

impl TransformerSpec {
    pub fn parse(mut list: &Expression) -> Result<Self> {
        let mut ellipsis = symbol::ELLIPSIS;
        let spec = match list.car()? {
            Expression::Symbol(s) => {
                ellipsis = *s;
                list = list.cdr()?;
                list.car().unwrap()
            }
            expr => expr,
        };

        let literals: Vec<Symbol> = spec
            .iter_list()
            .map(|x| x.and_then(Expression::try_as_symbol).map(|s| *s))
            .collect::<Result<_>>()?;

        let rules: Vec<SyntaxRule> = list
            .cdr()?
            .iter_list()
            .map(|x| x.and_then(|x| SyntaxRule::parse(x, &literals, ellipsis)))
            .collect::<Result<_>>()?;

        Ok(TransformerSpec {
            literals,
            ellipsis,
            rules,
        })
    }

    pub fn expand(&self, expr: &Expression) -> Result<Expression> {
        for rule in &self.rules {
            if let Some(expansion) = rule.match_expand(expr) {
                return Ok(expansion);
            }
        }
        Err(ErrorKind::GenericError("no pattern matched".into()).into())
    }
}

#[derive(Debug)]
struct SyntaxRule {
    pattern: Pattern,
    template: Template,
}

impl SyntaxRule {
    pub fn parse(list: &Expression, literals: &[Symbol], ellipsis: Symbol) -> Result<Self> {
        Ok(SyntaxRule {
            pattern: Pattern::parse(list.car()?, literals, ellipsis)?,
            template: Template::parse(list.cdr()?.car()?, literals, ellipsis)?,
        })
    }

    pub fn match_expand(&self, expr: &Expression) -> Option<Expression> {
        self.pattern
            .match_expr(expr)
            .map(|bindings| self.template.expand(&bindings))
    }
}

#[derive(Debug, PartialEq)]
enum Pattern {
    Literal(Symbol),
    Identifier(Symbol),
    Constant(Expression),
    List(Vec<Pattern>),
    ImproperList(Vec<Pattern>, Box<Pattern>),
}

impl Pattern {
    pub fn parse(expr: &Expression, literals: &[Symbol], ellipsis: Symbol) -> Result<Self> {
        match expr {
            Expression::Symbol(s) => Ok(if literals.contains(s) {
                Pattern::Literal(*s)
            } else {
                Pattern::Identifier(*s)
            }),
            Expression::Pair(ref car, ref cdr) => {
                let (mut car, mut cdr) = (car, cdr);
                let mut list = vec![];
                loop {
                    list.push(Pattern::parse(&*car, literals, ellipsis)?);
                    match &**cdr {
                        Expression::Nil => return Ok(Pattern::List(list)),
                        Expression::Pair(a, d) => {
                            car = a;
                            cdr = d;
                        }
                        _ => {
                            return Ok(Pattern::ImproperList(
                                list,
                                Box::new(Pattern::parse(&*cdr, literals, ellipsis)?),
                            ))
                        }
                    }
                }
            }
            _ => Ok(Pattern::Constant(expr.clone())),
        }
    }

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

#[derive(Debug, PartialEq)]
enum Template {
    Identifier(Symbol),
    Constant(Expression),
    List(Vec<Template>),
    ImproperList(Vec<Template>, Box<Template>),
}

impl Template {
    pub fn parse(expr: &Expression, literals: &[Symbol], ellipsis: Symbol) -> Result<Self> {
        match expr {
            Expression::Symbol(s) => Ok(Template::Identifier(*s)),
            Expression::Pair(ref car, ref cdr) => {
                let (mut car, mut cdr) = (car, cdr);
                let mut list = vec![];
                loop {
                    list.push(Template::parse(&*car, literals, ellipsis)?);
                    match &**cdr {
                        Expression::Nil => return Ok(Template::List(list)),
                        Expression::Pair(a, d) => {
                            car = a;
                            cdr = d;
                        }
                        _ => {
                            return Ok(Template::ImproperList(
                                list,
                                Box::new(Template::parse(&*cdr, literals, ellipsis)?),
                            ))
                        }
                    }
                }
            }
            _ => Ok(Template::Constant(expr.clone())),
        }
    }

    fn expand(&self, bindings: &HashMap<Symbol, Expression>) -> Expression {
        match self {
            Template::Constant(expr) => expr.clone(),
            Template::Identifier(ident) => match bindings.get(ident) {
                Some(expr) => expr.clone(),
                None => (*ident).into(),
            },
            Template::List(subs) => {
                let mut result = Expression::Nil;
                for sub in subs.iter().rev() {
                    let expr = sub.expand(bindings);
                    result = Expression::cons(expr, result);
                }
                result
            }
            Template::ImproperList(subs, tail) => {
                let mut result = tail.expand(bindings);
                for sub in subs.iter().rev() {
                    let expr = sub.expand(bindings);
                    result = Expression::cons(expr, result);
                }
                result
            }
        }
    }
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

    #[test]
    fn expand() {
        let template = Template::List(vec![
            Template::Identifier("if".into()),
            Template::Identifier("cond".into()),
            Template::Identifier("yes".into()),
            Template::Identifier("no".into()),
        ]);

        assert_eq!(
            scheme!(if, (less, x, 0), (neg, x), x),
            template.expand(
                &hashmap! { cond => scheme!(less, x, 0), yes => scheme!(neg, x), no => scheme!(x)}
            )
        );
    }

    #[test]
    fn parse_spec() {
        let expr = scheme!(
            my_ellipsis,
            (lit1, lit2),
            (42, "the answer!"),
            ((lit1, x, lit2), (x, x))
        );

        let spec = TransformerSpec::parse(&expr).unwrap();

        assert_eq!(vec![Symbol::new("lit1"), "lit2".into()], spec.literals);
        assert_eq!(Symbol::new("my_ellipsis"), spec.ellipsis);
        assert_eq!(Pattern::Constant(42.into()), spec.rules[0].pattern);
        assert_eq!(
            Template::Constant("the answer!".into()),
            spec.rules[0].template
        );
        assert_eq!(
            Pattern::List(vec![
                Pattern::Literal("lit1".into()),
                Pattern::Identifier("x".into()),
                Pattern::Literal("lit2".into())
            ]),
            spec.rules[1].pattern
        );
        assert_eq!(
            Template::List(vec![
                Template::Identifier("x".into()),
                Template::Identifier("x".into())
            ]),
            spec.rules[1].template
        );
    }
}

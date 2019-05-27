use crate::envref::EnvRef;
use crate::errors::*;
use crate::expression::{Expression, Ref};
use crate::symbol::{self, Symbol};
use crate::syntax::{self, expand};
use std::collections::HashMap;

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

#[derive(Clone)]
pub struct Macro {
    name: Symbol,
    spec: Ref<TransformerSpec>,
}

impl Macro {
    pub fn name(&self) -> Symbol {
        self.name
    }

    pub fn parse(expr: &Expression, env: &EnvRef) -> Result<Self> {
        let (name, rules) = expr.decons()?;
        let rules = rules.car()?;
        match rules.car()? {
            Expression::Symbol(s) if *s != symbol::SYNTAX_RULES => {
                return Err(ErrorKind::GenericError("expected syntax-rules".into()).into())
            }
            _ => {}
        }

        let spec = TransformerSpec::parse(rules.cdr()?, env)?;

        Ok(Macro {
            name: *name.try_as_symbol()?,
            spec: Ref::new(spec),
        })
    }

    pub fn expand(
        &self,
        expr: &Expression,
        env: &EnvRef,
        state: &syntax::State,
    ) -> Result<Expression> {
        self.spec.expand(expr, env, state)
    }
}

struct TransformerSpec {
    //literals: Vec<Symbol>,
    //ellipsis: Symbol,
    rules: Vec<SyntaxRule>,
}

impl TransformerSpec {
    pub fn parse(mut list: &Expression, env: &EnvRef) -> Result<Self> {
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
            .map(|x| x.and_then(|x| SyntaxRule::parse(x, &literals, ellipsis, env)))
            .collect::<Result<_>>()?;

        Ok(TransformerSpec {
            //literals,
            //ellipsis,
            rules,
        })
    }

    pub fn expand(
        &self,
        expr: &Expression,
        env: &EnvRef,
        state: &syntax::State,
    ) -> Result<Expression> {
        for rule in &self.rules {
            if let Some(expansion) = rule.match_expand(expr, env, state) {
                return expansion;
            }
        }
        Err(ErrorKind::GenericError("no pattern matched".into()).into())
    }
}

struct SyntaxRule {
    pattern: Pattern,
    template: Template,
}

impl SyntaxRule {
    pub fn parse(
        list: &Expression,
        literals: &[Symbol],
        ellipsis: Symbol,
        env: &EnvRef,
    ) -> Result<Self> {
        let pattern = Pattern::parse(list.car()?, literals, ellipsis)?;
        Ok(SyntaxRule {
            template: Template::parse(
                list.cdr()?.car()?,
                literals,
                &pattern.identifiers(),
                ellipsis,
                env,
            )?,
            pattern,
        })
    }

    pub fn match_expand(
        &self,
        expr: &Expression,
        env: &EnvRef,
        state: &syntax::State,
    ) -> Option<Result<Expression>> {
        self.pattern
            .match_expr(expr)
            .map(|bindings| self.template.expand(&bindings, env, state))
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
            Expression::Pair(pair) => {
                let mut car = &pair.car;
                let mut cdr = &pair.cdr;
                let mut list = vec![];
                loop {
                    list.push(Pattern::parse(car, literals, ellipsis)?);
                    match cdr {
                        Expression::Nil => return Ok(Pattern::List(list)),
                        Expression::Pair(p) => {
                            car = &p.car;
                            cdr = &p.cdr;
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

    fn identifiers(&self) -> Vec<Symbol> {
        match self {
            Pattern::Identifier(s) => vec![*s],
            Pattern::List(patterns) => patterns.iter().flat_map(|p| p.identifiers()).collect(),
            Pattern::ImproperList(patterns, tail) => {
                let mut ids: Vec<_> = patterns.iter().flat_map(|p| p.identifiers()).collect();
                ids.extend(tail.identifiers());
                ids
            }
            _ => vec![],
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
    pub fn parse(
        expr: &Expression,
        literals: &[Symbol],
        identifiers: &[Symbol],
        ellipsis: Symbol,
        env: &EnvRef,
    ) -> Result<Self> {
        match expr {
            Expression::Symbol(s) => {
                if identifiers.contains(s) {
                    Ok(Template::Identifier(*s))
                } else {
                    Ok(env
                        .borrow()
                        .lookup(s)
                        .map(Template::Constant)
                        .unwrap_or_else(|| Template::Identifier(*s)))
                }
                /*Ok(env
                .borrow()
                .lookup(s)
                .map(|x| Template::Constant(x))
                .unwrap_or_else(|| Template::Identifier(*s)))*/
                //Ok(Template::Identifier(*s))
                //env.borrow().lookup(s).map(|x| Template::Constant(x)).ok_or_else(|| ErrorKind::Undefined(*s).into())
            }
            Expression::Pair(pair) => {
                let mut car = &pair.car;
                let mut cdr = &pair.cdr;
                let mut list = vec![];
                loop {
                    list.push(Template::parse(
                        &*car,
                        literals,
                        identifiers,
                        ellipsis,
                        env,
                    )?);
                    match cdr {
                        Expression::Nil => return Ok(Template::List(list)),
                        Expression::Pair(p) => {
                            car = &p.car;
                            cdr = &p.cdr;
                        }
                        _ => {
                            return Ok(Template::ImproperList(
                                list,
                                Box::new(Template::parse(
                                    &*cdr,
                                    literals,
                                    identifiers,
                                    ellipsis,
                                    env,
                                )?),
                            ))
                        }
                    }
                }
            }
            _ => Ok(Template::Constant(expr.clone())),
        }
    }

    fn expand(
        &self,
        bindings: &HashMap<Symbol, Expression>,
        env: &EnvRef,
        state: &syntax::State,
    ) -> Result<Expression> {
        match self {
            Template::Constant(expr) => Ok(expr.clone()),
            Template::Identifier(ident) => match bindings.get(ident) {
                Some(expr) => Ok(expr.clone()),
                None => Ok((*ident).into()),
                //None => Err(ErrorKind::Undefined(*ident).into()),
            },
            Template::List(subs) => {
                let mut result = Expression::Nil;
                for sub in subs.iter().rev() {
                    let expr = sub.expand(bindings, env, state)?;
                    result = Expression::cons(expr, result);
                }
                let result = expand(&result, env, state)?;
                match result.car() {
                    Ok(Expression::Macro(m)) => m.expand(&result, env, state),
                    Ok(Expression::NativeMacro(m)) => m(&result, env, state),
                    _ => Ok(result),
                }
            }
            Template::ImproperList(subs, tail) => {
                let mut result = tail.expand(bindings, env, state)?;
                for sub in subs.iter().rev() {
                    let expr = sub.expand(bindings, env, state)?;
                    result = Expression::cons(expr, result);
                }
                Ok(result)
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
            Template::Identifier("cond".into()),
            Template::Identifier("yes".into()),
            Template::Identifier("no".into()),
        ]);

        use crate::environment::Environment;
        let env: EnvRef = Environment::new(None).into();

        assert_eq!(
            scheme!((less, x, 0), (neg, x), x),
            template.expand(
                &hashmap! { cond => scheme!(less, x, 0), yes => scheme!(neg, x), no => scheme!(x)},
                &env, &syntax::State::default()
            ).unwrap()
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

        use crate::environment::Environment;
        let env: EnvRef = Environment::new(None).into();

        let spec = TransformerSpec::parse(&expr, &env).unwrap();

        //assert_eq!(vec![Symbol::new("lit1"), "lit2".into()], spec.literals);
        //assert_eq!(Symbol::new("my_ellipsis"), spec.ellipsis);
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

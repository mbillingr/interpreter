use crate::environment::EnvRef;
use crate::errors::*;
use crate::expression::{cons, Expression, Pair};
use crate::parser::parse_file;
use crate::symbol;
use std::path::{Path, PathBuf};

#[derive(Default)]
pub struct State {
    current_file: Option<PathBuf>,
}

impl State {
    pub fn with_file(mut self, path: impl Into<PathBuf>) -> Self {
        self.current_file = Some(path.into());
        self
    }
}

pub fn expand(expr: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    inner_expand(expr, env, state).map_err(|e| e.with_context(expr.clone()))
}

pub fn inner_expand(expr: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    use Expression::*;
    match expr {
        Pair(pair) => {
            let src = pair.get_source();
            let car = &pair.car;
            match car {
                Symbol(s) => match env.borrow().lookup(s) {
                    Some(Expression::Macro(m)) => {
                        let expanded_macro = m.expand(expr, env, state).map(|x| x.sourced(src))?;
                        return expand(&expanded_macro, env, state);
                    }
                    Some(Expression::NativeMacro(m)) => return m(expr, env, state),
                    _ => {}
                },
                _ => {}
            }
            expr.map_list(|e| expand(&e, env, state))
                .map(|x| x.sourced(src))
        }
        _ => Ok(expr.clone()),
    }
}

/*pub fn car_to_special(list: &Expression, _env: &EnvRef, _state: &State) -> Result<Expression> {
    if let Expression::Symbol(s) = list.car()? {
        Ok(cons(Expression::Special(*s), list.cdr().unwrap().clone()))
    } else {
        panic!("need symbol in car position")
    }
}*/

pub fn car_to_special(list: &Expression, s: symbol::Symbol) -> Result<Expression> {
    Ok(cons(Expression::Special(s), list.cdr().unwrap().clone()))
}

pub fn expand_and(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    let (_, args) = list.decons()?;
    //assert_eq!(&scheme!(and), cmd);

    match args {
        Expression::Nil => Ok(Expression::True),
        Expression::Pair(p) if p.cdr == Expression::Nil => Ok((p.car).clone()),
        Expression::Pair(p) => expand_if(
            &scheme!(if, @(p.car).clone(), @expand_and(&scheme!(and, ...(p.cdr).clone()), env, state)?, #f),
            env,
            state,
        ),
        _ => unreachable!(),
    }
}

pub fn expand_begin(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    //assert_eq!(&scheme!(begin), list.car()?);
    let body = list.cdr().unwrap().map_list(|e| expand(&e, env, state))?;
    Ok(cons(Expression::Special(symbol::BEGIN), body))
}

pub fn expand_case(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    let mut list = list.iter_list();

    //assert_eq!(Some(&scheme!(case)), list.next_expr()?);
    assert!(list.next_expr()?.is_some());
    let key = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?.clone();

    let clauses: Vec<&Expression> = list.collect::<Result<_>>()?;

    let mut body = Expression::Undefined;

    for clause in clauses.into_iter().rev() {
        let data = clause.car()?.clone();
        let exprs = clause.cdr()?.clone();

        if let Ok(&s) = data.try_as_symbol() {
            if s == symbol::ELSE {
                body = scheme!(begin, ...exprs);
                continue;
            }
        }
        let conds = data.map_list(|d| Ok(scheme!(@{symbol::IS_EQV}, x, (quote, @d.clone()))))?;
        body = scheme!(if, (or, ...conds), (begin, ...exprs), @body);
    }

    let body = scheme!(let, ((x, @key)), @body);
    expand_let(&body, env, state)
}

pub fn expand_cond(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    //assert_eq!(&scheme!(cond), list.car()?);
    let body = list.cdr()?.map_list(|row| {
        let row = match row {
            Expression::Pair(pair) => {
                let Pair { car, cdr, .. } = &**pair;
                let car = if let Expression::Symbol(s) = car {
                    if *s == symbol::ELSE {
                        Expression::True
                    } else {
                        car.clone()
                    }
                } else {
                    car.clone()
                };
                Expression::cons(car, cdr.clone())
            }
            row => row.clone(),
        };
        expand(&row, env, state)
    })?;
    Ok(cons(Expression::Special(symbol::COND), body))
}

pub fn expand_define(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    //assert_eq!(&scheme!(define), list.car()?);
    let (signature, body) = list.cdr()?.decons().map_err(|_| ErrorKind::ArgumentError)?;

    let definition = if signature.is_symbol() {
        if body.cdr()? != &Expression::Nil {
            return Err(ErrorKind::ArgumentError)?;
        }
        let value = body.car()?;
        conslist!(signature.clone(), expand(value, env, state)?)
    } else if signature.is_pair() {
        let (name, signature) = signature.decons().map_err(|_| ErrorKind::ArgumentError)?;

        let lambda = scheme!(lambda, @signature.clone(), ...body.clone());
        let lambda = expand_lambda(&lambda, env, state)?;

        conslist!(name.clone(), lambda)
    } else {
        Err(ErrorKind::TypeError(format!(
            "invalid signature: {:?}",
            signature
        )))?
    };

    Ok(cons(Expression::Special(symbol::DEFINE), definition))
}

pub fn expand_if(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    let mut list = list.iter_list();

    //assert_eq!(Some(&scheme!(if)), list.next_expr()?);
    assert!(list.next_expr()?.is_some());
    let cond = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
    let if_ = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
    let else_ = list.next_expr()?.unwrap_or(&Expression::Undefined);

    Ok(conslist!(
        Expression::Special(symbol::IF),
        expand(cond, env, state)?,
        expand(if_, env, state)?,
        expand(else_, env, state)?
    ))
}

pub fn expand_include(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    let mut list = list.iter_list();
    //assert_eq!(Some(&scheme!(include)), list.next_expr()?);
    assert!(list.next_expr()?.is_some());

    let mut result = scheme!((Expression::Special(symbol::BEGIN)));

    for filename in list {
        let filename = filename?.try_as_str()?;
        let path = find_file(filename, state)
            .ok_or_else(|| ErrorKind::FileNotFoundError(filename.to_owned()))?;
        let expr = parse_file(path)?;
        let expr = expand(&expr, env, state)?;
        result = result.append(expr)?;
    }

    Ok(result)
}

pub fn expand_lambda(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    //assert_eq!(&scheme!(lambda), list.car()?);
    let (signature, body) = list.cdr()?.decons().map_err(|_| ErrorKind::ArgumentError)?;

    let body = body.map_list(|e| expand(&e, env, state))?;
    Ok(cons(
        Expression::Special(symbol::LAMBDA),
        cons(signature.clone(), body),
    ))
}

pub fn expand_let(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    let mut list = list.iter_list();

    //assert_eq!(Some(&scheme!(let)), list.next_expr()?);
    assert!(list.next_expr()?.is_some());

    // need to get the tail first, because next_expr() advances the iterator into the tail
    let body = list.tail()?;
    let assignments = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;

    let mut vars = Expression::Nil;
    let mut exps = Expression::Nil;

    let mut var_cursor = &mut vars;
    let mut exp_cursor = &mut exps;

    for vx in assignments.iter_list() {
        let mut vx = vx?.iter_list();
        let var = vx.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
        let exp = vx.next_expr()?.ok_or(ErrorKind::ArgumentError)?;

        *var_cursor = Expression::cons(var.clone(), Expression::Nil);
        var_cursor = var_cursor.cdr_mut().unwrap();

        *exp_cursor = Expression::cons(exp.clone(), Expression::Nil);
        exp_cursor = exp_cursor.cdr_mut().unwrap();
    }

    let lambda_form = scheme!(lambda, @vars, ...body.clone());
    exps = Expression::cons(lambda_form, exps);
    expand(&exps, env, state)
}

pub fn expand_lets(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    let mut list = list.iter_list();

    list.next_expr()?;

    //assert_eq!(Some(&scheme!(let)), list.next_expr()?);

    // need to get the tail first, because next_expr() advances the iterator into the tail
    let body = list.tail()?;
    let assignments = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;

    expand_let(&build_let_nest(assignments, body)?, env, state)
}

fn build_let_nest(assignments: &Expression, body: &Expression) -> Result<Expression> {
    if assignments.is_nil() {
        Ok(body.clone())
    } else {
        let (first, rest) = assignments.decons()?;
        if rest.is_nil() {
            Ok(scheme!(let, (@first.clone()), ...body.clone()))
        } else {
            Ok(scheme!(let, (@first.clone()), build_let_nest(rest, body)?))
        }
    }
}

pub fn expand_or(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    let (_, args) = list.decons()?;
    //assert_eq!(&scheme!(or), cmd);
    let mapped = args.map_list(|x| Ok(scheme!((@x.clone()))))?;
    let mapped = mapped.append(scheme!(((#t, #f))))?;
    expand_cond(&scheme!(cond, ...mapped), env, state)
}

pub fn expand_quasiquote(exps: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    expand(&structure_to_cons(exps.cdr()?.car()?)?, env, state)
}

fn structure_to_cons(exp: &Expression) -> Result<Expression> {
    Ok(match exp {
        Expression::Pair(p) => {
            if p.car == symbol::UNQUOTE {
                p.cdr.car()?.clone()
            } else {
                scheme!(cons, @structure_to_cons(&p.car)?, @structure_to_cons(&p.cdr)?)
            }
        }
        _ => scheme!(quote, @exp.clone()),
    })
}

pub fn expand_setvar(list: &Expression, env: &EnvRef, state: &State) -> Result<Expression> {
    //assert_eq!(&scheme!(symbol::SETVAR), list.car()?);
    Ok(cons(
        Expression::Special(symbol::SETVAR),
        expand(list.cdr().unwrap(), env, state)?,
    ))
}

/// super primitive implementation that does not attempt any search path and file extension magic.
pub fn find_file(path: impl AsRef<Path>, state: &State) -> Option<PathBuf> {
    let path = path.as_ref();
    let mut candidates = vec![];

    if path.is_relative() {
        candidates.extend(
            state
                .current_file
                .as_ref()
                .and_then(|file| file.parent())
                .map(|dir| dir.join(path)),
        );
    }

    candidates.push(path.to_path_buf());

    for candidate in candidates {
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

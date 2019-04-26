use crate::expression::{Expression, Ref};
use crate::symbol;
use crate::environment::EnvRef;
use crate::errors::*;
use crate::parser::parse_file;
use std::path::{Path, PathBuf};

// convert some syntactic forms, expand macros, check errors, ... (mostly to-do)
pub fn expand(expr: &Expression, env: &EnvRef) -> Result<Expression> {
    use Expression::*;
    match expr {
        Pair(ref car, _) => {
            let syntax_macro = if let Symbol(s) = &**car {
                match env.borrow().lookup(s) {
                    Some(Expression::Macro(m)) => Some(m),
                    _ => None,
                }
            } else {
                None
            };
            match **car {
                _ if syntax_macro.is_some() => syntax_macro.unwrap().expand(expr),
                Symbol(s) if s == symbol::AND => expand_and(expr, env),
                Symbol(s) if s == symbol::COND => expand_cond(expr, env),
                Symbol(s) if s == symbol::DEFINE => expand_define(expr, env),
                Symbol(s) if s == symbol::DEFINE_SYNTAX => Ok(expr.clone()),
                Symbol(s) if s == symbol::IF => expand_if(expr, env),
                Symbol(s) if s == symbol::INCLUDE => expand_include(expr, env),
                Symbol(s) if s == symbol::LAMBDA => expand_lambda(expr, env),
                Symbol(s) if s == symbol::LET => expand_let(expr, env),
                Symbol(s) if s == symbol::OR => expand_or(expr, env),
                Symbol(s) if s == symbol::QUOTE => Ok(expr.clone()),
                _ => expr.map_list(|e| expand(&e, env)),
            }
        }
        _ => Ok(expr.clone()),
    }
}

fn expand_define(list: &Expression, env: &EnvRef) -> Result<Expression> {
    assert_eq!(&scheme!(define), list.car()?);
    let (signature, body) = list.cdr()?.decons().map_err(|_| ErrorKind::ArgumentError)?;

    if signature.is_symbol() {
        if body.cdr()? != &Expression::Nil {
            return Err(ErrorKind::ArgumentError)?;
        }
        let value = body.car()?;
        Ok(scheme!(define, @signature.clone(), @expand(&value, env)?))
    } else if signature.is_pair() {
        let (name, signature) = signature.decons().map_err(|_| ErrorKind::ArgumentError)?;

        let lambda = scheme!(lambda, @signature.clone(), ...body.clone());
        let lambda = expand_lambda(&lambda, env)?;

        Ok(scheme!(define, @name.clone(), @lambda))
    } else {
        Err(ErrorKind::TypeError(format!("invalid signature: {:?}", signature)).into())
    }
}

fn expand_lambda(list: &Expression, env: &EnvRef) -> Result<Expression> {
    assert_eq!(&scheme!(lambda), list.car()?);
    let (signature, body) = list.cdr()?.decons().map_err(|_| ErrorKind::ArgumentError)?;

    if body.cdr().unwrap() == &Expression::Nil {
        Ok(scheme!(lambda, @signature.clone(), @expand(body.car()?, env)?))
    } else {
        let body = Expression::cons(scheme!(begin), body.map_list(|e| expand(&e, env))?);
        Ok(scheme!(lambda, @signature.clone(), @body))
    }
}

fn expand_cond(list: &Expression, env: &EnvRef) -> Result<Expression> {
    assert_eq!(&scheme!(cond), list.car()?);
    let body = list.cdr()?.map_list(|row| {
        let row = match row {
            Expression::Pair(car, cdr) => {
                let car = if let Expression::Symbol(s) = &**car {
                    if *s == symbol::ELSE {
                        Ref::new(Expression::True)
                    } else {
                        car.clone()
                    }
                } else {
                    car.clone()
                };
                Expression::Pair(car, cdr.clone())
            }
            row => row.clone(),
        };
        expand(&row, env)
    })?;
    Ok(Expression::cons(scheme!(cond), body))
}

fn expand_if(list: &Expression, env: &EnvRef) -> Result<Expression> {
    let mut list = list.iter_list();

    assert_eq!(Some(&scheme!(if)), list.next_expr()?);
    let cond = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
    let if_ = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
    let else_ = list.next_expr()?.unwrap_or(&Expression::Undefined);

    Ok(scheme!(if, @expand(cond, env)?, @expand(if_, env)?, @expand(else_, env)?))
}

fn expand_let(list: &Expression, env: &EnvRef) -> Result<Expression> {
    let mut list = list.iter_list();

    assert_eq!(Some(&scheme!(let)), list.next_expr()?);

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
    expand(&exps, env)
}

fn expand_or(list: &Expression, env: &EnvRef) -> Result<Expression> {
    let (cmd, args) = list.decons()?;
    assert_eq!(&scheme!(or), cmd);
    let mapped = args.map_list(|x| Ok(scheme!((@x.clone()))))?;
    let mapped = mapped.append(scheme!(((#t, #f))))?;
    expand_cond(&scheme!(cond, ...mapped), env)
}

fn expand_and(list: &Expression, env: &EnvRef) -> Result<Expression> {
    let (cmd, args) = list.decons()?;
    assert_eq!(&scheme!(and), cmd);

    match args {
        Expression::Nil => Ok(Expression::True),
        Expression::Pair(car, cdr) if **cdr == Expression::Nil => Ok((**car).clone()),
        Expression::Pair(car, cdr) => expand_if(
            &scheme!(if, @(**car).clone(), @expand_and(&scheme!(and, ...(**cdr).clone()), env)?, #f),
            env,
        ),
        _ => unreachable!(),
    }
}

fn expand_include(list: &Expression, env: &EnvRef) -> Result<Expression> {
    let mut list = list.iter_list();
    assert_eq!(Some(&scheme!(include)), list.next_expr()?);

    let mut result = scheme!((begin));

    for filename in list {
        let filename = filename?.try_as_str()?;
        let path =
            find_file(filename).ok_or_else(|| ErrorKind::FileNotFoundError(filename.to_owned()))?;
        let expr = parse_file(path)?;
        let expr = expand(&expr, env)?;
        result = result.append(expr)?;
    }

    Ok(result)
}

/// super primitive implementation that does not attempt any search path and file extension magic.
fn find_file(path: impl AsRef<Path>) -> Option<PathBuf> {
    let path = path.as_ref();
    if path.is_file() {
        Some(path.to_owned())
    } else {
        None
    }
}

use crate::environment::EnvRef;
use crate::errors::*;
use crate::expression::{Expression, Procedure};
use crate::symbol;
use std::borrow::Cow;

pub fn eval(expr: &Expression, mut env: EnvRef) -> Result<Expression> {
    use Expression::*;

    // We use Cow not for copy-on-write but for its ability
    // to represent optional ownership of a value.
    let mut expr = Cow::Borrowed(expr);

    loop {
        match *expr {
            Symbol(ref s) => {
                return env
                    .borrow()
                    .lookup(&s)
                    .ok_or_else(|| ErrorKind::Undefined(*s).into());
            }
            Undefined | Nil | Integer(_) | Float(_) | String(_) | Char(_) | True | False
            | Procedure(_) | Error(_) => {
                return Ok(expr.into_owned());
            }
            Native(_) => return Ok(expr.into_owned()),
            Pair(ref car, ref cdr) => {
                match **cdr {
                    Expression::Nil => {}
                    Expression::Pair(_, _) => {}
                    _ => return Ok(expr.into_owned()),
                }
                //let l = expr.try_into_list()?;
                match &**car {
                    Symbol(s) if *s == symbol::BEGIN => expr = Cow::Owned(begin(&cdr, &env)?),
                    Symbol(s) if *s == symbol::COND => match cond(&cdr, &env)? {
                        Return::RetVal(ex) => return Ok(ex),
                        Return::TailCall(ex) => expr = Cow::Owned(ex),
                    },
                    Symbol(s) if *s == symbol::DEFINE => return define(&cdr, &env),
                    Symbol(s) if *s == symbol::LAMBDA => return lambda(&cdr, &env),
                    Symbol(s) if *s == symbol::IF => {
                        expr = Cow::Owned(if_form(&cdr, env.clone())?.clone())
                    }
                    Symbol(s) if *s == symbol::EVAL => {
                        expr = Cow::Owned(eval(cdr.car()?, env.clone())?);
                    }
                    Symbol(s) if *s == symbol::QUOTE => {
                        return Ok(cdr.car()?.clone());
                    }
                    Symbol(s) if *s == symbol::SETVAR => return set_var(&cdr, &env),
                    car => {
                        let proc = eval(car, env.clone())?;
                        let args = (*cdr).map_list(|a| eval(a, env.clone()))?;
                        match proc {
                            Procedure(p) => {
                                let parent = env;
                                env = p.new_local_env(args)?;
                                expr = Cow::Owned(p.body_ex());
                                p.notify_call(&env, Some(&parent));
                            }
                            Native(func) => return func(args),
                            x => {
                                return Err(
                                    ErrorKind::TypeError(format!("not callable: {:?}", x)).into()
                                );
                            }
                        }
                    }
                }
            }
        };
    }
}

pub fn call(proc: Expression, args: Expression) -> Result<Expression> {
    match proc {
        Expression::Procedure(p) => {
            let env = p.new_local_env(args)?;
            p.notify_call(&env, None);
            eval(&p.body_ex(), env)
        }
        Expression::Native(func) => func(args),
        x => Err(ErrorKind::TypeError(format!("not callable: {:?}", x)).into()),
    }
}

fn begin(mut list: &Expression, env: &EnvRef) -> Result<Expression> {
    loop {
        match list.decons()? {
            (car, Expression::Nil) => {
                // we return the last element instead of evaluating it,
                // so that it can be tail-called
                return Ok(car.clone());
            }
            (car, cdr) => {
                eval(car, env.clone())?;
                list = cdr;
            }
        }
    }
}

fn define(list: &Expression, env: &EnvRef) -> Result<Expression> {
    let (name, list) = list.decons()?;
    let (val_ex, _) = list.decons()?;
    let value = eval(val_ex, env.clone())?;
    env.borrow_mut()
        .insert(name.try_as_symbol()?.clone(), value);
    Ok(Expression::Undefined)
}

fn set_var(list: &Expression, env: &EnvRef) -> Result<Expression> {
    let (name, list) = list.decons()?;
    let (val_ex, _) = list.decons()?;
    let value = eval(val_ex, env.clone())?;

    let symbol = name.try_as_symbol()?;

    env.borrow_mut()
        .set_value(symbol, value)
        .ok_or_else(|| ErrorKind::Undefined(*symbol))?;
    Ok(Expression::Undefined)
}

fn lambda(list: &Expression, env: &EnvRef) -> Result<Expression> {
    let (signature, list) = list.decons_rc()?;
    let (body, _) = list.decons_rc()?;
    let proc = Procedure::new(signature.clone(), body.clone(), env.clone());
    Ok(Expression::Procedure(proc))
}

enum Return {
    RetVal(Expression),
    TailCall(Expression),
}

fn cond(mut list: &Expression, env: &EnvRef) -> Result<Return> {
    while !list.is_nil() {
        let (row, cdr) = list.decons()?;
        list = &cdr;

        let (cond, cdr) = row.decons()?;
        let cond = eval(cond, env.clone())?;
        if cond.is_true() {
            if cdr.is_nil() {
                return Ok(Return::RetVal(cond));
            } else {
                return Ok(Return::TailCall(begin(cdr, env)?));
            }
        }
    }
    Ok(Return::RetVal(Expression::Undefined))
}

fn if_form(list: &Expression, env: EnvRef) -> Result<&Expression> {
    let (cond, list) = list.decons()?;
    let (then, list) = list.decons()?;
    let (otherwise, _) = list.decons()?;

    if eval(cond, env)?.is_true() {
        Ok(then)
    } else {
        Ok(otherwise)
    }
}

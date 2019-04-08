use crate::environment::EnvRef;
use crate::errors::*;
use crate::expression::{Expression, Procedure};

pub fn eval(mut expr: Expression, mut env: EnvRef) -> Result<Expression> {
    use Expression::*;
    loop {
        match expr {
            Symbol(s) => {
                return env
                    .borrow()
                    .lookup(&s)
                    .ok_or_else(|| ErrorKind::Undefined(s).into());
            }
            Undefined | Nil | Integer(_) | Float(_) | String(_) | True | False | Procedure(_)
            | Error(_) => {
                return Ok(expr);
            }
            Native(_) => return Ok(expr),
            Pair(car, cdr) => {
                match *cdr {
                    Expression::Nil => {}
                    Expression::Pair(_, _) => {}
                    _ => return Ok(Expression::Pair(car, cdr)),
                }
                //let l = expr.try_into_list()?;
                match &*car {
                    Symbol(ref s) if s == "begin" => expr = begin((*cdr).clone(), env.clone())?,
                    Symbol(ref s) if s == "cond" => match cond((*cdr).clone(), env.clone())? {
                        Return::RetVal(ex) => return Ok(ex),
                        Return::TailCall(ex) => expr = ex,
                    },
                    Symbol(ref s) if s == "define" => return define((*cdr).clone(), env.clone()),
                    Symbol(ref s) if s == "lambda" => return lambda((*cdr).clone(), &env),
                    Symbol(ref s) if s == "if" => expr = if_form((*cdr).clone(), env.clone())?,
                    car => {
                        let proc = eval(car.clone(), env.clone())?;
                        let args = (*cdr).map_list(|a| eval(a.clone(), env.clone()))?;
                        match proc {
                            Procedure(p) => {
                                expr = p.body_ex();
                                env = p.new_local_env(args)?;
                            }
                            Native(func) => return func(args),
                            _ => {
                                return Err(ErrorKind::TypeError("not callable".to_string()).into());
                            }
                        }
                    }
                }
            }
        }
    }
}

fn begin(mut list: Expression, env: EnvRef) -> Result<Expression> {
    loop {
        match list.decons()? {
            (car, Expression::Nil) => {
                // we return the last element instead of evaluating it,
                // so that it can be tail-called
                return Ok(car);
            }
            (car, cdr) => {
                eval(car, env.clone())?;
                list = cdr;
            }
        }
    }
}

fn define(mut list: Expression, env: EnvRef) -> Result<Expression> {
    let name = list.next()?.ok_or(ErrorKind::ArgumentError)?;
    let val_ex = list.next()?.ok_or(ErrorKind::ArgumentError)?;
    let value = eval(val_ex, env.clone())?;
    env.borrow_mut().insert(name.try_into_symbol()?, value);
    Ok(Expression::Undefined)
}

fn lambda(mut list: Expression, env: &EnvRef) -> Result<Expression> {
    let signature = list.next()?.ok_or(ErrorKind::ArgumentError)?;
    let body = list.next()?.ok_or(ErrorKind::ArgumentError)?;
    let proc = Procedure::build(signature, body, env)?;
    Ok(Expression::Procedure(proc))
}

enum Return {
    RetVal(Expression),
    TailCall(Expression),
}

fn cond(mut list: Expression, env: EnvRef) -> Result<Return> {
    while let Some(row) = list.next()? {
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

fn if_form(mut list: Expression, env: EnvRef) -> Result<Expression> {
    let cond = list.next()?.unwrap();
    let then = list.next()?.unwrap();
    let otherwise = list.next()?.unwrap();

    if eval(cond, env.clone())?.is_true() {
        Ok(then)
    } else {
        Ok(otherwise)
    }
}

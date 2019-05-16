use crate::environment::EnvRef;
use crate::errors::*;
use crate::expression::{Expression, Pair as PairType, Procedure, Ref};
use crate::libraries::{define_library, store_library};
use crate::macros;
use crate::symbol;
use std::borrow::Cow;

#[cfg(not(feature = "debugging"))]
mod debug_hooks {
    use super::*;
    pub fn enter_eval(expr: &Expression, env: &EnvRef) {}
    pub fn leave_eval(res: &Result<Expression>) {}
    pub fn predispatch(expr: &Expression, env: &EnvRef) {}
    pub fn function_call(proc: &Expression, args: &Expression, expr: &Expression) {}
}

#[cfg(feature = "debugging")]
mod debug_hooks {
    pub use crate::debugger::{enter_eval, function_call, leave_eval, predispatch};
}

pub fn eval(expr: &Expression, env: EnvRef) -> Result<Expression> {
    debug_hooks::enter_eval(expr, &env);
    let r = inner_eval(expr, env);
    debug_hooks::leave_eval(&r);
    r
}

enum Return {
    Value(Expression),
    TailCall(Expression, EnvRef),
}

pub fn inner_eval(expr: &Expression, mut env: EnvRef) -> Result<Expression> {
    use Expression::*;

    // We use Cow not for copy-on-write but for its ability
    // to represent optional ownership of a value.
    let mut expr = Cow::Borrowed(expr);

    loop {
        debug_hooks::predispatch(&expr, &env);
        let retval = match *expr {
            Symbol(ref s) => {
                Return::Value(env
                    .borrow()
                    .lookup(&s)
                    .ok_or_else(|| ErrorKind::Undefined(*s))?)
            }
            Undefined | Nil | Integer(_) | Float(_) | String(_) | Char(_) | True | False
            | Procedure(_) | Macro(_) /*| Error(_)*/ => {
                Return::Value(expr.into_owned())
            }
            Native(_) | NativeIntrusive(_) => Return::Value(expr.into_owned()),
            Pair(ref pair) => {
                let PairType{car, cdr, ..} = &**pair;
                match car {
                    Symbol(s) if *s == symbol::APPLY => {
                        let args = (*cdr).map_list(|a| eval(a, env.clone()))?;
                        let (proc, args) = prepare_apply(&args)?;
                        match proc {
                            Expression::Procedure(p) => {
                                let e = p.new_local_env(args)?;
                                let x = begin(p.body_ex(), &e)?;
                                Return::TailCall(x, e)
                            }
                            Expression::Native(func) => Return::Value(func(args)?),
                            x => Err(ErrorKind::TypeError(format!("not callable: {:?}", x)))?,
                        }
                    },
                    Symbol(s) if *s == symbol::BEGIN => Return::TailCall(begin(&cdr, &env)?, env),
                    Symbol(s) if *s == symbol::COND => Return::TailCall(cond(&cdr, &env)?, env),
                    Symbol(s) if *s == symbol::DEFINE => Return::Value(define(&cdr, &env)?),
                    Symbol(s) if *s == symbol::DEFINE_LIBRARY => {
                        let name = cdr.car()?;
                        let declarations = cdr.cdr()?;
                        let lib = define_library(declarations)?;
                        store_library(name, lib)?;
                        return Ok(Expression::Undefined);
                    }
                    Symbol(s) if *s == symbol::DEFINE_SYNTAX => {
                        let macro_ = macros::Macro::parse(cdr, &env)?;
                        env.borrow_mut()
                            .insert(macro_.name(), Expression::Macro(macro_));
                        return Ok(Expression::Undefined);
                    }
                    Symbol(s) if *s == symbol::LAMBDA => Return::Value(lambda(&cdr, &env)?),
                    Symbol(s) if *s == symbol::IF => Return::TailCall(if_form(&cdr, env.clone())?.clone(), env),
                    Symbol(s) if *s == symbol::EVAL => {
                        Return::TailCall(eval(cdr.car()?, env.clone())?, env)
                    }
                    Symbol(s) if *s == symbol::QUOTE => {
                        Return::Value(cdr.car()?.clone())
                    }
                    Symbol(s) if *s == symbol::SETVAR => return set_var(&cdr, &env),
                    car => {
                        let proc = eval(car, env.clone())?;
                        let args = (*cdr).map_list(|a| eval(a, env.clone()))?;
                        debug_hooks::function_call(&proc, &args, &expr);
                        match proc {
                            Procedure(p) => {
                                let e = p.new_local_env(args)?;
                                let x = begin(p.body_ex(), &e)?;
                                Return::TailCall(x, e)
                            }
                            Native(func) => Return::Value(func(args)?),
                            NativeIntrusive(func) => return func(args, &env),
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

        match retval {
            Return::Value(x) => return Ok(x),
            Return::TailCall(x, e) => {
                expr = Cow::Owned(x);
                env = e;
            }
        }
    }
}

/*pub fn is_special_form(expr: &Expression) -> bool {
    let s = match expr.car() {
        Ok(Expression::Symbol(s)) => *s,
        _ => return false,
    };

    s == symbol::BEGIN
        || s == symbol::COND
        || s == symbol::DEFINE
        || s == symbol::DEFINE_LIBRARY
        || s == symbol::DEFINE_SYNTAX
        || s == symbol::LAMBDA
        || s == symbol::IF
        || s == symbol::EVAL
        || s == symbol::IMPORT
        || s == symbol::QUOTE
        || s == symbol::SETVAR
        || s == symbol::TRACE
}*/

fn prepare_apply(list: &Expression) -> Result<(Expression, Expression)> {
    let (proc, mut in_cursor) = list.decons()?;

    let mut result = Expression::Nil;
    let mut out_cursor = &mut result;

    loop {
        match in_cursor {
            Expression::Nil => break,
            Expression::Pair(pair) => {
                let PairType { car, cdr, .. } = &**pair;
                in_cursor = &*cdr;

                if in_cursor.is_nil() {
                    *out_cursor = car.clone();
                    break;
                } else {
                    *out_cursor = Expression::cons(car.clone(), Expression::Nil);
                };

                out_cursor = out_cursor.cdr_mut().unwrap();
            }
            _ => return Err(ErrorKind::TypeError("not a list".into()))?,
        }
    }

    Ok((proc.clone(), result))
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
    let (signature, body) = list.decons()?;
    let proc = Procedure::new(
        Ref::new(signature.clone()),
        Ref::new(body.clone()),
        env.clone(),
    );
    Ok(Expression::Procedure(proc))
}

fn cond(mut list: &Expression, env: &EnvRef) -> Result<Expression> {
    while !list.is_nil() {
        let (row, cdr) = list.decons()?;
        list = &cdr;

        let (cond, cdr) = row.decons()?;
        let cond = eval(cond, env.clone())?;
        if cond.is_true() {
            if cdr.is_nil() {
                if cond.is_pair() {
                    return Ok(Expression::cons(
                        symbol::QUOTE,
                        Expression::cons(cond, Expression::Nil),
                    ));
                } else {
                    return Ok(cond);
                }
            } else {
                return Ok(begin(cdr, env)?);
            }
        }
    }
    Ok(Expression::Undefined)
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

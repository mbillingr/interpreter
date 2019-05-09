use crate::environment::EnvRef;
use crate::errors::*;
use crate::expression::{Expression, Pair as PairType, Procedure, Ref};
use crate::libraries::{define_library, import_library, store_library};
use crate::macros;
use crate::symbol;
use crate::tracer::{install_tracer, remove_tracer, CallGraph};
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

pub fn inner_eval(expr: &Expression, mut env: EnvRef) -> Result<Expression> {
    use Expression::*;

    // We use Cow not for copy-on-write but for its ability
    // to represent optional ownership of a value.
    let mut expr = Cow::Borrowed(expr);

    loop {
        debug_hooks::predispatch(&expr, &env);
        match *expr {
            Symbol(ref s) => {
                return env
                    .borrow()
                    .lookup(&s)
                    .ok_or_else(|| ErrorKind::Undefined(*s).into());
            }
            Undefined | Nil | Integer(_) | Float(_) | String(_) | Char(_) | True | False
            | Procedure(_) | Macro(_) /*| Error(_)*/ => {
                return Ok(expr.into_owned());
            }
            Native(_) | NativeIntrusive(_) => return Ok(expr.into_owned()),
            Pair(ref pair) => {
                let PairType{car, cdr, ..} = &**pair;
                match cdr {
                    Expression::Nil => {}
                    Expression::Pair(_) => {}
                    _ => return Ok(expr.into_owned()),
                }
                //let l = expr.try_into_list()?;
                match car {
                    Symbol(s) if *s == symbol::BEGIN => expr = Cow::Owned(begin(&cdr, &env)?),
                    Symbol(s) if *s == symbol::COND => match cond(&cdr, &env)? {
                        Return::RetVal(ex) => return Ok(ex),
                        Return::TailCall(ex) => expr = Cow::Owned(ex),
                    },
                    Symbol(s) if *s == symbol::DEFINE => return define(&cdr, &env),
                    Symbol(s) if *s == symbol::DEFINE_LIBRARY => {
                        let name = cdr.car()?;
                        let declarations = cdr.cdr()?;
                        let lib = define_library(declarations)?;
                        store_library(name, lib)?;
                        return Ok(Expression::Undefined);
                    }
                    Symbol(s) if *s == symbol::DEFINE_SYNTAX => {
                        let macro_ = macros::Macro::parse(cdr)?;
                        env.borrow_mut()
                            .insert(macro_.name(), Expression::Macro(macro_));
                        return Ok(Expression::Undefined);
                    }
                    Symbol(s) if *s == symbol::LAMBDA => return lambda(&cdr, &env),
                    Symbol(s) if *s == symbol::IF => {
                        expr = Cow::Owned(if_form(&cdr, env.clone())?.clone())
                    }
                    Symbol(s) if *s == symbol::EVAL => {
                        expr = Cow::Owned(eval(cdr.car()?, env.clone())?);
                    }
                    Symbol(s) if *s == symbol::IMPORT => {
                        import_library(cdr, &env)?;
                        return Ok(Expression::Undefined);
                    }
                    Symbol(s) if *s == symbol::QUOTE => {
                        return Ok(cdr.car()?.clone());
                    }
                    Symbol(s) if *s == symbol::SETVAR => return set_var(&cdr, &env),
                    Symbol(s) if *s == symbol::TRACE => {
                        let x = cdr.car()?;
                        let tracer_id = install_tracer(CallGraph::new());
                        let result = eval(x, env.clone());
                        let trace = remove_tracer(tracer_id);
                        let trace = trace.as_any().downcast_ref::<CallGraph>().unwrap();
                        println!("{:#?}", trace);
                        // todo: do something useful with the trace
                        expr = Cow::Owned(result?);
                    }
                    car => {
                        let proc = eval(car, env.clone())?;
                        let args = (*cdr).map_list(|a| eval(a, env.clone()))?;
                        debug_hooks::function_call(&proc, &args, &expr);
                        match proc {
                            Procedure(p) => {
                                let parent = env;
                                env = p.new_local_env(args)?;
                                expr = Cow::Owned(p.body_ex().clone());
                                p.notify_call(&env, &parent);
                            }
                            Native(func) => return func(args),
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
    }
}

pub fn is_special_form(expr: &Expression) -> bool {
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
}

pub fn call(proc: Expression, args: Expression, calling_env: &EnvRef) -> Result<Expression> {
    match proc {
        Expression::Procedure(p) => {
            let env = p.new_local_env(args)?;
            p.notify_call(&env, calling_env);
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
    let (signature, list) = list.decons()?;
    let (body, _) = list.decons()?;
    let proc = Procedure::new(
        Ref::new(signature.clone()),
        Ref::new(body.clone()),
        env.clone(),
    );
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

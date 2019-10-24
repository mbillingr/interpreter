use crate::environment::EnvRef;
use crate::errors::*;
use crate::expression::{Expression, Pair as PairType, Procedure, Ref};
use crate::libraries::{define_library, store_library};
use crate::symbol;
use crate::syntax;
use std::borrow::Cow;

#[cfg(not(feature = "debugging"))]
mod debug_hooks {
    use super::*;
    pub fn enter_eval(_expr: &Expression, _env: &EnvRef) {}
    pub fn leave_eval(_res: &Result<Expression>) {}
    pub fn predispatch(_expr: &Expression, _env: &EnvRef) {}
    pub fn function_call(_proc: &Expression, _args: &Expression, _expr: &Expression) {}
}

#[cfg(feature = "debugging")]
mod debug_hooks {
    pub use crate::debugger::{enter_eval, function_call, leave_eval, predispatch};
}

pub fn eval(expr: &Expression, env: EnvRef) -> Result<Expression> {
    debug_hooks::enter_eval(expr, &env);
    let r = inner_eval(expr, env);
    debug_hooks::leave_eval(&r);
    match r {
        Ok(x) => Ok(x),
        Err(e) => Err(e.with_context(expr.clone())),
    }
}

pub enum Return {
    Value(Expression),
    TailCall(Expression, EnvRef),
}

impl<T> From<T> for Return
where
    T: Into<Expression>,
{
    fn from(x: T) -> Self {
        Return::Value(x.into())
    }
}

pub fn inner_eval(expr: &Expression, mut env: EnvRef) -> Result<Expression> {
    use Expression::*;

    // We use Cow not for copy-on-write but for its ability
    // to represent optional ownership of a value.
    let mut expr = Cow::Borrowed(expr);

    loop {
        debug_hooks::predispatch(&expr, &env);
        let retval = match *expr {
            Symbol(ref s) => Return::Value(
                env.borrow()
                    .lookup(&s)
                    .ok_or_else(|| ErrorKind::Unbound(*s))?,
            ),
            Undefined | Nil | Integer(_) | Float(_) | String(_) | Char(_) | True | False
            | Procedure(_) | Macro(_) | Special(_) | Vector(_) | OpaqueVector(_) | File(_)
            | Class(_) | Instance(_) => Return::Value(expr.into_owned()),
            Native(_) | NativeIntrusive(_) | NativeClosure(_) => Return::Value(expr.into_owned()),
            NativeMacro(_) => Return::Value(expr.into_owned()),
            Pair(ref pair) => {
                let PairType { car, cdr, .. } = &**pair;
                match car {
                    Special(s) if *s == symbol::BEGIN => eval_sequence(&cdr, &env)?,
                    Special(s) if *s == symbol::COND => cond(&cdr, &env)?,
                    Special(s) if *s == symbol::DEFINE => define(&cdr, &env)?,
                    Special(s) if *s == symbol::DEFINE_LIBRARY => def_library(&cdr)?,
                    Special(s) if *s == symbol::IF => if_form(&cdr, env.clone())?,
                    Special(s) if *s == symbol::LAMBDA => lambda(&cdr, &env)?,
                    Special(s) if *s == symbol::SETVAR => set_var(&cdr, &env)?,
                    Special(s) if *s == symbol::QUOTE => quote(cdr)?,
                    car => {
                        let proc = eval(car, env.clone())?;
                        let args = (*cdr).map_list(|a| eval(a, env.clone()))?;
                        debug_hooks::function_call(&proc, &args, &expr);
                        apply(proc, args, &env)?
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

pub fn prepare_apply(list: &Expression) -> Result<(Expression, Expression)> {
    let (proc, mut in_cursor) = list.decons()?;

    let mut args = Expression::Nil;
    let mut out_cursor = &mut args;

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

    Ok((proc.clone(), args))
}

pub fn apply(proc: Expression, args: Expression, env: &EnvRef) -> Result<Return> {
    match inner_apply(proc.clone(), args.clone(), env) {
        Ok(r) => Ok(r),
        Err(e) => Err(e.with_context(Expression::cons(proc, args))),
    }
}

pub fn inner_apply(proc: Expression, args: Expression, env: &EnvRef) -> Result<Return> {
    match proc {
        Expression::Procedure(p) => {
            let e = p.new_local_env(args)?;
            eval_sequence(p.body_ex(), &e)
        }
        Expression::Native(func) => func(args),
        Expression::NativeIntrusive(func) => func(args, env),
        Expression::NativeClosure(func) => func(args),
        x => Err(ErrorKind::TypeError(format!("not callable: {:?}", x)))?,
    }
}

fn eval_sequence(mut list: &Expression, env: &EnvRef) -> Result<Return> {
    loop {
        match list.decons()? {
            (car, Expression::Nil) => return Ok(Return::TailCall(car.clone(), env.clone())),
            (car, cdr) => {
                eval(car, env.clone())?;
                list = cdr;
            }
        }
    }
}

fn define(list: &Expression, env: &EnvRef) -> Result<Return> {
    let (name, list) = list.decons()?;
    let (val_ex, _) = list.decons()?;
    let value = eval(val_ex, env.clone())?;
    env.borrow_mut()
        .insert(name.try_as_symbol()?.clone(), value);
    Ok(Return::Value(Expression::Undefined))
}

fn def_library(list: &Expression) -> Result<Return> {
    let name = list.car()?;
    let declarations = list.cdr()?;
    let lib = define_library(declarations, &syntax::State::default())?;
    store_library(name, lib)?;
    Ok(Return::Value(Expression::Undefined))
}

fn quote(list: &Expression) -> Result<Return> {
    list.car().map(Clone::clone).map(Return::Value)
}

fn set_var(list: &Expression, env: &EnvRef) -> Result<Return> {
    let (name, list) = list.decons()?;
    let (val_ex, _) = list.decons()?;
    let value = eval(val_ex, env.clone())?;

    let symbol = name.try_as_symbol()?;

    env.borrow_mut()
        .set_value(symbol, value)
        .ok_or_else(|| ErrorKind::Unbound(*symbol))?;
    Ok(Return::Value(Expression::Undefined))
}

fn lambda(list: &Expression, env: &EnvRef) -> Result<Return> {
    let (signature, body) = list.decons()?;
    let proc = Procedure::new(
        Ref::new(signature.clone()),
        Ref::new(body.clone()),
        env.clone(),
    );
    Ok(Return::Value(Expression::Procedure(proc)))
}

fn cond(mut list: &Expression, env: &EnvRef) -> Result<Return> {
    while !list.is_nil() {
        let (row, cdr) = list.decons()?;
        list = &cdr;

        let (cond, cdr) = row.decons()?;
        let cond = eval(cond, env.clone())?;
        if cond.is_true() {
            if cdr.is_nil() {
                return Ok(Return::Value(cond));
            } else if cdr.car()? == &symbol::COND_APPLY {
                let func = eval(cdr.cdr()?.car()?, env.clone())?;
                let call = conslist!(func, cond);
                return Ok(Return::TailCall(call, env.clone()));
            } else {
                return eval_sequence(cdr, env);
            }
        }
    }
    Ok(Return::Value(Expression::Undefined))
}

fn if_form(list: &Expression, env: EnvRef) -> Result<Return> {
    let (cond, list) = list.decons()?;
    let (then, list) = list.decons()?;
    let (otherwise, _) = list.decons()?;

    if eval(cond, env.clone())?.is_true() {
        Ok(Return::TailCall(then.clone(), env))
    } else {
        Ok(Return::TailCall(otherwise.clone(), env))
    }
}

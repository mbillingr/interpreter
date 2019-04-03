use crate::destructure::Destructure;
use crate::environment::EnvRef;
use crate::errors::*;
use crate::expression::{Expression, List, Procedure};

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
            Undefined | Nil | Integer(_) | Float(_) | String(_) | True | False | Procedure(_) => {
                return Ok(expr);
            }
            Native(_) => return Ok(expr),
            List(l) => match l.first() {
                None => return Ok(Nil),
                Some(Symbol(s)) if s == "begin" => expr = begin(l, env.clone())?,
                Some(Symbol(s)) if s == "cond" => match cond(l, env.clone())? {
                    Return::RetVal(ex) => return Ok(ex),
                    Return::TailCall(ex) => expr = ex,
                },
                Some(Symbol(s)) if s == "define" => return define(l, env.clone()),
                Some(Symbol(s)) if s == "lambda" => return lambda(l, &env),
                Some(Symbol(s)) if s == "if" => expr = if_form(l, env.clone())?,
                Some(_) => {
                    let mut items = l.into_iter();
                    let proc = eval(items.next().unwrap(), env.clone())?;
                    let args: Vec<_> = items
                        .map(|arg| eval(arg, env.clone()))
                        .collect::<Result<_>>()?;
                    match proc {
                        Procedure(p) => {
                            expr = p.body_ex();
                            env = p.new_local_env(args)?;
                        }
                        Native(func) => return func(args),
                        _ => return Err(ErrorKind::TypeError("not callable".to_string()).into()),
                    }
                }
            },
        }
    }
}

fn begin(mut list: List, env: EnvRef) -> Result<Expression> {
    if list.len() < 2 {
        return Err(ErrorKind::ArgumentError.into());
    }
    let last = list.pop().unwrap();
    for expr in list.into_iter().skip(1) {
        eval(expr, env.clone())?;
    }
    Ok(last)
}

fn define(mut list: List, env: EnvRef) -> Result<Expression> {
    assert_eq!(3, list.len());
    let body = list.pop().unwrap();
    let signature = list.pop().unwrap();

    match signature {
        Expression::Symbol(s) => {
            let value = eval(body, env.clone())?;
            env.borrow_mut().insert(s, value);
        }
        Expression::List(mut sig) => {
            let name = sig.remove(0).try_into_symbol()?;
            let proc = Procedure::build(Some(name), sig, body, &env)?;
            env.borrow_mut()
                .insert(proc.name.clone().unwrap(), Expression::Procedure(proc));
        }
        _ => {
            return Err(
                ErrorKind::TypeError(format!("Cannot use {} as signature.", signature)).into(),
            );
        }
    }

    Ok(Expression::Undefined)
}

fn lambda(list: List, env: &EnvRef) -> Result<Expression> {
    let (_, signature, body): (Expression, List, Expression) = list.destructure()?;
    let proc = Procedure::build(None, signature, body, env)?;
    Ok(Expression::Procedure(proc))
}

enum Return {
    RetVal(Expression),
    TailCall(Expression),
}

fn cond(list: List, env: EnvRef) -> Result<Return> {
    for pair in list.into_iter().skip(1) {
        let mut row = pair.try_into_list()?;
        let last = if row.len() > 1 { row.pop() } else { None };
        let mut row = row.into_iter();
        let cond = row.next().ok_or(ErrorKind::ArgumentError)?;
        let result = eval(cond, env.clone())?;
        if result.is_true() {
            if last.is_none() {
                return Ok(Return::RetVal(result));
            }
            for action in row {
                eval(action, env.clone())?;
            }
            return Ok(Return::TailCall(last.unwrap()));
        }
    }
    Ok(Return::RetVal(Expression::Undefined))
}

fn if_form(list: List, env: EnvRef) -> Result<Expression> {
    let mut list = list.into_iter().skip(1);

    let cond = list.next().unwrap();
    let then = list.next().unwrap();
    let otherwise = list.next().unwrap();

    if eval(cond, env.clone())?.is_true() {
        Ok(then)
    } else {
        Ok(otherwise)
    }
}

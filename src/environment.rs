use crate::errors::*;
use crate::expression::{Args, Expression, NativeFn, Procedure, WeakProcedure};
use crate::interpreter;
use crate::symbol::Symbol;
use rand::Rng;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::rc::{Rc, Weak};
use std::time::SystemTime;

pub type EnvRef = Rc<RefCell<Environment>>;
pub type EnvWeak = Weak<RefCell<Environment>>;

impl From<Environment> for EnvRef {
    fn from(env: Environment) -> Self {
        Rc::new(RefCell::new(env))
    }
}

#[derive(Debug)]
enum Entry {
    Value(Expression),
    Procedure(WeakProcedure),
}

#[derive(Debug)]
pub struct Environment {
    map: HashMap<Symbol, Entry>,
    parent: Option<EnvRef>,
}

/*impl Drop for Environment {
    fn drop(&mut self) {
        println!("Dropping env {}", self.id);
        NEXT_ID.with(|n| {
            n.set(n.get() - 1);
        });
    }
}*/

impl Environment {
    pub fn new(parent: Option<EnvRef>) -> Environment {
        Environment {
            map: Default::default(),
            parent,
        }
    }
    pub fn new_child(parent: EnvRef) -> Environment {
        Environment::new(Some(parent))
    }

    pub fn lookup(&self, key: &Symbol) -> Option<Expression> {
        let entry = self.map.get(key);
        match entry {
            None => self.parent.clone().and_then(|p| p.borrow().lookup(key)),
            Some(Entry::Value(expr)) => Some(expr.clone()),
            Some(Entry::Procedure(proc)) => Some(proc.clone().into()),
        }
    }

    pub fn insert<K: Into<Symbol>>(&mut self, key: K, expr: Expression) {
        // avoid Rc loops by storing functions that refer to the
        // environment they live in as weak references.
        let entry = match expr {
            Expression::Procedure(proc) => {
                if proc.env.as_ptr() == self {
                    Entry::Procedure(proc.into())
                } else {
                    Entry::Value(Expression::Procedure(proc))
                }
            }
            expr => Entry::Value(expr),
        };
        self.map.insert(key.into(), entry);
    }

    pub fn insert_native(&mut self, key: &str, func: NativeFn) {
        self.insert(key, Expression::Native(func));
    }

    pub fn set_vars(&mut self, names: Expression, args: Expression) -> Result<()> {
        let mut names = &names;
        let mut args = &args;
        loop {
            let arg;
            let name;
            match (names, args) {
                (Expression::Nil, Expression::Nil) => return Ok(()),
                (Expression::Symbol(s), a) => {
                    name = s;
                    arg = a;
                    names = &Expression::Nil;
                    args = &Expression::Nil;
                }
                (Expression::Pair(car, cdr), _) if car.is_named_symbol(".") => {
                    name = cdr.car()?.try_as_symbol()?;
                    names = cdr.cdr()?;
                    arg = args;
                    args = &Expression::Nil;
                }
                (Expression::Pair(_, _), Expression::Pair(_, _)) => {
                    name = names.car()?.try_as_symbol()?;
                    names = names.cdr()?;
                    arg = args.car()?;
                    args = args.cdr()?;
                }
                _ => return Err(ErrorKind::ArgumentError)?,
            }

            self.insert(name.clone(), arg.clone());
        }
    }

    pub fn all_keys(&self) -> impl Iterator<Item = Symbol> {
        let mut keys: Vec<_> = self.map.keys().cloned().collect();
        keys.extend(
            self.parent
                .iter()
                .flat_map(|parent| parent.borrow().all_keys()),
        );
        keys.into_iter()
    }
}

fn car(x: &Expression) -> Result<&Expression> {
    x.car().map_err(|_| ErrorKind::ArgumentError.into())
}

pub fn default_env() -> EnvRef {
    use Expression as X;

    let defenv: EnvRef = Environment::new(None).into();

    {
        let mut env = defenv.borrow_mut();

        // interpreter functions

        env.insert_native("apply", apply);

        // simple i/o

        env.insert_native("display", native_display);
        env.insert_native("error", |args| Ok(X::Error(Rc::new(args))));

        // pair operations

        env.insert_native("pair?", |args| Ok(car(&args)?.is_pair().into()));
        env.insert_native("cons", |args| {
            let (car, args) = args.decons_rc().map_err(|_| ErrorKind::ArgumentError)?;
            let (cdr, _) = args.decons_rc().map_err(|_| ErrorKind::ArgumentError)?;
            Ok(Expression::cons_rc(car.clone(), cdr.clone()))
        });
        env.insert_native("car", |args| Ok(car(&args)?.car()?.clone()));
        env.insert_native("cdr", |args| Ok(car(&args)?.cdr()?.clone()));

        // list operations

        env.insert_native("list", |args| Ok(args));
        env.insert_native("null?", |args| Ok(car(&args)?.is_nil().into()));

        // numerical operations

        env.insert_native("number?", |args| Ok(car(&args)?.is_number().into()));
        env.insert_native("+", |args| native_fold(args, X::zero(), X::add));
        env.insert_native("*", |args| native_fold(args, X::one(), X::mul));
        env.insert_native("-", |args| native_unifold(args, X::zero(), X::sub));
        env.insert_native("/", |args| native_unifold(args, X::one(), X::div));
        env.insert_native("remainder", |args| native_unifold(args, X::one(), X::rem));
        env.insert_native("min", |args| native_fold2(args, X::min));
        env.insert_native("max", |args| native_fold2(args, X::max));

        // logical operations

        env.insert_native("and", |args| native_fold(args, X::True, X::logical_and));
        env.insert_native("or", |args| native_fold(args, X::False, X::logical_or));
        env.insert(
            "not",
            X::Native(|args| {
                let x = car(&args)?;
                Ok((!x.is_true()).into())
            }),
        );

        // comparison

        env.insert_native("=", |args| native_compare(args, <X as PartialEq>::eq));
        env.insert_native("<", |args| native_compare(args, <X as PartialOrd>::lt));
        env.insert_native(">", |args| native_compare(args, <X as PartialOrd>::gt));

        // advanced math stuff

        env.insert_native("log", |args| {
            let x = car(&args)?;
            Ok(x.try_as_float()?.ln().into())
        });

        env.insert_native("floor", |args| {
            let x = car(&args)?;
            Ok(x.try_as_float()?.floor().into())
        });

        // misc

        env.insert_native("runtime", |_| {
            let t = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_micros();
            Ok(Expression::Integer(t as i64))
        });

        env.insert_native("random", |args| {
            let n = car(&args)?.try_as_integer()?;
            let r = rand::thread_rng().gen_range(0, n);
            Ok(Expression::Integer(r))
        });

        env.insert(
            "newline",
            X::Procedure(Procedure::build(X::Nil, scheme!((display, "\n")), &defenv).unwrap()),
        );
    }

    defenv
}

fn native_display(args: Args) -> Result<Expression> {
    print!("{}", car(&args)?);
    Ok(Expression::Undefined)
}

/// apply a bivariate function to all arguments in sequence
fn native_fold<F: Fn(Expression, Expression) -> Result<Expression>>(
    args: Args,
    mut acc: Expression,
    func: F,
) -> Result<Expression> {
    for b in args.iter_list()? {
        acc = func(acc, b?.clone())?;
    }
    Ok(acc)
}

/// apply a bivariate function to all arguments in sequence, initializing the
/// accumulator with the first element.
fn native_fold2<F: Fn(Expression, Expression) -> Result<Expression>>(
    args: Args,
    func: F,
) -> Result<Expression> {
    let (acc, tail) = args.decons()?;
    let mut acc = acc.clone();
    for b in tail.iter_list()? {
        acc = func(acc, b?.clone())?;
    }
    Ok(acc)
}

/// apply a bivariate comparison function to all arguments in sequence
fn native_compare<F: Fn(&Expression, &Expression) -> bool>(
    args: Args,
    pred: F,
) -> Result<Expression> {
    let mut args = args.iter_list()?;

    let mut a = match args.next_expr()? {
        None => return Ok(Expression::True),
        Some(x) => x,
    };

    for b in args {
        let b = b?;
        if pred(&a, &b) {
            a = b
        } else {
            return Ok(Expression::False);
        }
    }

    Ok(Expression::True)
}

/// apply a bivariate function to all arguments in sequence, but handle a single argument as
/// special case. For example: (- 5 2) -> 3  but (- 5) -> -5
fn native_unifold<F: Fn(Expression, Expression) -> Result<Expression>>(
    args: Args,
    mut acc: Expression,
    func: F,
) -> Result<Expression> {
    let mut args = args.iter_list()?;

    let first = args.next_expr()?.ok_or(ErrorKind::ArgumentError)?.clone();

    match args.next_expr()? {
        None => return func(acc, first),
        Some(second) => acc = func(first, second.clone())?,
    }

    for b in args {
        acc = func(acc, b?.clone())?;
    }
    Ok(acc)
}

fn apply(list: Expression) -> Result<Expression> {
    let mut result = Expression::Nil;
    let mut in_cursor = &list;
    let mut out_cursor = &mut result;

    loop {
        match in_cursor {
            Expression::Nil => break,
            Expression::Pair(car, cdr) => {
                //let x = interpreter::eval(&car, env.clone())?;
                let x = (**car).clone();
                in_cursor = &*cdr;

                if in_cursor.is_nil() {
                    *out_cursor = x;
                    break;
                } else {
                    *out_cursor = Expression::cons(x, Expression::Nil);
                };

                out_cursor = out_cursor.cdr_mut().unwrap();
            }
            _ => return Err(ErrorKind::TypeError("not a list".into()))?,
        }
    }

    let (proc, args) = result.decons()?;
    interpreter::call(proc.clone(), args.clone())
}

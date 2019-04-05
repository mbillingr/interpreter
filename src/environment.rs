use crate::errors::*;
use crate::expression::{Args, Expression, Procedure, Symbol, WeakProcedure};
use rand::Rng;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::rc::{Rc, Weak};
use std::time::SystemTime;
use crate::destructure::Destructure;

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

    pub fn lookup(&self, key: &str) -> Option<Expression> {
        let entry = self.map.get(key);
        match entry {
            None => self.parent.clone().and_then(|p| p.borrow().lookup(key)),
            Some(Entry::Value(expr)) => Some(expr.clone()),
            Some(Entry::Procedure(proc)) => Some(proc.clone().into()),
        }
    }

    pub fn insert<K: Into<String>>(&mut self, key: K, expr: Expression) {
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

    pub fn set_vars(&mut self, names: &[Expression], args: Vec<Expression>) -> Result<()> {
        if names.len() < args.len() {
            return Err(ErrorKind::ArgumentError.into());
        }

        for (n, a) in names.iter().zip(args) {
            self.insert(n.try_as_symbol()?.clone(), a);
        }

        Ok(())
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

pub fn default_env() -> EnvRef {
    use Expression as X;

    let defenv: EnvRef = Environment::new(None).into();

    {
        let mut env = defenv.borrow_mut();

        // simple i/o

        env.insert("display", X::Native(native_display));

        // pair operations

        env.insert("cons", X::Native(|args|{
            let (car, cdr): (Expression, Expression) = args.destructure()?;
            Ok(Expression::Pair(Box::new((car, cdr))))
        }));

        env.insert("car", X::Native(|args|{
            let pair: Expression = args.destructure()?;
            Ok(pair.try_into_pair()?.0)
        }));

        env.insert("cdr", X::Native(|args|{
            let pair: Expression = args.destructure()?;
            Ok(pair.try_into_pair()?.1)
        }));

        // numerical operations

        env.insert(
            "+".to_string(),
            X::Native(|args| native_fold(args, X::zero(), X::add)),
        );
        env.insert(
            "*".to_string(),
            X::Native(|args| native_fold(args, X::one(), X::mul)),
        );
        env.insert(
            "-".to_string(),
            X::Native(|args| native_unifold(args, X::zero(), X::sub)),
        );
        env.insert(
            "/".to_string(),
            X::Native(|args| native_unifold(args, X::one(), X::div)),
        );
        env.insert(
            "remainder".to_string(),
            X::Native(|args| native_unifold(args, X::one(), X::rem)),
        );

        // logical operations
        //    todo: would it make sense to implement these as special forms to take advantage of short-circuting?

        env.insert(
            "and".to_string(),
            X::Native(|args| native_fold(args, X::True, X::logical_and)),
        );
        env.insert(
            "or".to_string(),
            X::Native(|args| native_fold(args, X::False, X::logical_or)),
        );
        env.insert(
            "not".to_string(),
            X::Native(|args| {
                if args.len() != 1 {
                    Err(ErrorKind::ArgumentError.into())
                } else {
                    Ok((!args[0].is_true()).into())
                }
            }),
        );

        // comparison

        env.insert(
            "=".to_string(),
            X::Native(|args| native_compare(args, X::eq)),
        );
        env.insert(
            "<".to_string(),
            X::Native(|args| native_compare(args, X::lt)),
        );
        env.insert(
            ">".to_string(),
            X::Native(|args| native_compare(args, X::gt)),
        );

        // advanced math stuff

        env.insert("log", X::Native(|args| {
            let x: Expression = args.destructure()?;
            Ok(x.try_as_float()?.ln().into())
        }));

        // misc

        env.insert(
            "runtime".to_string(),
            X::Native(|_| {
                let t = SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap()
                    .as_micros();
                Ok(Expression::Integer(t as i64))
            }),
        );

        env.insert(
            "random".to_string(),
            X::Native(|args| {
                let n = args
                    .into_iter()
                    .next()
                    .ok_or(ErrorKind::ArgumentError)?
                    .try_as_integer()?;
                let r = rand::thread_rng().gen_range(0, n);
                Ok(Expression::Integer(r))
            }),
        );

        env.insert(
            "newline".to_string(),
            X::Procedure(
                Procedure::build(
                    vec![],
                    scheme!((display, "\n")),
                    &defenv,
                )
                .unwrap(),
            ),
        );
    }

    defenv
}

fn native_display(args: Args) -> Result<Expression> {
    print!(
        "{}",
        args.into_iter().next().ok_or(ErrorKind::ArgumentError)?
    );
    Ok(Expression::Undefined)
}

/// apply a bivariate function to all arguments in sequence
fn native_fold<F: Fn(Expression, Expression) -> Result<Expression>>(
    args: Args,
    mut acc: Expression,
    func: F,
) -> Result<Expression> {
    for b in args {
        acc = func(acc, b)?;
    }
    Ok(acc)
}

/// apply a bivariate function to all arguments in sequence
fn native_compare<F: Fn(&Expression, &Expression) -> bool>(
    args: Args,
    pred: F,
) -> Result<Expression> {
    let mut args = args.into_iter();

    let mut a = match args.next() {
        None => return Ok(Expression::True),
        Some(x) => x,
    };

    for b in args {
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
    let mut args = args.into_iter();

    let first = args.next().ok_or(ErrorKind::ArgumentError)?;

    match args.next() {
        None => return func(acc, first),
        Some(second) => acc = func(first, second)?,
    }

    for b in args {
        acc = func(acc, b)?;
    }
    Ok(acc)
}

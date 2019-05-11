pub use crate::envref::{EnvRef, EnvWeak};
use crate::errors::*;
use crate::expression::{Args, Expression, NativeFn, Pair, Procedure};
use crate::interpreter;
use crate::symbol::{self, Symbol};
use rand::Rng;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Sub};
use std::time::SystemTime;

#[derive(Debug, Clone)]
pub enum Entry {
    Value(Expression),
    Procedure(Procedure<EnvWeak>),
}

pub struct Environment {
    map: HashMap<Symbol, Entry>,
    parent: Option<EnvRef>,
    current_procedure: Procedure<EnvWeak>,
}

impl Environment {
    pub fn new(parent: Option<EnvRef>) -> Environment {
        Environment {
            map: Default::default(),
            parent,
            current_procedure: Default::default(),
        }
    }

    pub fn new_child<T: Into<Procedure<EnvWeak>>>(parent: EnvRef, proc: T) -> Environment {
        Environment {
            map: Default::default(),
            parent: Some(parent),
            current_procedure: proc.into(),
        }
    }

    /*pub fn parent(&self) -> Option<&EnvRef> {
        self.parent.as_ref()
    }*/

    pub fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    pub fn current_procedure(&self) -> &Procedure<EnvWeak> {
        &self.current_procedure
    }

    pub fn name(&self) -> Symbol {
        self.current_procedure.name()
    }

    pub fn lookup(&self, key: &Symbol) -> Option<Expression> {
        let entry = self.map.get(key);
        match entry {
            None => self.parent.as_ref().and_then(|p| p.borrow().lookup(key)),
            Some(Entry::Value(expr)) => Some(expr.clone()),
            Some(Entry::Procedure(proc)) => Some(proc.clone().into()),
        }
    }

    pub fn set_value(&mut self, key: &Symbol, value: Expression) -> Option<()> {
        if self.map.contains_key(key) {
            *self.map.get_mut(key).unwrap() = self.expr_to_entry(*key, value);
            Some(())
        } else {
            self.parent
                .as_mut()
                .and_then(|p| p.borrow_mut().set_value(key, value))
        }
    }

    pub fn insert<K: Into<Symbol>>(&mut self, key: K, expr: Expression) {
        let key = key.into();
        self.map.insert(key, self.expr_to_entry(key, expr));
    }

    pub fn insert_entry<K: Into<Symbol>>(&mut self, key: K, entry: Entry) {
        self.map.insert(key.into(), entry);
    }

    fn expr_to_entry(&self, key: Symbol, expr: Expression) -> Entry {
        // avoid Rc loops by storing functions that refer to the
        // environment they live in as weak references.
        match expr {
            Expression::Procedure(proc) => {
                if proc.env().as_ptr() as *const _ == self {
                    Entry::Procedure(proc.rename(key).into())
                } else {
                    Entry::Value(Expression::Procedure(proc))
                }
            }
            expr => Entry::Value(expr),
        }
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
                (Expression::Pair(pair), _) if pair.car.is_named_symbol(symbol::DOT) => {
                    name = pair.cdr.car()?.try_as_symbol()?;
                    names = pair.cdr.cdr()?;
                    arg = args;
                    args = &Expression::Nil;
                }
                (Expression::Pair(_), Expression::Pair(_)) => {
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

    pub fn items(&self) -> impl Iterator<Item = (&Symbol, &Entry)> {
        /*let mut keys: Vec<_> = self.map.keys().cloned().collect();
        keys.extend(
            self.parent
                .iter()
                .flat_map(|parent| parent.borrow().all_keys()),
        );
        keys.into_iter()*/
        self.map.iter()
    }

    pub fn get_scope(&self) -> Vec<Symbol> {
        let mut result = vec![];

        let mut parent = self.parent.clone();
        while let Some(env) = parent {
            parent = env.borrow().parent.clone();

            if env.borrow().is_root() {
                break;
            }

            result.push(env.borrow().current_procedure().name());
        }

        result
    }

    pub fn export(&self, names: &HashMap<Symbol, Symbol>) -> Result<Environment> {
        Ok(Environment {
            map: names
                .iter()
                .map(|(name, export_name)| {
                    self.map
                        .get(name)
                        .cloned()
                        .map(|entry| (*export_name, entry))
                        .ok_or_else(|| ErrorKind::UndefinedExport(*name).into())
                })
                .collect::<Result<_>>()?,
            parent: self.parent.clone(),
            current_procedure: self.current_procedure.clone(),
        })
    }

    pub fn print(&self) -> usize {
        if let Some(parent) = &self.parent {
            let depth = parent.borrow().print() + 4;
            let indent: String = (0..depth).map(|_| ' ').collect();
            let header = format!("----------<{}>----------", self.name());
            println!("{}+{}", indent, header);
            for (k, v) in &self.map {
                println!(
                    "{}| {}: {}",
                    indent,
                    k,
                    match v {
                        Entry::Procedure(p) => p.name().to_string(),
                        Entry::Value(v) => v.short_repr(),
                    }
                );
            }
            let footer: String = (0..header.len()).map(|_| '-').collect();
            println!("{}+{}", indent, footer);
            depth
        } else {
            println!("<global>");
            0
        }
    }
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Environment {}: {{", self.name())?;
        for (i, (k, v)) in self.map.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            match v {
                Entry::Value(x) => write!(f, "{}: {}", k, x)?,
                Entry::Procedure(p) => write!(f, "{}: {:?}", k, p)?,
            }
        }
        write!(f, "}}")
    }
}

fn car(x: &Expression) -> Result<&Expression> {
    x.car().map_err(|_| ErrorKind::ArgumentError.into())
}

fn cdr(x: &Expression) -> Result<&Expression> {
    x.cdr().map_err(|_| ErrorKind::ArgumentError.into())
}

pub fn default_env() -> EnvRef {
    use Expression as X;

    let defenv: EnvRef = Environment::new(None).into();

    {
        let mut env = defenv.borrow_mut();

        // place holders

        env.insert(symbol::LAMBDA, symbol::LAMBDA.into());

        // interpreter functions

        env.insert("apply", Expression::NativeIntrusive(apply));

        env.insert_native("eq?", |args| native_binary(args, Expression::eqv));
        env.insert_native("eqv?", |args| native_binary(args, Expression::eqv));
        env.insert_native("equal?", |args| native_binary(args, Expression::equal));

        // types

        env.insert_native("symbol?", |args| Ok(car(&args)?.is_symbol().into()));

        // simple i/o

        env.insert_native("display", native_display);
        // todo: find a way to put Expressions into Errors (Problem: Errors must be Send but expressions are not)
        env.insert_native("error", |args| {
            Err(ErrorKind::GenericError(format!("{}", args)))?
        });

        // pair operations

        env.insert_native("pair?", |args| Ok(car(&args)?.is_pair().into()));
        env.insert_native("cons", |args| {
            let (car, args) = args.decons().map_err(|_| ErrorKind::ArgumentError)?;
            let (cdr, _) = args.decons().map_err(|_| ErrorKind::ArgumentError)?;
            Ok(Expression::cons(car.clone(), cdr.clone()))
        });
        env.insert_native("car", |args| Ok(car(&args)?.car()?.clone()));
        env.insert_native("cdr", |args| Ok(car(&args)?.cdr()?.clone()));

        env.insert_native("set-car!", |args| {
            let pair = car(&args)?;
            let x = car(cdr(&args)?)?;
            pair.set_car(x.clone())?;
            Ok(Expression::Undefined)
        });

        env.insert_native("set-cdr!", |args| {
            let pair = car(&args)?;
            let x = car(cdr(&args)?)?;
            pair.set_cdr(x.clone())?;
            Ok(Expression::Undefined)
        });

        // list operations

        env.insert_native("list", Ok);
        env.insert_native("null?", |args| Ok(car(&args)?.is_nil().into()));

        // numerical operations

        env.insert_native("number?", |args| Ok(car(&args)?.is_number().into()));
        env.insert_native("exact?", |args| Ok(car(&args)?.is_exact().into()));
        env.insert_native("integer?", |args| Ok(car(&args)?.is_integer().into()));
        env.insert_native("+", |args| native_fold(args, X::zero(), X::add));
        env.insert_native("*", |args| native_fold(args, X::one(), X::mul));
        env.insert_native("-", |args| native_unifold(args, X::zero(), X::sub));
        env.insert_native("/", |args| native_unifold(args, X::one(), X::div));
        env.insert_native("min", |args| native_fold2(args, X::min));
        env.insert_native("max", |args| native_fold2(args, X::max));
        env.insert_native("round", |args| car(&args)?.round());
        env.insert_native("remainder", |args| {
            native_binary(args, X::truncate_remainder)
        });
        env.insert_native("quotient", |args| native_binary(args, X::truncate_quotient));

        // logical operations

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

        env.insert_native("floor", |args| {
            let x = car(&args)?;
            Ok(x.try_as_float()?.floor().into())
        });

        env.insert_native("log", |args| {
            let x = car(&args)?;
            Ok(x.try_as_float()?.ln().into())
        });

        env.insert_native("sin", |args| {
            let x = car(&args)?;
            Ok(x.try_as_float()?.sin().into())
        });

        env.insert_native("cos", |args| {
            let x = car(&args)?;
            Ok(x.try_as_float()?.cos().into())
        });

        env.insert_native("atan", |args| {
            let x = car(&args)?.try_as_float()?;
            let y = cdr(&args)?;
            if y.is_nil() {
                Ok(x.atan().into())
            } else {
                Ok(x.atan2(car(y)?.try_as_float()?).into())
            }
        });

        // misc

        env.insert_native("runtime", |_| {
            let t = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_micros();
            Ok(Expression::Integer(t as i64))
        });

        env.insert_native("random", |args| match car(&args)? {
            Expression::Integer(n) => Ok(Expression::Integer(rand::thread_rng().gen_range(0, n))),
            Expression::Float(n) => Ok(Expression::Float(rand::thread_rng().gen_range(0.0, n))),
            x => {
                Err(ErrorKind::TypeError(format!("Invalid upper limit for random: {:?}", x)).into())
            }
        });

        env.insert(
            "newline",
            X::Procedure(Procedure::build(X::Nil, scheme!(((display, "\n"))), &defenv).unwrap()),
        );

        env.insert(
            "print-env",
            Expression::NativeIntrusive(|_, env| {
                env.borrow().print();
                Ok(Expression::Undefined)
            }),
        );
    }

    defenv
}

fn native_binary<R, F>(args: Args, op: F) -> Result<Expression>
where
    R: IntoResultExpression,
    F: Fn(&Expression, &Expression) -> R,
{
    let (a, args) = args.decons().map_err(|_| ErrorKind::ArgumentError)?;
    let (b, _) = args.decons().map_err(|_| ErrorKind::ArgumentError)?;
    op(a, b).into_result()
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
    for b in args.iter_list() {
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
    for b in tail.iter_list() {
        acc = func(acc, b?.clone())?;
    }
    Ok(acc)
}

/// apply a bivariate comparison function to all arguments in sequence
fn native_compare<F: Fn(&Expression, &Expression) -> bool>(
    args: Args,
    pred: F,
) -> Result<Expression> {
    let mut args = args.iter_list();

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
    let mut args = args.iter_list();

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

fn apply(list: Expression, env: &EnvRef) -> Result<Expression> {
    let mut result = Expression::Nil;
    let mut in_cursor = &list;
    let mut out_cursor = &mut result;

    loop {
        match in_cursor {
            Expression::Nil => break,
            Expression::Pair(pair) => {
                let Pair { car, cdr, .. } = &**pair;
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

    let (proc, args) = result.decons()?;
    interpreter::call(proc.clone(), args.clone(), env)
}

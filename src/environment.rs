#[cfg(feature = "debugging")]
use crate::debugger::Debugger;
#[cfg(feature = "debugging")]
use crate::debugger_imgui_frontend;
pub use crate::envref::{EnvRef, EnvWeak};
use crate::errors::*;
use crate::expression::{
    define_class, define_method, Args, Class, Expression, Instance, NativeFn, Procedure, Ref,
};
use crate::interpreter::{apply, prepare_apply, Return};
use crate::io::{LineReader, ReplInput};
use crate::lexer::Lexer;
use crate::native_closure::NativeClosure;
use crate::number::Number;
use crate::parser::{parse_file, read_lex};
use crate::symbol::{self, Symbol};
use crate::syntax::{
    car_to_special, expand, expand_and, expand_begin, expand_case, expand_cond, expand_define,
    expand_if, expand_include, expand_lambda, expand_let, expand_lets, expand_or,
    expand_quasiquote, expand_setvar,
};
use num_traits::{FromPrimitive, ToPrimitive, Zero};
use std::collections::HashMap;
use std::convert::TryInto;
use std::fs::File;
use std::io::Write;
use std::ops::{Add, Div, Mul, Sub};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::SystemTime;

static SYMBOL_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone)]
pub enum Entry {
    Value(Expression),
    Procedure(Procedure<EnvWeak>),
}

impl Entry {
    pub fn short_repr(&self) -> String {
        match self {
            Entry::Value(expr) => expr.short_repr(),
            Entry::Procedure(proc) => Expression::Procedure(proc.clone().into()).short_repr(),
        }
    }
}

pub struct Environment {
    map: HashMap<Symbol, Entry>,
    parent: Option<EnvRef>,
    current_procedure: Procedure<EnvWeak>,
    name: Option<String>,
}

impl Environment {
    pub fn new(parent: Option<EnvRef>) -> Environment {
        Environment {
            map: Default::default(),
            parent,
            current_procedure: Default::default(),
            name: None,
        }
    }

    pub fn new_child<T: Into<Procedure<EnvWeak>>>(parent: EnvRef, proc: T) -> Environment {
        Environment {
            map: Default::default(),
            parent: Some(parent),
            current_procedure: proc.into(),
            name: None,
        }
    }

    pub fn parent(&self) -> Option<&EnvRef> {
        self.parent.as_ref()
    }

    pub fn current_procedure(&self) -> &Procedure<EnvWeak> {
        &self.current_procedure
    }

    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    pub fn name(&self) -> &str {
        self.name
            .as_ref()
            .map(String::as_str)
            .unwrap_or(self.current_procedure.name().name())
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
            name: self.name.clone(),
        })
    }

    pub fn print(&self) -> usize {
        let depth = if let Some(parent) = &self.parent {
            parent.borrow().print() + 4
        } else {
            0
        };
        let indent: String = (0..depth).map(|_| ' ').collect();
        let fb = self.formatted_bindings();
        let rowlen = fb.iter().map(String::len).max().unwrap_or(0);
        println!("{}+-{:-<rl$}-+", indent, self.name(), rl = rowlen);
        for row in fb {
            println!("{}| {} |", indent, row);
        }
        println!("{}+-{:-<rl$}-+", indent, "", rl = rowlen);
        depth
    }

    pub fn print_bindings(&self, name: &str) {
        if name != self.name() {
            match &self.parent {
                Some(p) => p.borrow().print_bindings(name),
                None => println!("Unknown environment: {}", name),
            }
        } else {
            for row in self.formatted_bindings() {
                println!("{}", row);
            }
        }
    }

    pub fn formatted_bindings(&self) -> Vec<String> {
        let mut identifiers: Vec<_> = self.map.keys().copied().collect();
        identifiers.sort();

        let items: Vec<_> = identifiers
            .into_iter()
            .map(|i| match &self.map[&i] {
                Entry::Value(x) if x.is_procedure() => (
                    format!("({} . args)", i),
                    x.type_description().to_string(),
                    String::new(),
                ),
                Entry::Value(x) => (
                    i.name().to_owned(),
                    x.type_description().to_string(),
                    format!("{:?}", x),
                ),
                Entry::Procedure(p) => (
                    format!("{:?}", Expression::cons(i, p.params_ex().clone())),
                    "Procedure".to_string(),
                    String::new(),
                ),
            })
            .collect();

        let left_width = items
            .iter()
            .map(|(left, _, _)| left)
            .map(String::len)
            .max()
            .unwrap_or(0);
        let middle_width = items
            .iter()
            .map(|(_, middle, _)| middle)
            .map(String::len)
            .max()
            .unwrap_or(0);
        let right_width = items
            .iter()
            .map(|(_, _, right)| right)
            .map(String::len)
            .max()
            .unwrap_or(0);

        items
            .into_iter()
            .map(|(left, middle, right)| {
                format!(
                    "{:l$} | {:m$} | {:r$}",
                    left,
                    middle,
                    right,
                    l = left_width,
                    m = middle_width,
                    r = right_width
                )
            })
            .collect()
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

        // macros

        env.insert("and", Expression::NativeMacro(expand_and));
        env.insert("begin", Expression::NativeMacro(expand_begin));
        env.insert("case", Expression::NativeMacro(expand_case));
        env.insert("cond", Expression::NativeMacro(expand_cond));
        env.insert("define", Expression::NativeMacro(expand_define));
        env.insert(
            symbol::DEFINE_LIBRARY.name(),
            Expression::NativeMacro(|expr, _, _| car_to_special(expr, symbol::DEFINE_LIBRARY)),
        );
        env.insert(
            symbol::DEFINE_SYNTAX.name(),
            Expression::NativeMacro(|expr, _, _| car_to_special(expr, symbol::DEFINE_SYNTAX)),
        );
        env.insert("if", Expression::NativeMacro(expand_if));
        env.insert("include", Expression::NativeMacro(expand_include));
        env.insert("lambda", Expression::NativeMacro(expand_lambda));
        env.insert("let", Expression::NativeMacro(expand_let));
        env.insert("let*", Expression::NativeMacro(expand_lets));
        env.insert("or", Expression::NativeMacro(expand_or));
        env.insert(
            symbol::QUOTE.name(),
            Expression::NativeMacro(|expr, _, _| car_to_special(expr, symbol::QUOTE)),
        );
        env.insert(
            symbol::QUASIQUOTE.name(),
            Expression::NativeMacro(expand_quasiquote),
        );
        env.insert("set!", Expression::NativeMacro(expand_setvar));

        // interpreter functions

        env.insert(
            "apply",
            Expression::NativeIntrusive(|args, env| {
                let (op, args) = prepare_apply(&args)?;
                apply(op, args, env)
            }),
        );

        env.insert(
            "eval",
            Expression::NativeIntrusive(|expr, env| {
                Ok(Return::TailCall(expr.car()?.clone(), env.clone()))
            }),
        );

        env.insert_native("read", |_| {
            let mut input = ReplInput::new("");
            let mut source = String::new();
            loop {
                source.push_str(&input.read_line()?);

                let mut lexer = Lexer::new();
                lexer.tokenize(&source)?;

                if lexer.is_balanced() {
                    return read_lex(&mut lexer).map(Into::into);
                }
            }
        });

        env.insert_native("file-read", |args| {
            let (filename,): (&str,) = destructure!(args => auto)?;
            parse_file(&filename).map(Into::into)
        });

        env.insert_native("eq?", |args| native_binary(args, Expression::eqv));
        env.insert_native("eqv?", |args| native_binary(args, Expression::eqv));
        env.insert_native("equal?", |args| native_binary(args, Expression::equal));

        env.insert_native("gensym", |args| {
            let base = args
                .car()
                .map(|x| format!("{}", x))
                .unwrap_or("sym".to_string());
            let name = format!("{}{}", base, SYMBOL_COUNTER.fetch_add(1, Ordering::Relaxed));
            Ok(Symbol::new_uninterned(name).into())
        });

        // object system

        env.insert(
            "Object",
            Expression::Class(Ref::new(Class::new(
                symbol::Symbol::new("Object"),
                None,
                vec![],
            ))),
        );

        // Form: (define-class class superclass (fields...))
        env.insert(
            "define-class",
            Expression::NativeMacro(|expr, env, _| {
                let (_, tail) = expr.decons()?;
                let (name, tail) = tail.decons()?;
                let (base, tail) = tail.decons()?;
                let (fields, _) = tail.decons()?;

                let mut env = env.borrow_mut();

                let name = name.try_as_symbol()?;

                let base = env
                    .lookup(base.try_as_symbol()?)
                    .and_then(|x| x.try_as_class().cloned())
                    .ok_or_else(|| ErrorKind::TypeError(format!("base must be a class")))?;

                let cls = Ref::new(Class::new(
                    *name,
                    Some(base.clone()),
                    fields
                        .iter_list()
                        .map(|x| Ok(*x?.try_as_symbol()?))
                        .collect::<Result<_>>()?,
                ));

                define_class(&mut env, cls);

                Ok(Expression::Undefined.into())
            }),
        );

        // Form: (define-generic (function (obj) params...) body...)
        env.insert(
            "define-generic",
            Expression::NativeMacro(|expr, env, state| {
                let (_, definition) = expr.decons()?;
                let (name_and_params, body) = definition.decons()?;
                let (name, params) = name_and_params.decons()?;
                let (obj, params) = params.decons()?;
                let obj = obj.car()?;

                let params = Expression::cons(obj.clone(), params.clone());

                let name = name.try_as_symbol()?.clone();

                let environment = env.clone();

                env.borrow_mut().insert(
                    name.clone(),
                    Expression::NativeClosure(Ref::new(NativeClosure::new(
                        vec![Box::new(name), Box::new(environment)],
                        |args, vars| {
                            let name = *vars[0].downcast_ref::<Symbol>().unwrap();
                            let env = vars[1].downcast_ref::<EnvRef>().unwrap().clone();
                            let obj = args.car()?;
                            let obj = obj
                                .try_as_instance()
                                .ok_or_else(|| ErrorKind::TypeError(format!("not an object")))?;
                            obj.invoke_method(name, args.clone(), env)
                        },
                    ))),
                );

                let class = env
                    .borrow()
                    .lookup(&Symbol::new("Object"))
                    .and_then(|x| x.try_as_class().cloned())
                    .ok_or_else(|| {
                        ErrorKind::TypeError(format!("Object is not a class in the current scope"))
                    })?;

                let body = expand(body, env, state)?;
                define_method(class, name, params, body, env.clone());

                Ok(Expression::Undefined.into())
            }),
        );

        // Form: (define-method (function (obj class) params...) body...)
        env.insert(
            "define-method",
            Expression::NativeMacro(|expr, env, state| {
                let (_, tail) = expr.decons()?;
                let (name_and_params, body) = tail.decons()?;
                let (name, params) = name_and_params.decons()?;
                let (obj_and_class, params) = params.decons()?;
                let obj = obj_and_class.car()?;
                let class = obj_and_class.cdr()?.car()?;

                let params = Expression::cons(obj.clone(), params.clone());

                let name = name.try_as_symbol()?.clone();

                let class = env
                    .borrow()
                    .lookup(class.try_as_symbol()?)
                    .and_then(|x| x.try_as_class().cloned())
                    .ok_or_else(|| {
                        ErrorKind::TypeError(format!("method must be defined on a class"))
                    })?;

                let body = expand(body, env, state)?;
                define_method(class, name, params, body, env.clone());

                Ok(Expression::Undefined.into())
            }),
        );

        env.insert_native("class-fields", |args| {
            let (cls,): (Expression,) = destructure!(args => auto)?;

            if let Some(c) = cls.try_as_class() {
                let list: Expression = c
                    .all_field_names()
                    .iter()
                    .map(|name| name.clone().into())
                    .collect();
                Ok(list.into())
            } else {
                Err(ErrorKind::TypeError(format!("not a class")).into())
            }
        });

        env.insert_native("object->class", |args| {
            let (obj,): (Expression,) = destructure!(args => auto)?;

            if let Some(o) = obj.try_as_instance() {
                Ok(Expression::Class(o.base().clone()).into())
            } else {
                Err(ErrorKind::TypeError(format!("not an object")).into())
            }
        });

        env.insert_native("field-value", |args| {
            let (obj, (field,)): (Expression, (Expression,)) = destructure!(args => auto, auto)?;

            if let Some(o) = obj.try_as_instance() {
                o.get_field_value(field.try_as_symbol()?)
                    .cloned()
                    .map(|value| value.into())
                    .ok_or_else(|| ErrorKind::ArgumentError.into())
            } else {
                Err(ErrorKind::TypeError(format!("not an object")).into())
            }
        });

        env.insert_native("set-field-value!", |args| {
            let (obj, (value, (field,))): (Expression, (Expression, (Expression,))) =
                destructure!(args => auto, auto, auto)?;

            if let Some(o) = obj.try_as_instance() {
                let o = unsafe { &mut *(&**o as *const Instance as *mut Instance) };
                o.set_field_value(field.try_as_symbol()?, value)
                    .ok_or_else(|| ErrorKind::ArgumentError)?;
                Ok(Expression::Undefined.into())
            } else {
                Err(ErrorKind::TypeError(format!("not an object")).into())
            }
        });

        // files

        env.insert_native("file-open", |args| {
            let (name, (flag,)): (Expression, (Expression,)) = destructure!(args => auto, auto)?;

            let name = name.try_as_str()?;
            let file = match flag {
                Expression::Symbol(s) if s.name() == "r" => File::open(name)?,
                Expression::Symbol(s) if s.name() == "w" => File::create(name)?,
                _ => Err(ErrorKind::GenericError(format!(
                    "Unknown file mode: {}",
                    flag
                )))?,
            };

            Ok(Expression::File(Ref::new(Some(file))).into())
        });

        env.insert_native("file?", |args| {
            let (f,): (Expression,) = destructure!(args => auto)?;
            if let Expression::File(_) = f {
                Ok(true.into())
            } else {
                Ok(false.into())
            }
        });

        env.insert_native("file-close!", |args| {
            let (f,): (Expression,) = destructure!(args => auto)?;
            if let Expression::File(inner) = f {
                let handler: &mut Option<File> = unsafe { &mut *(&*inner as *const _ as *mut _) };
                *handler = None;
            }
            Ok(Expression::Undefined.into())
        });

        env.insert_native("fdisplay", |args| {
            let (f, (obj,)): (Expression, (Expression,)) = destructure!(args => auto, auto)?;
            if let Expression::File(fref) = f {
                if let Some(fh) = &*fref {
                    let fh: &mut File = unsafe { &mut *(fh as *const _ as *mut _) };
                    write!(fh, "{}", obj)?;
                    return Ok(Expression::Undefined.into());
                }
            }
            Err(ErrorKind::GenericError(format!("can only write to open files")).into())
        });

        env.insert_native("fwrite", |args| {
            let (f, (obj,)): (Expression, (Expression,)) = destructure!(args => auto, auto)?;
            if let Expression::File(fref) = f {
                if let Some(fh) = &*fref {
                    let fh: &mut File = unsafe { &mut *(fh as *const _ as *mut _) };
                    write!(fh, "{}", obj.short_repr())?;
                    return Ok(Expression::Undefined.into());
                }
            }
            Err(ErrorKind::GenericError(format!("can only write to open files")).into())
        });

        // types

        env.insert_native("boolean?", |args| {
            car(&args).map(Expression::is_bool).map(Into::into)
        });
        env.insert_native("char?", |args| {
            car(&args).map(Expression::is_char).map(Into::into)
        });
        env.insert_native("complex?", |args| {
            car(&args)
                .and_then(Expression::try_as_number)
                .map(Number::is_complex)
                .map(Into::into)
        });
        env.insert_native("integer?", |args| {
            car(&args)
                .and_then(Expression::try_as_number)
                .map(Number::is_integer)
                .map(Into::into)
        });
        env.insert_native("null?", |args| {
            car(&args).map(Expression::is_nil).map(Into::into)
        });
        env.insert_native("number?", |args| {
            car(&args).map(Expression::is_number).map(Into::into)
        });
        env.insert_native("pair?", |args| {
            car(&args).map(Expression::is_pair).map(Into::into)
        });
        env.insert_native("procedure?", |args| {
            car(&args).map(Expression::is_procedure).map(Into::into)
        });
        env.insert_native("real?", |args| {
            car(&args)
                .and_then(Expression::try_as_number)
                .map(Number::is_real)
                .map(Into::into)
        });
        env.insert_native("rational?", |args| {
            car(&args)
                .and_then(Expression::try_as_number)
                .map(Number::is_rational)
                .map(Into::into)
        });
        env.insert_native("string?", |args| {
            car(&args).map(Expression::is_string).map(Into::into)
        });
        env.insert_native("symbol?", |args| {
            car(&args).map(Expression::is_symbol).map(Into::into)
        });
        env.insert_native("vector?", |args| {
            car(&args).map(Expression::is_vector).map(Into::into)
        });

        // simple i/o

        env.insert_native("display", native_display);
        // todo: find a way to put Expressions into Errors (Problem: Errors must be Send but expressions are not)
        env.insert_native("error", |args| {
            Err(ErrorKind::GenericError(format!("{}", args)))?
        });

        // pair operations

        env.insert_native("cons", |args| {
            let (car, args) = args.decons().map_err(|_| ErrorKind::ArgumentError)?;
            let (cdr, _) = args.decons().map_err(|_| ErrorKind::ArgumentError)?;
            Ok(Return::Value(Expression::cons(car.clone(), cdr.clone())))
        });
        env.insert_native("car", |args| Ok(car(&args)?.car()?.clone().into()));
        env.insert_native("cdr", |args| Ok(car(&args)?.cdr()?.clone().into()));

        env.insert_native("set-car!", |args| {
            let pair = car(&args)?;
            let x = car(cdr(&args)?)?;
            pair.set_car(x.clone())?;
            Ok(Return::Value(Expression::Undefined))
        });

        env.insert_native("set-cdr!", |args| {
            let pair = car(&args)?;
            let x = car(cdr(&args)?)?;
            pair.set_cdr(x.clone())?;
            Ok(Return::Value(Expression::Undefined))
        });

        // list operations

        env.insert_native("list", |args| Ok(Return::Value(args)));

        // numerical constructors

        env.insert_native("string->number", |args| {
            let s = car(&args)?.try_as_str()?;
            Ok(s.parse::<Number>()
                .map(Into::into)
                .unwrap_or(Expression::False)
                .into())
        });

        /// deprecated since 0.2.2
        env.insert_native("complex", |args| {
            eprintln!("Warning: (complex re im) is deprecated and will be removed in a future release. Use (make-rectangular re im) instead.");
            let re = car(&args)?.try_as_number()?.to_f64().unwrap();
            let im = car(cdr(&args)?)?.try_as_number()?.to_f64().unwrap();
            Ok(Number::complex(re, im).into())
        });

        env.insert_native("make-rectangular", |args| {
            let re = car(&args)?.try_as_number()?.to_f64().unwrap();
            let im = car(cdr(&args)?)?.try_as_number()?.to_f64().unwrap();
            Ok(Number::complex(re, im).into())
        });

        env.insert_native("make-polar", |args| {
            let radius = car(&args)?.try_as_number()?.to_f64().unwrap();
            let angle = car(cdr(&args)?)?.try_as_number()?.to_f64().unwrap();
            let re = radius * angle.cos();
            let im = radius * angle.sin();
            Ok(Number::complex(re, im).into())
        });

        // numerical operations

        env.insert_native("exact?", |args| Ok(car(&args)?.is_exact().into()));
        env.insert_native("+", |args| native_fold(args, X::zero(), X::add));
        env.insert_native("*", |args| native_fold(args, X::one(), X::mul));
        env.insert_native("-", |args| native_unifold(args, X::zero(), X::sub));
        env.insert_native("/", |args| native_unifold(args, X::one(), X::div));
        env.insert_native("min", |args| native_fold2(args, X::min));
        env.insert_native("max", |args| native_fold2(args, X::max));
        env.insert_native("round", |args| car(&args)?.round().map(Return::Value));
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

        // string library

        env.insert_native("symbol->string", |args| {
            let (s,): (Symbol,) = destructure!(args => auto)?;
            Ok(s.name().into())
        });

        env.insert_native("number->string", |args| {
            let s = match args.car()? {
                Expression::Number(n) => format!("{}", n),
                x => Err(ErrorKind::TypeError(format!("Expected number: {:?}", x)))?,
            };
            Ok(s.into())
        });

        env.insert_native("string->symbol", |args| {
            let (s,): (&str,) = destructure!(args => auto)?;
            Ok(Symbol::new(s).into())
        });

        env.insert_native("string-length", |args| {
            let (s,): (&str,) = destructure!(args => auto)?;
            Ok((s.len() as i64).into())
        });

        env.insert_native("string=?", |args| {
            let (a, (b,)): (&str, (&str,)) = destructure!(args => auto, auto)?;
            Ok((a == b).into())
        });

        env.insert_native("substring", |args| {
            let (string, (start, (end,))): (&str, (usize, (usize,))) =
                destructure!(args => auto, auto, auto)?;
            Ok(string[start..end].into())
        });

        env.insert_native("string-ref", |args| {
            let (string, (idx,)): (&str, (usize,)) = destructure!(args => auto, auto)?;
            let ch: char = string
                .chars()
                .skip(idx)
                .next()
                .ok_or_else(|| ErrorKind::ArgumentError)?;
            Ok(Expression::Char(ch).into())
        });

        env.insert_native("string-append", |args| {
            let mut result = String::new();
            for s in args.iter_list() {
                result += s?.try_into()?;
            }
            Ok(result.into())
        });

        env.insert_native("string->list", |args| {
            let mut list = Expression::Nil;
            let (string,): (&str,) = destructure!(args => auto)?;
            for ch in string.chars().rev() {
                list = Expression::cons(Expression::Char(ch), list);
            }
            Ok(list.into())
        });

        env.insert_native("list->string", |args| {
            let mut string = String::new();
            let (list,): (Expression,) = destructure!(args => auto)?;
            for x in list.iter_list() {
                match x {
                    Ok(Expression::Char(ch)) => string.push(*ch),
                    _ => Err(ErrorKind::TypeError(format!("Expected list of chars")))?,
                }
            }
            Ok(string.into())
        });

        // vectors

        env.insert_native("make-vector", |args| {
            let (n,): (usize,) = destructure!(args => auto)?;
            Ok(Expression::vector(n).into())
        });

        env.insert_native("make-opaque-vector", |args| {
            let (n,): (usize,) = destructure!(args => auto)?;
            Ok(Expression::opaque_vector(n).into())
        });

        env.insert_native("vector-ref", |args| {
            let (v, (i,)): (Expression, (usize,)) = destructure!(args => auto, auto)?;

            if let Some(vs) = v.try_as_vector() {
                if i >= vs.len() {
                    Err(ErrorKind::GenericError(format!(
                        "Index out of bounds {} >= {}",
                        i,
                        vs.len()
                    ))
                    .into())
                } else {
                    Ok(vs[i].clone().into())
                }
            } else {
                Err(ErrorKind::TypeError(format!("not a vector {:?}", v)).into())
            }
        });

        env.insert_native("vector-set!", |args| {
            let (v, (i, (x,))): (Expression, (usize, (Expression,))) =
                destructure!(args => auto, auto, auto)?;

            if let Some(vs) = v.try_as_vector_mut() {
                if i >= vs.len() {
                    Err(ErrorKind::GenericError(format!(
                        "Index out of bounds {} >= {}",
                        i,
                        vs.len()
                    ))
                    .into())
                } else {
                    vs[i] = x;
                    Ok(Expression::Undefined.into())
                }
            } else {
                Err(ErrorKind::TypeError(format!("not a vector {:?}", v)).into())
            }
        });

        env.insert_native("vector-length", |args| {
            let (v,): (Expression,) = destructure!(args => auto)?;

            if let Some(vs) = v.try_as_vector() {
                Ok((vs.len() as i64).into())
            } else {
                Err(ErrorKind::TypeError(format!("not a vector {:?}", v)).into())
            }
        });

        env.insert_native("vector-copy!", |args| {
            let (dst, (dstart, (src, (sstart, (len,))))): (
                Expression,
                (usize, (Expression, (usize, (usize,)))),
            ) = destructure!(args => auto, auto, auto, auto, auto)?;

            let dst = if let Some(v) = dst.try_as_vector_mut() {
                v
            } else {
                return Err(ErrorKind::TypeError(format!("not a vector {:?}", dst)).into());
            };

            let src = if let Some(v) = src.try_as_vector_mut() {
                v
            } else {
                return Err(ErrorKind::TypeError(format!("not a vector {:?}", src)).into());
            };

            if src.len() < len + sstart {
                return Err(ErrorKind::GenericError(format!("Out of bounds")).into());
            }

            if dst.len() - dstart < len - sstart {
                return Err(
                    ErrorKind::GenericError(format!("destination vector too short")).into(),
                );
            }

            for (d, s) in dst[dstart..].iter_mut().zip(&src[sstart..sstart + len]) {
                *d = s.clone();
            }

            Ok(Expression::Undefined.into())
        });

        // misc

        env.insert_native("runtime", |_| {
            let t = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_micros();
            Ok(Expression::from_u128(t)
                .expect("System time out of representable range")
                .into())
        });

        env.insert_native("random", |args| match car(&args)? {
            Expression::Number(n) => {
                Ok(Expression::Number(Number::rand_range(&Number::zero(), n)).into())
            }
            x => {
                Err(ErrorKind::TypeError(format!("Invalid upper limit for random: {:?}", x)).into())
            }
        });

        env.insert(
            "newline",
            X::Procedure(Procedure::build(X::Nil, scheme!(((display, "\n"))), &defenv).unwrap()),
        );

        env.insert_native("format", |args| {
            let (destination, args) = args.decons()?;
            let (control_string, args) = args.decons()?;

            let control_string = control_string.try_as_str()?;
            let mut args = args.clone();

            let mut output = String::new();

            let mut input = control_string.chars().peekable();

            let next_arg = |args: &mut Expression| {
                let (car, cdr) = args.decons().unwrap();
                let car = car.clone();
                *args = cdr.clone();
                car
            };

            let parse_parameter = |input: &mut std::iter::Peekable<std::str::Chars>,
                                   args: &mut Expression| {
                match input.peek() {
                    Some('v') => {
                        input.next();
                        next_arg(args)
                            .try_as_integer()
                            .map(|i| i.to_usize().unwrap())
                            .ok()
                        //.map(|x| x.to_u32_digits())
                        //.filter(|(sign, _)| *sign == Sign::Plus)
                        //.map(|(_, digits)| digits[0])
                    }
                    Some(ch) if ch.is_ascii_digit() => {
                        let mut i: usize = 0;
                        while input.peek().unwrap_or(&'x').is_ascii_digit() {
                            i = i * 10 + input.next().unwrap().to_digit(10).unwrap() as usize;
                        }
                        Some(i)
                    }
                    _ => None,
                }
            };

            let parse_directive = |input: &mut std::iter::Peekable<std::str::Chars>,
                                   output: &mut String,
                                   args: &mut Expression| {
                let param = parse_parameter(input, args);
                let mut left_pad = false;
                while let Some(ch) = input.next() {
                    match ch {
                        '@' => left_pad = true,
                        '~' => {
                            for _ in 0..param.unwrap_or(1) {
                                output.push('~')
                            }
                            return;
                        }
                        '%' => {
                            output.push('\n');
                            return;
                        }
                        'A' | 'a' | 'S' | 's' => {
                            let x = match ch {
                                'A' | 'a' => format!("{}", next_arg(args)),
                                'S' | 's' => format!("{:?}", next_arg(args)),
                                _ => unreachable!(),
                            };
                            if let Some(width) = param {
                                let width = width as usize;
                                if x.len() < width {
                                    let pad = " ".repeat(width - x.len());
                                    if left_pad {
                                        output.push_str(&pad);
                                        output.push_str(&x);
                                    } else {
                                        output.push_str(&x);
                                        output.push_str(&pad);
                                    }
                                } else {
                                    output.push_str(&x);
                                }
                            } else {
                                output.push_str(&x);
                            }
                            return;
                        }
                        _ => unimplemented!(),
                    }
                }
            };

            let parse = |input: &mut std::iter::Peekable<std::str::Chars>,
                         output: &mut String,
                         args: &mut Expression| loop {
                match input.next() {
                    Some('~') => parse_directive(input, output, args),
                    Some(ch) => output.push(ch),
                    None => break,
                }
            };

            parse(&mut input, &mut output, &mut args);

            match destination {
                Expression::True => print!("{}", output),
                Expression::False => return Ok(Expression::from(output).into()),
                _ => return Err(ErrorKind::GenericError(format!("Invalid destination")).into()),
            }

            Ok(Expression::Undefined.into())
        });

        env.insert(
            "print-env",
            Expression::NativeIntrusive(|_, env| {
                env.borrow().print();
                Ok(Return::Value(Expression::Undefined))
            }),
        );

        env.insert(
            "print-bindings",
            Expression::NativeIntrusive(|args, env| {
                let (name,): (Symbol,) = destructure!(args => auto)?;
                env.borrow().print_bindings(name.name());
                Ok(Return::Value(Expression::Undefined))
            }),
        );

        env.insert_native("body", |args| match car(&args)? {
            Expression::Procedure(p) => Ok(p.body_ex().clone().into()),
            a => Err(ErrorKind::TypeError(format!("not a procedure: {:?}", a)))?,
        });

        #[cfg(feature = "debugging")]
        env.insert(
            "debug",
            Expression::NativeIntrusive(|args, env| {
                let debugger = Debugger::new(car(&args)?.clone(), env.clone());
                debugger_imgui_frontend::run(debugger);
                Ok(Return::Value(Expression::Undefined))
            }),
        );

        env.insert_native("help", |_| {
            println!("{}", include_str!("../HELP.md"));
            Ok(Return::Value(Expression::Undefined))
        });

        env.insert_native("exit", |args| {
            use std::process::exit;
            use std::thread::sleep;
            use std::time::Duration;

            if args.is_nil() {
                println!("Bye.");
                sleep(Duration::from_millis(1500));
                exit(0)
            }

            let code = match args.car() {
                Ok(Expression::False) => -1,
                Ok(Expression::Number(n)) => n.to_i32().unwrap_or(0),
                _ => 0,
            };

            exit(code)
        })
    }

    defenv
}

fn native_binary<R, F>(args: Args, op: F) -> Result<Return>
where
    R: IntoResultExpression,
    F: Fn(&Expression, &Expression) -> R,
{
    let (a, args) = args.decons().map_err(|_| ErrorKind::ArgumentError)?;
    let (b, _) = args.decons().map_err(|_| ErrorKind::ArgumentError)?;
    op(a, b).into_result().map(Return::Value)
}

fn native_display(args: Args) -> Result<Return> {
    print!("{}", car(&args)?);
    Ok(Return::Value(Expression::Undefined))
}

/// apply a bivariate function to all arguments in sequence
fn native_fold<F: Fn(Expression, Expression) -> Result<Expression>>(
    args: Args,
    mut acc: Expression,
    func: F,
) -> Result<Return> {
    for b in args.iter_list() {
        acc = func(acc, b?.clone())?;
    }
    Ok(Return::Value(acc))
}

/// apply a bivariate function to all arguments in sequence, initializing the
/// accumulator with the first element.
fn native_fold2<F: Fn(Expression, Expression) -> Result<Expression>>(
    args: Args,
    func: F,
) -> Result<Return> {
    let (acc, tail) = args.decons()?;
    let mut acc = acc.clone();
    for b in tail.iter_list() {
        acc = func(acc, b?.clone())?;
    }
    Ok(Return::Value(acc))
}

/// apply a bivariate comparison function to all arguments in sequence
fn native_compare<F: Fn(&Expression, &Expression) -> bool>(args: Args, pred: F) -> Result<Return> {
    let mut args = args.iter_list();

    let mut a = match args.next_expr()? {
        None => return Ok(Return::Value(Expression::True)),
        Some(x) => x,
    };

    for b in args {
        let b = b?;
        if pred(&a, &b) {
            a = b
        } else {
            return Ok(Return::Value(Expression::False));
        }
    }

    Ok(Return::Value(Expression::True))
}

/// apply a bivariate function to all arguments in sequence, but handle a single argument as
/// special case. For example: (- 5 2) -> 3  but (- 5) -> -5
fn native_unifold<F: Fn(Expression, Expression) -> Result<Expression>>(
    args: Args,
    mut acc: Expression,
    func: F,
) -> Result<Return> {
    let mut args = args.iter_list();

    let first = args.next_expr()?.ok_or(ErrorKind::ArgumentError)?.clone();

    match args.next_expr()? {
        None => return func(acc, first).map(Return::Value),
        Some(second) => acc = func(first, second.clone())?,
    }

    for b in args {
        acc = func(acc, b?.clone())?;
    }

    Ok(Return::Value(acc))
}

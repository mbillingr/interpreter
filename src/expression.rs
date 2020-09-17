use crate::environment::{EnvRef, EnvWeak, Environment};
use crate::errors::*;
pub use crate::integer::Int;
use crate::interpreter::{apply, Return};
use crate::macros::Macro;
use crate::native_closure::NativeClosure;
use crate::sourcecode::SourceView;
use crate::symbol::{self, Symbol};
use crate::syntax;
use std::fs::File;
use std::hash::{Hash, Hasher};

#[cfg(feature = "thread-safe")]
pub use std::sync::{Arc as Ref, Weak};

use crate::number::Number;
use num_traits::{FromPrimitive, ToPrimitive};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::FromIterator;
#[cfg(not(feature = "thread-safe"))]
pub use std::rc::{Rc as Ref, Weak};

pub type Args = Expression;
pub type NativeFn = fn(Args) -> Result<Return>;
pub type NativeIntrusiveFn = fn(Args, &EnvRef) -> Result<Return>;
pub type MacroFn = fn(&Expression, &EnvRef, &syntax::State) -> Result<Expression>;

#[derive(Debug)]
pub struct Pair {
    pub car: Expression,
    pub cdr: Expression,

    #[cfg(feature = "source-tracking")]
    pub src: Option<SourceView>,
}

impl Pair {
    pub fn new(car: Expression, cdr: Expression) -> Ref<Self> {
        Ref::new(Pair {
            car,
            cdr,
            #[cfg(feature = "source-tracking")]
            src: None,
        })
    }
}

#[cfg(not(feature = "source-tracking"))]
impl Pair {
    pub fn new_sourced(car: Expression, cdr: Expression, _: Option<SourceView>) -> Ref<Self> {
        Pair::new(car, cdr)
    }

    pub fn get_source(&self) -> Option<SourceView> {
        None
    }
    pub fn set_source(&mut self, _src: Option<SourceView>) {}
}

#[cfg(feature = "source-tracking")]
impl Pair {
    pub fn new_sourced(car: Expression, cdr: Expression, src: Option<SourceView>) -> Ref<Self> {
        Ref::new(Pair { car, cdr, src })
    }

    pub fn get_source(&self) -> Option<SourceView> {
        self.src.clone()
    }
}

#[derive(Clone)]
pub enum Expression {
    Undefined,
    Nil,
    Symbol(Symbol),
    Special(Symbol),
    String(Ref<String>),
    Char(char),
    Number(Number),
    True,
    False,
    Pair(Ref<Pair>),
    Procedure(Procedure<EnvRef>),
    Macro(Macro),
    NativeMacro(MacroFn),
    Native(NativeFn),
    NativeIntrusive(NativeIntrusiveFn),
    NativeClosure(Ref<NativeClosure>),
    Vector(Ref<Vec<Expression>>),
    OpaqueVector(Ref<Vec<Expression>>),
    File(Ref<Option<File>>),

    Class(Ref<Class>),
    Instance(Ref<Instance>),
}

impl Expression {
    pub fn zero() -> Self {
        Expression::int(0)
    }

    pub fn one() -> Self {
        Expression::int(1)
    }

    pub fn int(x: impl Into<Int>) -> Self {
        let x: Int = x.into();
        Expression::Number(x.into())
    }

    pub fn number(x: impl Into<Number>) -> Self {
        Expression::Number(x.into())
    }

    pub fn from_literal<T: AsRef<str> + ToString>(s: T) -> Self {
        match s.as_ref() {
            "#t" => return Expression::True,
            "#f" => return Expression::False,
            _ => {}
        }

        if let Ok(i) = s.as_ref().parse::<Int>() {
            return Expression::Number(i.into());
        }

        if let Ok(f) = s.as_ref().parse::<f64>() {
            return Expression::Number(f.into());
        }

        Expression::Symbol(Symbol::new(s))
    }

    pub fn from_vec(l: Vec<Expression>) -> Self {
        let mut list = Expression::Nil;
        for x in l.into_iter().rev() {
            list = Expression::cons(x, list);
        }
        list
    }

    pub fn vector(size: usize) -> Self {
        Expression::Vector(Ref::new(vec![Expression::Undefined; size]))
    }

    pub fn opaque_vector(size: usize) -> Self {
        Expression::OpaqueVector(Ref::new(vec![Expression::Undefined; size]))
    }

    pub fn cons(car: impl Into<Expression>, cdr: impl Into<Expression>) -> Self {
        Expression::Pair(Pair::new(car.into(), cdr.into()))
    }

    pub fn decons(&self) -> Result<(&Expression, &Expression)> {
        match self {
            Expression::Pair(pair) => Ok((&pair.car, &pair.cdr)),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn car(&self) -> Result<&Expression> {
        match self {
            Expression::Pair(pair) => Ok(&pair.car),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn cdr(&self) -> Result<&Expression> {
        match self {
            Expression::Pair(pair) => Ok(&pair.cdr),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    /*pub fn car_mut(&mut self) -> Result<&mut Expression> {
        match self {
            Expression::Pair(pair) => Ok(&mut Ref::get_mut(pair)
                .expect("mutable reference must be unique")
                .0),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }*/

    pub fn cdr_mut(&mut self) -> Result<&mut Expression> {
        match self {
            Expression::Pair(pair) => Ok(&mut Ref::get_mut(pair)
                .expect("mutable reference must be unique")
                .cdr),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn set_car(&self, x: Expression) -> Result<()> {
        match self {
            Expression::Pair(pair) => {
                // mutating shared data is unsafe in Rust but expected behavior in Scheme.
                unsafe {
                    let car = &pair.car as *const _ as *mut Expression;
                    *car = x;
                }
            }
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
        Ok(())
    }

    pub fn set_cdr(&self, x: Expression) -> Result<()> {
        match self {
            Expression::Pair(pair) => {
                // mutating shared data is unsafe in Rust but expected behavior in Scheme.
                unsafe {
                    let cdr = &pair.cdr as *const _ as *mut Expression;
                    *cdr = x;
                }
            }
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
        Ok(())
    }

    pub fn get_source(&self) -> Option<SourceView> {
        match self {
            Expression::Pair(pair) => pair.get_source(),
            _ => None,
        }
    }

    pub fn sourced(self, src: Option<SourceView>) -> Expression {
        match self {
            Expression::Pair(pair) => {
                Expression::Pair(Pair::new_sourced(pair.car.clone(), pair.cdr.clone(), src))
            }
            _ => self,
        }
    }

    pub fn iter_list(&self) -> ListIterator {
        ListIterator::from_expression(self)
    }

    /*pub fn len(&self) -> Result<usize> {
        let mut n = 0;
        for x in self.iter_list() {
            x?;
            n += 1;
        }
        Ok(n)
    }*/

    pub fn append(&self, last: Expression) -> Result<Self> {
        let mut start = Expression::Nil;
        let mut current = &mut start;

        for x in self.iter_list() {
            *current = Expression::cons(x?.clone(), Expression::Nil);
            current = current.cdr_mut().unwrap();
        }

        *current = last;

        Ok(start)
    }

    pub fn map_list<F: FnMut(&Expression) -> Result<Expression>>(
        &self,
        mut func: F,
    ) -> Result<Expression> {
        let mut result = Expression::Nil;
        let mut in_cursor = self;
        let mut out_cursor = &mut result;

        loop {
            match in_cursor {
                Expression::Nil => break,
                Expression::Pair(pair) => {
                    let Pair { car, cdr, .. } = &**pair;
                    let x = func(&car)?;
                    in_cursor = &*cdr;

                    *out_cursor = Expression::cons(x, Expression::Nil);
                    out_cursor = out_cursor.cdr_mut().unwrap();
                }
                _ => return Err(ErrorKind::TypeError("not a list".into()))?,
            }
        }

        Ok(result)
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Expression::True | Expression::False => true,
            _ => false,
        }
    }

    pub fn is_char(&self) -> bool {
        match self {
            Expression::Char(_) => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Expression::Nil => true,
            _ => false,
        }
    }

    pub fn is_true(&self) -> bool {
        match self {
            Expression::False => false,
            _ => true,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Expression::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Expression::Number(n) => n.is_integer(),
            _ => false,
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            Expression::Number(n) => n.is_exact(),
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Expression::String(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Expression::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_named_symbol<T: AsRef<str>>(&self, name: T) -> bool {
        match self {
            Expression::Symbol(s) => s.name() == name.as_ref(),
            _ => false,
        }
    }

    pub fn is_procedure(&self) -> bool {
        match self {
            Expression::Procedure(_) => true,
            Expression::Native(_) => true,
            Expression::NativeIntrusive(_) => true,
            _ => false,
        }
    }

    /*pub fn is_list(&self) -> bool {
        match self {
            Expression::Nil => true,
            Expression::Pair(pair) => pair.1.is_list(),
            _ => false,
        }
    }*/

    pub fn is_pair(&self) -> bool {
        match self {
            Expression::Pair(_) => true,
            _ => false,
        }
    }

    pub fn is_vector(&self) -> bool {
        match self {
            Expression::Vector(_) => true,
            Expression::OpaqueVector(_) => true,
            _ => false,
        }
    }

    pub fn try_as_vector(&self) -> Option<&[Self]> {
        match self {
            Expression::Vector(v) => Some(v),
            Expression::OpaqueVector(v) => Some(v),
            _ => None,
        }
    }

    pub fn try_as_vector_mut(&self) -> Option<&mut [Self]> {
        match self {
            Expression::OpaqueVector(v) | Expression::Vector(v) => unsafe {
                // mutating shared data is unsafe in Rust but expected behavior in Scheme.
                let v = &mut *(&**v as *const _ as *mut Vec<_>);
                Some(v.as_mut_slice())
            },
            _ => None,
        }
    }

    pub fn try_as_integer(&self) -> Result<&Int> {
        self.try_as_number().and_then(|n| {
            n.try_as_integer()
                .ok_or_else(|| ErrorKind::TypeError(format!("{} is not an integer.", self)).into())
        })
    }

    pub fn try_as_number(&self) -> Result<&Number> {
        match self {
            Expression::Number(n) => Ok(n),
            _ => Err(ErrorKind::TypeError(format!("{} is not a number.", self)).into()),
        }
    }

    pub fn try_as_float(&self) -> Result<f64> {
        match self {
            Expression::Number(n) => n.to_f64().ok_or_else(|| {
                ErrorKind::TypeError(format!("cannot convert {} to float.", self)).into()
            }),
            _ => Err(ErrorKind::TypeError(format!("{} is not a number.", self)).into()),
        }
    }

    pub fn try_as_symbol(&self) -> Result<&Symbol> {
        match self {
            Expression::Symbol(s) => Ok(s),
            _ => Err(ErrorKind::TypeError(format!("{} is not a Symbol.", self)).into()),
        }
    }

    pub fn try_as_str(&self) -> Result<&str> {
        match self {
            Expression::String(s) => Ok(s),
            _ => Err(ErrorKind::TypeError(format!("{} is not a String.", self)).into()),
        }
    }

    pub fn try_as_class(&self) -> Option<&Ref<Class>> {
        match self {
            Expression::Class(c) => Some(c),
            _ => None,
        }
    }

    pub fn try_as_instance(&self) -> Option<&Ref<Instance>> {
        match self {
            Expression::Instance(o) => Some(o),
            _ => None,
        }
    }

    /*pub fn try_into_symbol(self) -> Result<Symbol> {
        match self {
            Expression::Symbol(s) => Ok(s),
            _ => Err(ErrorKind::TypeError(format!("{} is not a Symbol.", self)).into()),
        }
    }*/

    /*pub fn try_to_vec(&self) -> Result<Vec<Expression>> {
        self.iter_list().map(|r| r.map(Clone::clone)).collect()
    }*/

    /*pub fn logical_and(self, other: Self) -> Result<Self> {
        if self.is_true() {
            Ok(other)
        } else {
            Ok(self)
        }
    }

    pub fn logical_or(self, other: Self) -> Result<Self> {
        if self.is_true() {
            Ok(self)
        } else {
            Ok(other)
        }
    }*/

    pub fn min(self, other: Self) -> Result<Self> {
        Ok(if other < self { other } else { self })
    }

    pub fn max(self, other: Self) -> Result<Self> {
        Ok(if self < other { other } else { self })
    }

    pub fn eqv(&self, rhs: &Self) -> bool {
        use Expression::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a.eqv(b),
            (String(a), String(b)) => Ref::ptr_eq(a, b),
            (Symbol(a), Symbol(b))
            | (Symbol(a), Special(b))
            | (Special(a), Symbol(b))
            | (Special(a), Special(b)) => a == b,
            (Char(a), Char(b)) => a == b,
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            (Pair(a), Pair(b)) => Ref::ptr_eq(a, b),
            (Vector(a), Vector(b)) | (OpaqueVector(a), OpaqueVector(b)) => Ref::ptr_eq(a, b),
            (Procedure(a), Procedure(b)) => a.eqv(b),
            (Native(a), Native(b)) => a == b,
            (Class(a), Class(b)) => Ref::ptr_eq(a, b),
            (Instance(a), Instance(b)) => Ref::ptr_eq(a, b),
            _ => false,
        }
    }

    pub fn equal(&self, rhs: &Self) -> bool {
        use Expression::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a.eqv(b),
            (String(a), String(b)) => a == b,
            (Symbol(a), Symbol(b))
            | (Symbol(a), Special(b))
            | (Special(a), Symbol(b))
            | (Special(a), Special(b)) => a == b,
            (Char(a), Char(b)) => a == b,
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            (Pair(a), Pair(b)) => a.car == b.car && a.cdr == b.cdr,
            (Vector(a), Vector(b)) | (OpaqueVector(a), OpaqueVector(b)) => {
                if a.len() != b.len() {
                    false
                } else {
                    for (ai, bi) in a.iter().zip(b.iter()) {
                        if ai != bi {
                            return false;
                        }
                    }
                    true
                }
            }
            (Procedure(a), Procedure(b)) => a.equal(b),
            (Native(a), Native(b)) => a == b,
            _ => false,
        }
    }

    pub fn round(&self) -> Result<Self> {
        match self {
            Expression::Number(n) => Ok(Expression::Number(n.round())),
            _ => Err(ErrorKind::TypeError(format!("not a number: {}", self)).into()),
        }
    }

    pub fn truncate_quotient(&self, other: &Self) -> Result<Self> {
        use Expression::*;
        match (self, other) {
            (Number(a), Number(b)) => a.truncate_quotient(b).map(Number),
            (a, b) => Err(ErrorKind::TypeError(format!("not a number: {}, {}", a, b)).into()),
        }
    }

    pub fn truncate_remainder(&self, other: &Self) -> Result<Self> {
        use Expression::*;
        match (self, other) {
            (Number(a), Number(b)) => a.truncate_remainder(b).map(Number),
            (a, b) => Err(ErrorKind::TypeError(format!("not a number: {}, {}", a, b)).into()),
        }
    }

    pub fn short_repr(&self) -> String {
        match self {
            Expression::Undefined => "#<unspecified>".into(),
            Expression::Nil => "()".into(),
            Expression::Symbol(s) => format!("{}", s),
            Expression::Special(s) => format!("<{}>", s),
            Expression::String(s) => format!("{:?}", s),
            Expression::Number(n) => format!("{}", n),
            Expression::Char(ch) => format!("{:?}", ch),
            Expression::True => "#t".into(),
            Expression::False => "#f".into(),
            Expression::Pair(pair) => {
                let Pair { car, cdr, .. } = &**pair;
                let mut s = format!("({}", car.short_repr());
                let mut cdr = cdr;
                loop {
                    match cdr {
                        Expression::Nil => break,
                        Expression::Pair(ref pair) => {
                            s += &format!(" {}", pair.car.short_repr());
                            cdr = &pair.cdr;
                        }
                        _ => {
                            s += &format!(" . {}", cdr.short_repr());
                            break;
                        }
                    }
                }
                s.push(')');
                s
            }
            Expression::Procedure(p) => format!("{}", p.name()),
            Expression::Macro(_) => "<syntax>".into(),
            Expression::NativeMacro(_) => "<native syntax>".into(),
            Expression::Native(_)
            | Expression::NativeIntrusive(_)
            | Expression::NativeClosure(_) => "<primitive>".into(),
            Expression::Vector(v) => {
                let items: Vec<_> = v.iter().map(|x| x.short_repr()).collect();
                format!("#({})", items.join(" "))
            }
            Expression::OpaqueVector(v) => format!("<opaque vector with {} elements>", v.len()),
            Expression::File(f) => format!("<file: {:?}>", f),
            //Expression::Error(_) => "<ERROR>".into(),
            Expression::Class(cls) => format!("<class {}>", cls.name),
            Expression::Instance(obj) => format!("<instance of {} {:p}>", obj.base.name, &**obj),
        }
    }
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Undefined => write!(f, "#<unspecified>"),
            Expression::Nil => write!(f, "'()"),
            Expression::Symbol(s) => write!(f, "{:?}", s),
            Expression::Special(s) => write!(f, "<{:?}>", s),
            Expression::String(s) => write!(f, "{:?}", s),
            Expression::Number(n) => write!(f, "{}", n),
            Expression::Char(ch) => write!(f, "{:?}", ch),
            Expression::True => write!(f, "#t"),
            Expression::False => write!(f, "#f"),
            Expression::Pair(pair) => {
                let car = &pair.car;
                let mut cdr = &pair.cdr;
                write!(f, "({:?}", car)?;
                loop {
                    match cdr {
                        Expression::Nil => break,
                        Expression::Pair(p) => {
                            write!(f, " {:?}", p.car)?;
                            cdr = &p.cdr;
                        }
                        _ => {
                            write!(f, " . {:?}", cdr)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Expression::Procedure(p) => write!(f, "#<procedure {:p} {}>", p, p.params_ex()),
            Expression::Macro(m) => write!(f, "#<macro {}>", m.name()),
            Expression::NativeMacro(_) => write!(f, "<native macro>"),
            Expression::Native(_)
            | Expression::NativeIntrusive(_)
            | Expression::NativeClosure(_) => write!(f, "<native>"),
            Expression::Vector(v) => write!(f, "{:?}", v),
            Expression::OpaqueVector(v) => write!(f, "<opaque vector with {} elements>", v.len()),
            Expression::File(fh) => write!(f, "<file: {:?}>", fh),
            Expression::Class(cls) => write!(f, "<class {}>", cls.name),
            Expression::Instance(obj) => write!(f, "<instance of {} {:p}>", obj.base.name, &**obj),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Undefined => write!(f, "#<unspecified>"),
            Expression::Nil => write!(f, "'()"),
            Expression::Symbol(s) => write!(f, "{}", s.name()),
            Expression::String(s) => write!(f, "{}", s),
            Expression::Special(s) => write!(f, "<{}>", s),
            Expression::Number(n) => write!(f, "{}", n),
            Expression::Char(ch) => write!(f, "{}", ch),
            Expression::True => write!(f, "#t"),
            Expression::False => write!(f, "#f"),
            Expression::Pair(pair) => {
                let car = &pair.car;
                let mut cdr = &pair.cdr;
                write!(f, "({}", car)?;
                loop {
                    match cdr {
                        Expression::Nil => break,
                        Expression::Pair(p) => {
                            write!(f, " {}", p.car)?;
                            cdr = &p.cdr;
                        }
                        _ => {
                            write!(f, " . {}", cdr)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Expression::Procedure(p) => write!(f, "#<procedure {:p} {}>", p, p.params_ex()),
            Expression::Macro(m) => write!(f, "#<macro {}>", m.name()),
            Expression::NativeMacro(_) => write!(f, "<native macro>"),
            Expression::Native(_)
            | Expression::NativeIntrusive(_)
            | Expression::NativeClosure(_) => write!(f, "<native>"),
            Expression::Vector(v) => {
                write!(f, "[")?;
                if !v.is_empty() {
                    write!(f, "{}", v[0])?;
                    for x in &v[1..] {
                        write!(f, " {}", x)?;
                    }
                }
                write!(f, "]")
            }
            Expression::OpaqueVector(v) => write!(f, "<opaque vector with {} elements>", v.len()),
            Expression::File(fh) => write!(f, "<file: {:?}>", fh),
            Expression::Class(cls) => write!(f, "<class {}>", cls.name),
            Expression::Instance(obj) => write!(f, "<instance of {} {:p}>", obj.base.name, &**obj),
        }
    }
}

impl From<&str> for Expression {
    fn from(s: &str) -> Self {
        Expression::String(Ref::new(s.into()))
    }
}

impl From<Symbol> for Expression {
    fn from(s: Symbol) -> Self {
        Expression::Symbol(s)
    }
}

impl From<String> for Expression {
    fn from(s: String) -> Self {
        Expression::String(Ref::new(s))
    }
}

impl From<i64> for Expression {
    fn from(i: i64) -> Self {
        Expression::int(i)
    }
}

impl From<f64> for Expression {
    fn from(f: f64) -> Self {
        Expression::number(f)
    }
}

impl From<bool> for Expression {
    fn from(b: bool) -> Self {
        if b {
            Expression::True
        } else {
            Expression::False
        }
    }
}

impl From<Number> for Expression {
    fn from(n: Number) -> Self {
        Expression::Number(n)
    }
}

impl From<Vec<Expression>> for Expression {
    fn from(vec: Vec<Expression>) -> Self {
        Expression::from_vec(vec)
    }
}

impl FromIterator<Expression> for Expression {
    fn from_iter<I: IntoIterator<Item = Expression>>(iter: I) -> Self {
        let mut result = Expression::Nil;
        let mut cursor = &mut result;

        for expr in iter {
            *cursor = Expression::cons(expr, Expression::Nil);
            cursor = cursor.cdr_mut().unwrap();
        }

        result
    }
}

impl std::convert::TryFrom<&Expression> for Symbol {
    type Error = crate::errors::Error;

    fn try_from(x: &Expression) -> Result<Symbol> {
        match x {
            Expression::Symbol(s) => Ok(*s),
            _ => Err(ErrorKind::TypeError(format!("Expected symbol: {:?}", x)).into()),
        }
    }
}

impl TryFrom<&Expression> for Int {
    type Error = crate::errors::Error;

    fn try_from(x: &Expression) -> Result<Int> {
        x.try_as_number().and_then(|n| {
            n.to_integer()
                .and_then(Number::try_into_integer)
                .ok_or_else(|| ErrorKind::TypeError(format!("Expected integer: {:?}", x)).into())
        })
    }
}

impl std::convert::TryFrom<&Expression> for f64 {
    type Error = crate::errors::Error;

    fn try_from(x: &Expression) -> Result<f64> {
        x.try_as_number().and_then(|n| {
            n.to_f64()
                .ok_or_else(|| ErrorKind::TypeError(format!("Expected integer: {:?}", x)).into())
        })
    }
}

impl std::convert::TryFrom<&Expression> for usize {
    type Error = crate::errors::Error;

    fn try_from(x: &Expression) -> Result<usize> {
        x.try_as_number().and_then(|n| {
            n.to_usize()
                .ok_or_else(|| ErrorKind::TypeError(format!("Expected integer: {:?}", x)).into())
        })
    }
}

impl<'a> std::convert::TryFrom<&'a Expression> for &'a str {
    type Error = crate::errors::Error;

    fn try_from(x: &'a Expression) -> Result<&'a str> {
        match x {
            Expression::String(s) => Ok(s),
            _ => Err(ErrorKind::TypeError(format!("Expected string: {:?}", x)).into()),
        }
    }
}

impl std::convert::TryFrom<&Expression> for bool {
    type Error = crate::errors::Error;

    fn try_from(x: &Expression) -> Result<bool> {
        match x {
            Expression::True => Ok(true),
            Expression::False => Ok(false),
            _ => Err(ErrorKind::TypeError(format!("Expected boolean: {:?}", x)).into()),
        }
    }
}

impl std::convert::TryFrom<&Expression> for Expression {
    type Error = crate::errors::Error;

    fn try_from(expr: &Expression) -> Result<Self> {
        Ok(expr.clone())
    }
}

impl FromPrimitive for Expression {
    fn from_i64(n: i64) -> Option<Self> {
        Number::from_i64(n).map(Expression::Number)
    }

    fn from_u64(n: u64) -> Option<Self> {
        Number::from_u64(n).map(Expression::Number)
    }

    fn from_i128(n: i128) -> Option<Self> {
        Number::from_i128(n).map(Expression::Number)
    }

    fn from_u128(n: u128) -> Option<Self> {
        Number::from_u128(n).map(Expression::Number)
    }

    fn from_f32(n: f32) -> Option<Self> {
        Number::from_f32(n).map(Expression::Number)
    }

    fn from_f64(n: f64) -> Option<Self> {
        Number::from_f64(n).map(Expression::Number)
    }
}

impl std::ops::Add for Expression {
    type Output = Result<Expression>;
    fn add(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Number(a), Number(b)) => Ok(Number(a + b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot add {} + {}", a, b)).into()),
        }
    }
}

impl std::ops::Mul for Expression {
    type Output = Result<Expression>;
    fn mul(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Number(a), Number(b)) => Ok(Number(a * b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot multiply {} * {}", a, b)).into()),
        }
    }
}

impl std::ops::Sub for Expression {
    type Output = Result<Expression>;
    fn sub(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Number(a), Number(b)) => Ok(Number(a - b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot subtract {} - {}", a, b)).into()),
        }
    }
}

impl std::ops::Div for Expression {
    type Output = Result<Expression>;
    fn div(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Number(a), Number(b)) => Ok(Number(a / b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot divide {} / {}", a, b)).into()),
        }
    }
}

impl std::ops::Rem for Expression {
    type Output = Result<Expression>;
    fn rem(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Number(a), Number(b)) => Ok(Number(a % b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot divide {} / {}", a, b)).into()),
        }
    }
}

impl std::cmp::PartialOrd for Expression {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        use Expression::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a.partial_cmp(b),
            (String(a), String(b)) => a.partial_cmp(b),
            (Symbol(a), Symbol(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl std::cmp::PartialEq for Expression {
    fn eq(&self, rhs: &Self) -> bool {
        use Expression::*;
        match (self, rhs) {
            (Number(a), Number(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Special(a), Special(b)) => a == b,
            //(Char(a), Char(b)) => a == b,
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            (Pair(a), Pair(b)) => a.car == b.car && a.cdr == b.cdr,
            _ => false,
        }
    }
}

impl std::cmp::PartialEq<Symbol> for Expression {
    fn eq(&self, rhs: &Symbol) -> bool {
        match self {
            Expression::Symbol(s) => s == rhs,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct Procedure<E> {
    body: Ref<Expression>,
    params: Ref<Expression>,
    env: E,
    name: Symbol,
}

impl<E: Default> Default for Procedure<E> {
    fn default() -> Self {
        Procedure {
            body: Ref::new(Expression::Nil),
            params: Ref::new(Expression::Nil),
            env: Default::default(),
            name: "()".into(),
        }
    }
}

impl<E> Procedure<E> {
    pub fn name(&self) -> Symbol {
        self.name
    }

    pub fn rename(mut self, name: Symbol) -> Self {
        self.name = name;
        self
    }

    pub fn body_ex(&self) -> &Expression {
        &self.body
    }

    pub fn params_ex(&self) -> &Expression {
        &self.params
    }

    pub fn env(&self) -> &E {
        &self.env
    }

    pub fn eqv(&self, other: &Self) -> bool {
        Ref::ptr_eq(&self.body, &other.body)
    }

    pub fn equal(&self, other: &Self) -> bool {
        self.body.equal(&other.body) && self.params.equal(&other.params)
    }
}

impl Procedure<EnvRef> {
    pub fn new(params: Ref<Expression>, body: Ref<Expression>, env: EnvRef) -> Self {
        Procedure {
            body,
            params,
            env,
            name: symbol::GREEK_LAMBDA,
        }
    }

    pub fn build(signature: Expression, body: Expression, env: &EnvRef) -> Result<Self> {
        Ok(Procedure::new(
            Ref::new(signature),
            Ref::new(body),
            env.clone(),
        ))
    }

    pub fn new_local_env(&self, args: Expression) -> Result<EnvRef> {
        let mut env = Environment::new_child(self.env.clone(), self.clone());
        env.set_vars(self.params_ex().clone(), args)?;
        Ok(env.into())
    }
}

impl<T> Hash for Procedure<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let id = Ref::as_ref(&self.body) as *const _ as *const u8;
        id.hash(state);
    }
}

impl<T> Eq for Procedure<T> {}
impl<T> PartialEq for Procedure<T> {
    fn eq(&self, other: &Self) -> bool {
        self.eqv(other)
    }
}

impl From<Procedure<EnvRef>> for Procedure<EnvWeak> {
    fn from(proc: Procedure<EnvRef>) -> Self {
        Procedure {
            body: proc.body,
            params: proc.params,
            env: proc.env.downgrade(),
            name: proc.name,
        }
    }
}

impl From<Procedure<EnvWeak>> for Procedure<EnvRef> {
    fn from(proc: Procedure<EnvWeak>) -> Self {
        Procedure {
            env: proc
                .env
                .upgrade()
                .unwrap_or_else(|| panic!("procedure's environment has been dropped")),
            body: proc.body,
            params: proc.params,
            name: proc.name,
        }
    }
}

impl From<Procedure<EnvWeak>> for Expression {
    fn from(proc: Procedure<EnvWeak>) -> Self {
        Expression::Procedure(proc.into())
    }
}

impl<T> std::fmt::Debug for Procedure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", Expression::cons(self.name, (*self.params).clone()))
    }
}

pub struct ListIterator<'a> {
    next_pair: &'a Expression,
}

impl<'a> ListIterator<'a> {
    pub fn from_expression(expr: &'a Expression) -> Self {
        ListIterator { next_pair: expr }
    }

    pub fn next_expr(&mut self) -> Result<Option<&'a Expression>> {
        self.next().transpose()
    }

    pub fn head(&self) -> Result<&'a Expression> {
        match self.next_pair {
            Expression::Nil => Ok(self.next_pair),
            Expression::Pair(pair) => Ok(&pair.car),
            _ => Err(ErrorKind::TypeError("not a list".into()))?,
        }
    }

    pub fn tail(&self) -> Result<&'a Expression> {
        match self.next_pair {
            Expression::Nil => Ok(self.next_pair),
            Expression::Pair(pair) => Ok(&pair.cdr),
            _ => Err(ErrorKind::TypeError("not a list".into()))?,
        }
    }
}

impl<'a> Iterator for ListIterator<'a> {
    type Item = Result<&'a Expression>;
    fn next(&mut self) -> Option<Self::Item> {
        let (car, cdr) = match self.next_pair {
            Expression::Nil => return None,
            Expression::Pair(pair) => (&pair.car, &pair.cdr),
            _ => return Some(Err(ErrorKind::TypeError("not a list".into()).into())),
        };

        self.next_pair = cdr;
        Some(Ok(car))
    }
}

pub fn cons(a: Expression, d: Expression) -> Expression {
    Expression::cons(a, d)
}

#[derive(Debug)]
pub struct Class {
    name: Symbol,
    base: Option<Ref<Class>>,
    n_fields: usize,
    field_names: Vec<Symbol>,
    methods: HashMap<Symbol, Expression>,
}

impl Class {
    pub fn new(name: Symbol, base: Option<Ref<Self>>, fields: Vec<Symbol>) -> Self {
        let n_fields = fields.len() + base.as_ref().map(|cls| cls.n_fields).unwrap_or(0);
        Class {
            name,
            base,
            n_fields,
            field_names: fields,
            methods: HashMap::new(),
        }
    }

    pub fn n_base_fields(&self) -> usize {
        self.base.as_ref().map(|cls| cls.n_fields).unwrap_or(0)
    }

    fn field_offset(&self) -> usize {
        self.n_base_fields()
    }

    pub fn all_field_names(&self) -> Vec<Symbol> {
        let mut names = self
            .base
            .as_ref()
            .map(|cls| cls.all_field_names())
            .unwrap_or(vec![]);
        names.extend(&self.field_names);
        names
    }

    pub fn add_method(&mut self, name: Symbol, callable: Expression) {
        self.methods.insert(name, callable);
    }

    pub fn instantiate(cls: Ref<Self>, field_values: Vec<Expression>) -> Result<Instance> {
        if field_values.len() != cls.n_fields {
            return Err(ErrorKind::ArgumentError.into());
        }
        Ok(Instance {
            base: cls,
            field_values,
        })
    }

    pub fn get_field_index(&self, name: &Symbol) -> Option<usize> {
        for (i, n) in self.field_names.iter().enumerate() {
            if n == name {
                return Some(i + self.field_offset());
            }
        }

        self.base.as_ref().and_then(|b| b.get_field_index(name))
    }

    pub fn invoke_method(&self, method: Symbol, args: Expression, env: EnvRef) -> Result<Return> {
        match self.methods.get(&method) {
            Some(m) => return apply(m.clone(), args, &env),
            None => {}
        }
        match self.base {
            Some(ref bc) => bc.invoke_method(method, args, env),
            None => Err(ErrorKind::TypeError(format!("method {} not found", method)).into()),
        }
    }
}

pub struct Instance {
    base: Ref<Class>,
    field_values: Vec<Expression>,
}

impl Instance {
    pub fn is_instance(&self, cls: &Ref<Class>) -> bool {
        let mut base = &self.base;
        loop {
            if Ref::ptr_eq(&base, cls) {
                return true;
            }
            match base.base {
                Some(ref b) => base = b,
                None => return false,
            }
        }
    }

    pub fn base(&self) -> &Ref<Class> {
        &self.base
    }

    pub fn get_field_value(&self, name: &Symbol) -> Option<&Expression> {
        self.base
            .get_field_index(name)
            .map(|idx| &self.field_values[idx])
    }

    pub fn set_field_value(&mut self, name: &Symbol, value: Expression) -> Option<()> {
        self.base
            .get_field_index(name)
            .map(|idx| self.field_values[idx] = value)
    }

    pub fn invoke_method(&self, method: Symbol, args: Expression, env: EnvRef) -> Result<Return> {
        self.base.invoke_method(method, args, env)
    }
}

pub fn define_class(env: &mut Environment, class: Ref<Class>) {
    env.insert(class.name, Expression::Class(class.clone()));

    env.insert(
        format!("make-{}", class.name).as_str(),
        Expression::NativeClosure(Ref::new(NativeClosure::new(
            vec![Box::new(class.clone())],
            |args, vars| {
                let cls = vars[0].downcast_ref::<Ref<Class>>().unwrap().clone();
                Ok(Expression::Instance(Ref::new(Class::instantiate(
                    cls,
                    args.iter_list()
                        .map(|x| Ok(x?.clone()))
                        .collect::<Result<_>>()?,
                )?))
                .into())
            },
        ))),
    );

    env.insert(
        format!("{}?", class.name).as_str(),
        Expression::NativeClosure(Ref::new(NativeClosure::new(
            vec![Box::new(class.clone())],
            |args, vars| {
                let cls = vars[0].downcast_ref::<Ref<Class>>().unwrap();
                let obj = args.car()?;

                Ok(obj
                    .try_as_instance()
                    .map(|o| o.is_instance(cls))
                    .unwrap_or(false)
                    .into())
            },
        ))),
    );

    for (idx, field) in class.all_field_names().iter().enumerate() {
        env.insert(
            format!("{}-{}", class.name, field).as_str(),
            Expression::NativeClosure(Ref::new(NativeClosure::new(
                vec![Box::new(class.clone()), Box::new(idx)],
                |args, vars| {
                    let cls = vars[0].downcast_ref::<Ref<Class>>().unwrap();
                    let idx = *vars[1].downcast_ref::<usize>().unwrap();
                    let obj = args.car()?;

                    Ok(obj
                        .try_as_instance()
                        .filter(|o| o.is_instance(cls))
                        .map(|o| o.field_values[idx].clone())
                        .ok_or(ErrorKind::TypeError(format!(
                            "Expected instance of {}",
                            cls.name
                        )))?
                        .into())
                },
            ))),
        );

        let cls = class.clone();
        env.insert(
            format!("set-{}-{}!", cls.name, field).as_str(),
            Expression::NativeClosure(Ref::new(NativeClosure::new(
                vec![Box::new(class.clone()), Box::new(idx)],
                |args, vars| {
                    let cls = vars[0].downcast_ref::<Ref<Class>>().unwrap();
                    let idx = *vars[1].downcast_ref::<usize>().unwrap();
                    let obj = args.car()?;
                    let value = args.cdr()?.car()?;

                    let obj = obj.try_as_instance().filter(|o| o.is_instance(cls)).ok_or(
                        ErrorKind::TypeError(format!("Expected instance of {}", cls.name)),
                    )?;

                    let obj = unsafe { &mut *(&**obj as *const Instance as *mut Instance) };

                    obj.field_values[idx] = value.clone();

                    Ok(Expression::Undefined.into())
                },
            ))),
        );
    }
}

pub fn define_method(
    class: Ref<Class>,
    name: Symbol,
    params: Expression,
    body: Expression,
    env: EnvRef,
) {
    let the_method = Expression::Procedure(Procedure::new(Ref::new(params), Ref::new(body), env));

    let class = unsafe { &mut *(&*class as *const Class as *mut Class) };

    class.add_method(name, the_method);
}

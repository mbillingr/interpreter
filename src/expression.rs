use crate::environment::{EnvRef, EnvWeak, Environment};
use crate::errors::*;
use crate::symbol::{self, Symbol};
use std::rc::Rc;

pub type List = Expression;
pub type Args = Expression;
pub type NativeFn = fn(Args) -> Result<Expression>;
pub type NativeIntrusiveFn = fn(Args, &EnvRef) -> Result<Expression>;

#[derive(Clone)]
pub enum Expression {
    Undefined,
    Nil,
    Symbol(Symbol),
    String(Rc<String>),
    Char(char),
    Integer(i64),
    Float(f64),
    True,
    False,
    Pair(Rc<(Expression)>, Rc<(Expression)>),
    Procedure(Procedure<EnvRef>),
    Native(NativeFn),
    NativeIntrusive(NativeIntrusiveFn),
    Error(Rc<List>),
}

impl Expression {
    pub fn zero() -> Self {
        Expression::Integer(0)
    }

    pub fn one() -> Self {
        Expression::Integer(1)
    }

    pub fn from_literal<T: AsRef<str> + ToString>(s: T) -> Self {
        match s.as_ref() {
            "#t" => return Expression::True,
            "#f" => return Expression::False,
            _ => {}
        }

        if let Ok(i) = s.as_ref().parse() {
            return Expression::Integer(i);
        }

        if let Ok(f) = s.as_ref().parse() {
            return Expression::Float(f);
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

    pub fn cons(car: impl Into<Expression>, cdr: impl Into<Expression>) -> Self {
        Expression::Pair(Rc::new(car.into()), Rc::new(cdr.into()))
    }

    pub fn decons(&self) -> Result<(&Expression, &Expression)> {
        match self {
            Expression::Pair(car, cdr) => Ok((car, cdr)),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn cons_rc(car: Rc<Expression>, cdr: Rc<Expression>) -> Self {
        Expression::Pair(car, cdr)
    }

    pub fn decons_rc(&self) -> Result<(&Rc<Expression>, &Rc<Expression>)> {
        match self {
            Expression::Pair(car, cdr) => Ok((car, cdr)),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn car(&self) -> Result<&Expression> {
        match self {
            Expression::Pair(car, _) => Ok(car),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn cdr(&self) -> Result<&Expression> {
        match self {
            Expression::Pair(_, cdr) => Ok(cdr),
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn car_mut(&mut self) -> Result<&mut Expression> {
        match self {
            Expression::Pair(car, _) => {
                Ok(Rc::get_mut(car).expect("mutable reference must be unique"))
            }
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn cdr_mut(&mut self) -> Result<&mut Expression> {
        match self {
            Expression::Pair(_, cdr) => {
                Ok(Rc::get_mut(cdr).expect("mutable reference must be unique"))
            }
            _ => Err(ErrorKind::TypeError(format!("not a pair: {}", self)))?,
        }
    }

    pub fn iter_list(&self) -> Result<ListIterator> {
        ListIterator::from_expression(self)
    }

    pub fn len(&self) -> Result<usize> {
        let mut n = 0;
        for x in self.iter_list()? {
            x?;
            n += 1;
        }
        Ok(n)
    }

    pub fn append(&self, last: Expression) -> Result<Self> {
        let mut start = Expression::Nil;
        let mut current = &mut start;

        for x in self.iter_list()? {
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
                Expression::Pair(car, cdr) => {
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
            Expression::Integer(_) => true,
            Expression::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Expression::Integer(_) => true,
            Expression::Float(f) if float_eq(*f, f.trunc()) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Expression::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_named_symbol(&self, name: &str) -> bool {
        match self {
            Expression::Symbol(s) => s.name() == name,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Expression::Nil => true,
            Expression::Pair(_, cdr) => cdr.is_list(),
            _ => false,
        }
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Expression::Pair(_, _) => true,
            _ => false,
        }
    }

    pub fn try_as_integer(&self) -> Result<i64> {
        match self {
            Expression::Integer(i) => Ok(*i),
            Expression::Float(f) if float_eq(*f, f.trunc()) => Ok(*f as i64),
            _ => Err(ErrorKind::TypeError(format!("{} is not an integer.", self)).into()),
        }
    }

    pub fn try_as_float(&self) -> Result<f64> {
        match self {
            Expression::Integer(i) => Ok(*i as f64),
            Expression::Float(f) => Ok(*f),
            _ => Err(ErrorKind::TypeError(format!("{} is not a number.", self)).into()),
        }
    }

    pub fn try_as_symbol(&self) -> Result<&Symbol> {
        match self {
            Expression::Symbol(s) => Ok(s),
            _ => Err(ErrorKind::TypeError(format!("{} is not a Symbol.", self)).into()),
        }
    }

    pub fn try_into_symbol(self) -> Result<Symbol> {
        match self {
            Expression::Symbol(s) => Ok(s),
            _ => Err(ErrorKind::TypeError(format!("{} is not a Symbol.", self)).into()),
        }
    }

    pub fn try_to_vec(&self) -> Result<Vec<Expression>> {
        self.iter_list()?.map(|r| r.map(Clone::clone)).collect()
    }

    pub fn logical_and(self, other: Self) -> Result<Self> {
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
    }

    pub fn min(self, other: Self) -> Result<Self> {
        Ok(if other < self { other } else { self })
    }

    pub fn max(self, other: Self) -> Result<Self> {
        Ok(if self < other { other } else { self })
    }

    pub fn eqv(&self, rhs: &Self) -> bool {
        use Expression::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a == b,
            (Float(a), Float(b)) => float_eq(*a, *b),
            (String(a), String(b)) => Rc::ptr_eq(a, b),
            (Symbol(a), Symbol(b)) => a == b,
            (Char(a), Char(b)) => a == b,
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            (Pair(a_car, a_cdr), Pair(b_car, b_cdr)) => {
                Rc::ptr_eq(a_car, b_car) && Rc::ptr_eq(a_cdr, b_cdr)
            }
            (Procedure(a), Procedure(b)) => a.eqv(b),
            (Native(a), Native(b)) => a == b,
            _ => false,
        }
    }

    pub fn equal(&self, rhs: &Self) -> bool {
        use Expression::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a == b,
            (Float(a), Float(b)) => float_eq(*a, *b),
            (String(a), String(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Char(a), Char(b)) => a == b,
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            (Pair(a_car, a_cdr), Pair(b_car, b_cdr)) => a_car == b_car && a_cdr == b_cdr,
            (Procedure(a), Procedure(b)) => a.equal(b),
            (Native(a), Native(b)) => a == b,
            _ => false,
        }
    }

    pub fn truncate_quotient(&self, other: &Self) -> Result<Self> {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a / b)),
            (Integer(a), Float(b)) => {
                if other.is_integer() {
                    Ok(Float(*a as f64 / b))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", b)).into())
                }
            }
            (Float(a), Integer(b)) => {
                if self.is_integer() {
                    Ok(Float(a / *b as f64))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", a)).into())
                }
            }
            (Float(a), Float(b)) if self.is_integer() && other.is_integer() => Ok(Float(a / b)),
            (a, b) => Err(ErrorKind::TypeError(format!("not integers: {}, {}", a, b)).into()),
        }
    }

    pub fn truncate_remainder(&self, other: &Self) -> Result<Self> {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a % b)),
            (Integer(a), Float(b)) => {
                if other.is_integer() {
                    Ok(Float(*a as f64 % b))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", b)).into())
                }
            }
            (Float(a), Integer(b)) => {
                if self.is_integer() {
                    Ok(Float(a % *b as f64))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", a)).into())
                }
            }
            (Float(a), Float(b)) if self.is_integer() && other.is_integer() => Ok(Float(a % b)),
            (a, b) => Err(ErrorKind::TypeError(format!("not integers: {}, {}", a, b)).into()),
        }
    }
}

// This function exists to make clippy stop complaining about exact floating point comparison.
// I think it accepts this because of the name...
#[inline(always)]
fn float_eq(a: f64, b: f64) -> bool {
    a == b
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Undefined => write!(f, "#<unspecified>"),
            Expression::Nil => write!(f, "()"),
            Expression::Symbol(s) => write!(f, "{:?}", s),
            Expression::String(s) => write!(f, "{:?}", s),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(i) => write!(f, "{}", i),
            Expression::Char(ch) => write!(f, "{:?}", ch),
            Expression::True => write!(f, "#t"),
            Expression::False => write!(f, "#f"),
            Expression::Pair(ref car, ref cdr) => {
                let mut cdr = cdr;
                write!(f, "({:?}", car)?;
                loop {
                    match **cdr {
                        Expression::Nil => break,
                        Expression::Pair(ref a, ref d) => {
                            write!(f, " {:?}", a)?;
                            cdr = d;
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
            Expression::Native(_) | Expression::NativeIntrusive(_) => write!(f, "<native>"),
            Expression::Error(l) => {
                let tmp: Vec<_> = l
                    .iter_list()
                    .unwrap()
                    .map(|item| format!("{:?}", item.unwrap()))
                    .collect();
                write!(f, "ERROR: {}", tmp.join(" "))
            }
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Undefined => write!(f, "#<unspecified>"),
            Expression::Nil => write!(f, "()"),
            Expression::Symbol(s) => write!(f, "{}", s.name()),
            Expression::String(s) => write!(f, "{}", s),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(i) => write!(f, "{}", i),
            Expression::Char(ch) => write!(f, "{}", ch),
            Expression::True => write!(f, "#t"),
            Expression::False => write!(f, "#f"),
            Expression::Pair(ref car, ref cdr) => {
                let mut cdr = cdr;
                write!(f, "({}", car)?;
                loop {
                    match **cdr {
                        Expression::Nil => break,
                        Expression::Pair(ref a, ref d) => {
                            write!(f, " {}", a)?;
                            cdr = d;
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
            Expression::Native(_) | Expression::NativeIntrusive(_) => write!(f, "<native>"),
            Expression::Error(l) => {
                let tmp: Vec<_> = l
                    .iter_list()
                    .unwrap()
                    .map(|item| format!("{}", item.unwrap()))
                    .collect();
                write!(f, "ERROR: {}", tmp.join(" "))
            }
        }
    }
}

impl From<&str> for Expression {
    fn from(s: &str) -> Self {
        Expression::String(Rc::new(s.into()))
    }
}

impl From<Symbol> for Expression {
    fn from(s: Symbol) -> Self {
        Expression::Symbol(s)
    }
}

impl From<String> for Expression {
    fn from(s: String) -> Self {
        Expression::String(Rc::new(s))
    }
}

impl From<i64> for Expression {
    fn from(i: i64) -> Self {
        Expression::Integer(i)
    }
}

impl From<f64> for Expression {
    fn from(f: f64) -> Self {
        Expression::Float(f)
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

impl std::ops::Add for Expression {
    type Output = Result<Expression>;
    fn add(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a + b)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 + b)),
            (Float(a), Integer(b)) => Ok(Float(a + b as f64)),
            (Float(a), Float(b)) => Ok(Float(a + b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot add {} + {}", a, b)).into()),
        }
    }
}

impl std::ops::Mul for Expression {
    type Output = Result<Expression>;
    fn mul(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a * b)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 * b)),
            (Float(a), Integer(b)) => Ok(Float(a * b as f64)),
            (Float(a), Float(b)) => Ok(Float(a * b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot multiply {} * {}", a, b)).into()),
        }
    }
}

impl std::ops::Sub for Expression {
    type Output = Result<Expression>;
    fn sub(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a - b)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 - b)),
            (Float(a), Integer(b)) => Ok(Float(a - b as f64)),
            (Float(a), Float(b)) => Ok(Float(a - b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot subtract {} - {}", a, b)).into()),
        }
    }
}

impl std::ops::Div for Expression {
    type Output = Result<Expression>;
    fn div(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Float(a as f64 / b as f64)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 / b)),
            (Float(a), Integer(b)) => Ok(Float(a / b as f64)),
            (Float(a), Float(b)) => Ok(Float(a / b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot divide {} / {}", a, b)).into()),
        }
    }
}

impl std::ops::Rem for Expression {
    type Output = Result<Expression>;
    fn rem(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a % b)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 % b)),
            (Float(a), Integer(b)) => Ok(Float(a % b as f64)),
            (Float(a), Float(b)) => Ok(Float(a % b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot divide {} / {}", a, b)).into()),
        }
    }
}

impl std::cmp::PartialOrd for Expression {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        use Expression::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a.partial_cmp(b),
            (Integer(a), Float(b)) => (*a as f64).partial_cmp(b),
            (Float(a), Integer(b)) => a.partial_cmp(&(*b as f64)),
            (Float(a), Float(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl std::cmp::PartialEq for Expression {
    fn eq(&self, rhs: &Self) -> bool {
        use Expression::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a == b,
            (Integer(a), Float(b)) => (*a as f64) == *b,
            (Float(a), Integer(b)) => *a == (*b as f64),
            (Float(a), Float(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Char(a), Char(b)) => a == b,
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            (Pair(a_car, a_cdr), Pair(b_car, b_cdr)) => a_car == b_car && a_cdr == b_cdr,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Procedure<E> {
    body: Rc<Expression>,
    params: Rc<Expression>,
    env: E,
    name: Symbol,
}

impl Procedure<EnvRef> {
    pub fn new(params: Rc<Expression>, body: Rc<Expression>, env: EnvRef) -> Self {
        Procedure {
            body,
            params,
            env,
            name: symbol::GREEK_LAMBDA,
        }
    }

    pub fn build(signature: Expression, body: Expression, env: &EnvRef) -> Result<Self> {
        Ok(Procedure::new(
            Rc::new(signature),
            Rc::new(body),
            env.clone(),
        ))
    }

    pub fn rename(mut self, name: Symbol) -> Self {
        self.name = name;
        self
    }

    pub fn env(&self) -> &EnvRef {
        &self.env
    }

    pub fn new_local_env(&self, args: Expression) -> Result<EnvRef> {
        let mut env = Environment::new_child(self.env.clone(), self.clone());
        env.set_vars(self.params_ex(), args)?;
        Ok(env.into())
    }

    pub fn body_ex(&self) -> Expression {
        (*self.body).clone()
    }

    pub fn params_ex(&self) -> Expression {
        (*self.params).clone()
    }

    pub fn eqv(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.body, &other.body)
    }

    pub fn equal(&self, other: &Self) -> bool {
        self.body.equal(&other.body) && self.params.equal(&other.params)
    }

    pub fn notify_call(&self, _called_env: &EnvRef, _calling_env: Option<&EnvRef>) {
        /*print!("calling {} ", self.name);
        match calling_env {
            Some(pe) => match pe.borrow().current_procedure() {
                Some(proc) => println!("from {}", proc.name),
                None => println!("from the root"),
            }
            None => println!("from an unknown location"),
        }*/
    }
}

impl From<Procedure<EnvRef>> for Procedure<EnvWeak> {
    fn from(proc: Procedure<EnvRef>) -> Self {
        Procedure {
            body: proc.body,
            params: proc.params,
            env: Rc::downgrade(&proc.env),
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

pub struct ListIterator<'a> {
    next_pair: &'a Expression,
}

impl<'a> ListIterator<'a> {
    pub fn from_expression(expr: &'a Expression) -> Result<Self> {
        Ok(ListIterator { next_pair: expr })
    }

    pub fn next_expr(&mut self) -> Result<Option<&'a Expression>> {
        self.next().transpose()
    }

    pub fn tail(&self) -> Result<&'a Expression> {
        match self.next_pair {
            Expression::Nil => Ok(self.next_pair),
            Expression::Pair(_, cdr) => Ok(&**cdr),
            _ => Err(ErrorKind::TypeError("not a list".into()))?,
        }
    }
}

impl<'a> Iterator for ListIterator<'a> {
    type Item = Result<&'a Expression>;
    fn next(&mut self) -> Option<Self::Item> {
        let (car, cdr) = match self.next_pair {
            Expression::Nil => return None,
            Expression::Pair(car, cdr) => (&**car, &**cdr),
            _ => return Some(Err(ErrorKind::TypeError("not a list".into()).into())),
        };

        self.next_pair = cdr;
        Some(Ok(car))
    }
}

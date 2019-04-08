use crate::environment::{EnvRef, EnvWeak, Environment};
use crate::errors::*;
use std::rc::Rc;

pub type List = Expression;
pub type Args = Expression;
pub type Symbol = String;

#[derive(Clone)]
pub enum Expression {
    Undefined,
    Nil,
    Symbol(Symbol),
    String(String),
    Integer(i64),
    Float(f64),
    True,
    False,
    Pair(Rc<(Expression)>, Rc<(Expression)>),
    Procedure(Procedure),
    Native(fn(Args) -> Result<Expression>),
    // Yes, it's possible to make functions take arguments is iterators, but this introduces considerable complexity
    // Also, the scheme standard expects all arguments to be evaluated before execution anyway...
    //Native(fn(&mut dyn Iterator<Item=Result<Expression>>) -> Result<Expression>),
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

        Expression::Symbol(s.to_string())
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
            _ => Err(ErrorKind::TypeError("not a pair".into()))?,
        }
    }

    pub fn cons_rc(car: Rc<Expression>, cdr: Rc<Expression>) -> Self {
        Expression::Pair(car, cdr)
    }

    pub fn decons_rc(&self) -> Result<(&Rc<Expression>, &Rc<Expression>)> {
        match self {
            Expression::Pair(car, cdr) => Ok((car, cdr)),
            _ => Err(ErrorKind::TypeError("not a pair".into()))?,
        }
    }

    pub fn car(&self) -> Result<&Expression> {
        match self {
            Expression::Pair(car, _) => Ok(car),
            _ => Err(ErrorKind::TypeError("not a pair".into()))?,
        }
    }

    pub fn cdr(&self) -> Result<&Expression> {
        match self {
            Expression::Pair(_, cdr) => Ok(cdr),
            _ => Err(ErrorKind::TypeError("not a pair".into()))?,
        }
    }

    pub fn car_mut(&mut self) -> Result<&mut Expression> {
        match self {
            Expression::Pair(car, _) => {
                Ok(Rc::get_mut(car).expect("mutable reference must be unique"))
            }
            _ => Err(ErrorKind::TypeError("not a pair".into()))?,
        }
    }

    pub fn cdr_mut(&mut self) -> Result<&mut Expression> {
        match self {
            Expression::Pair(_, cdr) => {
                Ok(Rc::get_mut(cdr).expect("mutable reference must be unique"))
            }
            _ => Err(ErrorKind::TypeError("not a pair".into()))?,
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

    pub fn map_list<F: Fn(&Expression) -> Result<Expression>>(
        &self,
        func: F,
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

    pub fn is_symbol(&self) -> bool {
        match self {
            Expression::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_named_symbol(&self, name: &str) -> bool {
        match self {
            Expression::Symbol(s) => s == name,
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

    pub fn try_as_integer(&self) -> Result<i64> {
        match self {
            Expression::Integer(i) => Ok(*i),
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
        self.iter_list()?.map(|r| r.map(|x| x.clone())).collect()
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
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Undefined => write!(f, "#<unspecified>"),
            Expression::Nil => write!(f, "()"),
            Expression::Symbol(s) => write!(f, "{}", s),
            Expression::String(s) => write!(f, "{:?}", s),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(i) => write!(f, "{}", i),
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
            Expression::Native(_) => write!(f, "<native>"),
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
            Expression::Symbol(s) => write!(f, "{}", s),
            Expression::String(s) => write!(f, "{}", s),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(i) => write!(f, "{}", i),
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
            Expression::Native(_) => write!(f, "<native>"),
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
        Expression::String(s.into())
    }
}

impl From<String> for Expression {
    fn from(s: String) -> Self {
        Expression::String(s)
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
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            (Pair(a_car, a_cdr), Pair(b_car, b_cdr)) => a_car == b_car && a_cdr == b_cdr,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Procedure {
    pub body: Rc<Expression>,
    pub params: Rc<Expression>,
    pub env: EnvRef,
}
/*
impl Drop for Procedure {
    fn drop(&mut self) {
        println!("Dropping proceduce {:?}", self.name);
    }
}

impl Clone for Procedure {
    fn clone(&self) -> Self {
        println!("Cloning proceduce {:?}", self.name);
        Procedure {
            name: self.name.clone(),
            body: self.body.clone(),
            params: self.params.clone(),
            env: self.env.clone(),
        }
    }
}*/

impl Procedure {
    pub fn new(params: Rc<Expression>, body: Rc<Expression>, env: EnvRef) -> Self {
        Procedure { body, params, env }
    }

    pub fn build(signature: Expression, body: Expression, env: &EnvRef) -> Result<Self> {
        Ok(Procedure {
            body: Rc::new(body),
            params: Rc::new(signature),
            env: env.clone(),
        })
    }

    pub fn env(&self) -> EnvRef {
        self.env.clone()
        //.upgrade()
        //.expect("method's environment has been dropped")
    }

    pub fn new_local_env(&self, args: Expression) -> Result<EnvRef> {
        let mut env = Environment::new_child(self.env());
        env.set_vars(self.params_ex(), args)?;
        Ok(env.into())
    }

    pub fn body_ex(&self) -> Expression {
        (*self.body).clone()
    }

    pub fn params_ex(&self) -> Expression {
        (*self.params).clone()
    }
}

/// A procedure that stores only a weak reference to it's environment
///
/// Used to store procedures inside environments without Rc cycles.
#[derive(Debug, Clone)]
pub struct WeakProcedure {
    pub body: Rc<Expression>,
    pub params: Rc<Expression>,
    pub env: EnvWeak,
}

impl From<Procedure> for WeakProcedure {
    fn from(proc: Procedure) -> Self {
        WeakProcedure {
            body: proc.body,
            params: proc.params,
            env: Rc::downgrade(&proc.env),
        }
    }
}

impl From<WeakProcedure> for Procedure {
    fn from(proc: WeakProcedure) -> Self {
        Procedure {
            env: proc
                .env
                .upgrade()
                .unwrap_or_else(|| panic!("procedure's environment has been dropped")),
            body: proc.body,
            params: proc.params,
        }
    }
}

impl From<WeakProcedure> for Expression {
    fn from(proc: WeakProcedure) -> Self {
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

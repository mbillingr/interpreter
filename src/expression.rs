use crate::environment::{EnvRef, EnvWeak, Environment};
use crate::errors::*;
use std::rc::Rc;

pub type List = Vec<Expression>;
pub type Args = List;
pub type Symbol = String;

#[derive(Clone)]
pub enum Expression {
    Undefined,
    Nil,
    /// for now use a Vec... maybe change to linked list in the future?
    List(List),
    Symbol(Symbol),
    String(String),
    Integer(i64),
    Float(f64),
    True,
    False,
    Procedure(Procedure),
    Native(fn(Args) -> Result<Expression>),
    // Yes, it's possible to make functions take arguments is iterators, but this introduces considerable complexity
    //Native(fn(&mut dyn Iterator<Item=Result<Expression>>) -> Result<Expression>),
}

impl Expression {
    pub fn zero() -> Self {
        Expression::Integer(0)
    }
    pub fn one() -> Self {
        Expression::Integer(1)
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

    pub fn is_list(&self) -> bool {
        match self {
            Expression::List(_) => true,
            _ => false,
        }
    }

    pub fn try_as_integer(&self) -> Result<i64> {
        match self {
            Expression::Integer(i) => Ok(*i),
            _ => Err(ErrorKind::TypeError(format!("{} is not an integer.", self)).into()),
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

    pub fn try_into_list(self) -> Result<List> {
        match self {
            Expression::List(l) => Ok(l),
            _ => Err(ErrorKind::TypeError(format!("{} is not a List.", self)).into()),
        }
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
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Undefined => write!(f, "#<unspecified>"),
            Expression::Nil => write!(f, "()"),
            Expression::List(l) => {
                let tmp: Vec<_> = l.iter().map(|item| format!("{}", item)).collect();
                write!(f, "({})", tmp.join(" "))
            }
            Expression::Symbol(s) => write!(f, "{}", s),
            Expression::String(s) => write!(f, "{:?}", s),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(i) => write!(f, "{}", i),
            Expression::True => write!(f, "#t"),
            Expression::False => write!(f, "#f"),
            Expression::Procedure(p) => write!(f, "#<procedure {} {}>", p.name_ex(), p.params_ex()),
            Expression::Native(_) => write!(f, "<native>"),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Undefined => write!(f, "#<unspecified>"),
            Expression::Nil => write!(f, "()"),
            Expression::List(l) => {
                let tmp: Vec<_> = l.iter().map(|item| format!("{}", item)).collect();
                write!(f, "({})", tmp.join(" "))
            }
            Expression::Symbol(s) => write!(f, "{}", s),
            Expression::String(s) => write!(f, "{}", s),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(i) => write!(f, "{}", i),
            Expression::True => write!(f, "#t"),
            Expression::False => write!(f, "#f"),
            Expression::Procedure(p) => write!(f, "#<procedure {} {}>", p.name_ex(), p.params_ex()),
            Expression::Native(_) => write!(f, "<native>"),
        }
    }
}

impl From<&str> for Expression {
    fn from(s: &str) -> Self {
        match s {
            "#t" => return Expression::True,
            "#f" => return Expression::False,
            _ => {}
        }

        if let Ok(i) = s.parse() {
            return Expression::Integer(i);
        }

        if let Ok(f) = s.parse() {
            return Expression::Float(f);
        }

        Expression::Symbol(s.into())
    }
}

impl From<String> for Expression {
    fn from(s: String) -> Self {
        match s.as_str() {
            "#t" => return Expression::True,
            "#f" => return Expression::False,
            _ => {}
        }

        if let Ok(i) = s.parse() {
            return Expression::Integer(i);
        }

        if let Ok(f) = s.parse() {
            return Expression::Float(f);
        }

        Expression::Symbol(s)
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

impl From<List> for Expression {
    fn from(list: List) -> Self {
        Expression::List(list)
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
            (True, True) => true,
            (False, False) => true,
            (Nil, Nil) => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct Procedure {
    pub name: Option<Symbol>,
    pub body: Box<Expression>,
    pub params: List,
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
    pub fn build(
        name: Option<Symbol>,
        signature: List,
        body: Expression,
        env: &EnvRef,
    ) -> Result<Self> {
        Ok(Procedure {
            name,
            body: Box::new(body),
            params: signature,
            env: env.clone(),
        })
    }

    pub fn env(&self) -> EnvRef {
        self.env.clone()
        //.upgrade()
        //.expect("method's environment has been dropped")
    }

    pub fn new_local_env(&self, args: Vec<Expression>) -> Result<EnvRef> {
        let mut env = Environment::new_child(self.env());
        env.set_vars(self.params.as_slice(), args)?;
        Ok(env.into())
    }

    pub fn name_ex(&self) -> Expression {
        Expression::Symbol(self.name.clone().unwrap_or_else(|| format!("{:p}", self)))
    }

    pub fn body_ex(&self) -> Expression {
        (*self.body).clone()
    }

    pub fn params_ex(&self) -> Expression {
        Expression::List(self.params.clone())
    }
}

/// A procedure that stores only a weak reference to it's environment
///
/// Used to store procedures inside environments without Rc cycles.
#[derive(Debug, Clone)]
pub struct WeakProcedure {
    pub name: Option<Symbol>,
    pub body: Box<Expression>,
    pub params: List,
    pub env: EnvWeak,
}

impl From<Procedure> for WeakProcedure {
    fn from(proc: Procedure) -> Self {
        WeakProcedure {
            name: proc.name,
            body: proc.body,
            params: proc.params,
            env: Rc::downgrade(&proc.env),
        }
    }
}

impl From<WeakProcedure> for Procedure {
    fn from(proc: WeakProcedure) -> Self {
        Procedure {
            env: proc.env.upgrade().unwrap_or_else(|| {
                panic!("procedure {:?}'s environment has been dropped", proc.name)
            }),
            name: proc.name,
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

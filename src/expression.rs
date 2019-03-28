use crate::environment::EnvRef;
use crate::errors::*;

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

    pub fn expect_str(&self) -> Result<&str> {
        match self {
            Expression::String(s) => Ok(s),
            _ => Err(ErrorKind::TypeError(format!("{} is not a String.", self)).into()),
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

        return Expression::Symbol(s);
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

#[derive(Clone)]
pub struct Procedure {
    pub name: Option<Symbol>,
    pub body: List,
    pub params: List,
    pub env: EnvRef,
}

impl Procedure {
    pub fn name_ex(&self) -> Expression {
        Expression::Symbol(self.name.clone().unwrap_or(format!("{:p}", self)))
    }

    pub fn body_ex(&self) -> Expression {
        Expression::List(self.body.clone())
    }

    pub fn params_ex(&self) -> Expression {
        Expression::List(self.params.clone())
    }
}

use crate::errors::{ErrorKind, Result};
use crate::integer::Int;
use rand::{thread_rng, Rng};

#[derive(Clone, Debug)]
pub enum Number {
    Integer(Int),
    Float(f64),
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Number::Integer(i) => write!(f, "{}", i),
            Number::Float(x) => write!(f, "{}", x),
        }
    }
}

macro_rules! impl_from {
    ($t:ty, $variant:ident) => {
        impl From<$t> for Number {
            fn from(x: $t) -> Self {
                Number::$variant(x.into())
            }
        }
    };
}

impl_from!(Int, Integer);
impl_from!(i32, Integer);
impl_from!(i64, Integer);
impl_from!(i128, Integer);
impl_from!(u128, Integer);
impl_from!(f32, Float);
impl_from!(f64, Float);

impl Number {
    pub fn zero() -> Self {
        Number::Integer(Int::zero())
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Number::Integer(_) => true,
            Number::Float(f) if *f == f.trunc() => true,
            _ => false,
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            Number::Integer(_) => true,
            _ => false,
        }
    }

    pub fn try_as_integer(&self) -> Option<&Int> {
        match self {
            Number::Integer(i) => Some(i),
            _ => None,
        }
    }

    pub fn to_float(&self) -> f64 {
        match self {
            Number::Integer(i) => i.to_float(),
            Number::Float(f) => *f,
        }
    }

    pub fn eqv(&self, other: &Self) -> bool {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => a == b,
            (Float(a), Float(b)) => *a == *b,
            _ => false,
        }
    }

    pub fn round(&self) -> Self {
        match self {
            Number::Integer(_) => self.clone(),
            Number::Float(f) => Int::from_f64(f.round())
                .map(Number::Integer)
                .unwrap_or(Number::Float(f.round())),
        }
    }

    pub fn truncate_quotient(&self, other: &Self) -> Result<Self> {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a.div(b))),
            (Integer(a), Float(b)) => {
                if other.is_integer() {
                    Ok(Float(a.to_float() / b))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", b)).into())
                }
            }
            (Float(a), Integer(b)) => {
                if self.is_integer() {
                    Ok(Float(a / b.to_float()))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", a)).into())
                }
            }
            (Float(a), Float(b)) if self.is_integer() && other.is_integer() => Ok(Float(a / b)),
            (a, b) => Err(ErrorKind::TypeError(format!("not integer: {}, {}", a, b)).into()),
        }
    }

    pub fn truncate_remainder(&self, other: &Self) -> Result<Self> {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a.rem(b))),
            (Integer(a), Float(b)) => {
                if other.is_integer() {
                    Ok(Float(a.to_float() % b))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", b)).into())
                }
            }
            (Float(a), Integer(b)) => {
                if self.is_integer() {
                    Ok(Float(a % b.to_float()))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", a)).into())
                }
            }
            (Float(a), Float(b)) if self.is_integer() && other.is_integer() => Ok(Float(a % b)),
            (a, b) => Err(ErrorKind::TypeError(format!("not integer: {}, {}", a, b)).into()),
        }
    }

    pub fn rand_range(lo: &Number, hi: &Number) -> Self {
        use Number::*;
        match (lo, hi) {
            (Integer(a), Integer(b)) => Integer(Int::rand_range(a, b)),
            (Float(a), Float(b)) => Float(thread_rng().gen_range(a, b)),
            (Integer(a), Float(_)) => Self::rand_range(&Float(a.to_float()), hi),
            (Float(_), Integer(b)) => Self::rand_range(lo, &Float(b.to_float())),
        }
    }
}

impl std::ops::Add for Number {
    type Output = Number;
    fn add(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a.add(&b)),
            (Integer(a), Float(b)) => Float(a.to_float() + b),
            (Float(a), Integer(b)) => Float(a + b.to_float()),
            (Float(a), Float(b)) => Float(a + b),
        }
    }
}

impl std::ops::Mul for Number {
    type Output = Number;
    fn mul(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a.mul(&b)),
            (Integer(a), Float(b)) => Float(a.to_float() * b),
            (Float(a), Integer(b)) => Float(a * b.to_float()),
            (Float(a), Float(b)) => Float(a * b),
        }
    }
}

impl std::ops::Sub for Number {
    type Output = Number;
    fn sub(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a.sub(&b)),
            (Integer(a), Float(b)) => Float(a.to_float() - b),
            (Float(a), Integer(b)) => Float(a - b.to_float()),
            (Float(a), Float(b)) => Float(a - b),
        }
    }
}

impl std::ops::Div for Number {
    type Output = Number;
    fn div(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Float(a.to_float() / b.to_float()),
            (Integer(a), Float(b)) => Float(a.to_float() / b),
            (Float(a), Integer(b)) => Float(a / b.to_float()),
            (Float(a), Float(b)) => Float(a / b),
        }
    }
}

impl std::ops::Rem for Number {
    type Output = Number;
    fn rem(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a.rem(&b)),
            (Integer(a), Float(b)) => Float(a.to_float() % b),
            (Float(a), Integer(b)) => Float(a % b.to_float()),
            (Float(a), Float(b)) => Float(a % b),
        }
    }
}

impl std::cmp::PartialOrd for Number {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        use Number::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a.partial_cmp(b),
            (Integer(a), Float(b)) => a.to_float().partial_cmp(b),
            (Float(a), Integer(b)) => a.partial_cmp(&b.to_float()),
            (Float(a), Float(b)) => a.partial_cmp(b),
        }
    }
}

impl std::cmp::PartialEq for Number {
    fn eq(&self, rhs: &Self) -> bool {
        use Number::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a == b,
            (Integer(a), Float(b)) => a.to_float() == *b,
            (Float(a), Integer(b)) => *a == b.to_float(),
            (Float(a), Float(b)) => a == b,
        }
    }
}

impl std::convert::TryFrom<&Number> for Int {
    type Error = crate::errors::Error;

    fn try_from(x: &Number) -> Result<Int> {
        match x {
            Number::Integer(i) => Ok(i.clone()),
            _ => Err(ErrorKind::TypeError(format!("Expected integer: {:?}", x)).into()),
        }
    }
}

impl std::convert::TryFrom<&Number> for f64 {
    type Error = crate::errors::Error;

    fn try_from(x: &Number) -> Result<f64> {
        match x {
            Number::Float(f) => Ok(*f),
            _ => Err(ErrorKind::TypeError(format!("Expected fleat: {:?}", x)).into()),
        }
    }
}

impl std::convert::TryFrom<&Number> for usize {
    type Error = crate::errors::Error;

    fn try_from(x: &Number) -> Result<usize> {
        match x {
            Number::Integer(i) if *i >= Int::from(0) => i.to_usize().ok_or_else(|| {
                ErrorKind::TypeError("Integer out of range for usize".to_owned()).into()
            }),
            _ => Err(ErrorKind::TypeError(format!("Expected positive integer: {:?}", x)).into()),
        }
    }
}

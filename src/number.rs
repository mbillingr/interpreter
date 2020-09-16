use crate::errors::{ErrorKind, Result};
use crate::integer::Int;
use num_traits::{FromPrimitive, One, Zero};
use rand::{thread_rng, Rng};
use std::ops::{Add, Div, Mul, Rem, Sub};

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

impl From<Int> for Number {
    fn from(n: Int) -> Self {
        Number::Integer(n)
    }
}

impl From<i64> for Number {
    fn from(n: i64) -> Self {
        Number::from_i64(n).unwrap()
    }
}

impl From<f64> for Number {
    fn from(n: f64) -> Self {
        Number::from_f64(n).unwrap()
    }
}

impl FromPrimitive for Number {
    fn from_i64(n: i64) -> Option<Self> {
        Int::from_i64(n).map(Number::Integer)
    }

    fn from_i128(n: i128) -> Option<Self> {
        Int::from_i128(n).map(Number::Integer)
    }

    fn from_u64(n: u64) -> Option<Self> {
        Int::from_u64(n).map(Number::Integer)
    }

    fn from_u128(n: u128) -> Option<Self> {
        Int::from_u128(n).map(Number::Integer)
    }

    fn from_f32(n: f32) -> Option<Self> {
        Some(Number::Float(n as f64))
    }

    fn from_f64(n: f64) -> Option<Self> {
        Some(Number::Float(n))
    }
}

impl Number {
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
            (Integer(a), Integer(b)) => Ok(Integer(a / b)),
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

impl Zero for Number {
    fn zero() -> Self {
        Number::Integer(Int::zero())
    }

    fn is_zero(&self) -> bool {
        match self {
            Number::Integer(n) => n.is_zero(),
            Number::Float(x) => x.is_zero(),
        }
    }
}

impl One for Number {
    fn one() -> Self {
        Number::Integer(Int::one())
    }
}

impl Add for Number {
    type Output = Number;
    fn add(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a + b),
            (Integer(a), Float(b)) => Float(a.to_float() + b),
            (Float(a), Integer(b)) => Float(a + b.to_float()),
            (Float(a), Float(b)) => Float(a + b),
        }
    }
}

impl Mul for Number {
    type Output = Number;
    fn mul(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a * b),
            (Integer(a), Float(b)) => Float(a.to_float() * b),
            (Float(a), Integer(b)) => Float(a * b.to_float()),
            (Float(a), Float(b)) => Float(a * b),
        }
    }
}

impl Sub for Number {
    type Output = Number;
    fn sub(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a - b),
            (Integer(a), Float(b)) => Float(a.to_float() - b),
            (Float(a), Integer(b)) => Float(a - b.to_float()),
            (Float(a), Float(b)) => Float(a - b),
        }
    }
}

impl Div for Number {
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

impl Rem for Number {
    type Output = Number;
    fn rem(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a % b),
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

use crate::errors::{ErrorKind, Result};
use crate::integer::{rand_range, Int};
#[cfg(feature = "bigint")]
use num_rational::BigRational as Ratio;
#[cfg(not(feature = "bigint"))]
use num_rational::Rational64 as Ratio;
use num_traits::{FromPrimitive, One, ToPrimitive, Zero};
use rand::{thread_rng, Rng};
use std::ops::{Add, Div, Mul, Rem, Sub};

#[derive(Clone, Debug)]
pub enum Number {
    Integer(Int),
    Rational(Ratio),
    Float(f64),
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Number::Integer(i) => write!(f, "{}", i),
            Number::Rational(r) => write!(f, "{}", r),
            Number::Float(x) => write!(f, "{}", x),
        }
    }
}

impl From<Int> for Number {
    fn from(n: Int) -> Self {
        Number::Integer(n)
    }
}

impl From<i32> for Number {
    fn from(n: i32) -> Self {
        Number::from_i32(n).unwrap()
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

impl ToPrimitive for Number {
    fn to_i64(&self) -> Option<i64> {
        match self {
            Number::Integer(i) => i.to_i64(),
            Number::Rational(r) => r.to_i64(),
            Number::Float(f) => f.to_i64(),
        }
    }

    fn to_i128(&self) -> Option<i128> {
        match self {
            Number::Integer(i) => i.to_i128(),
            Number::Rational(r) => r.to_i128(),
            Number::Float(f) => f.to_i128(),
        }
    }

    fn to_u64(&self) -> Option<u64> {
        match self {
            Number::Integer(i) => i.to_u64(),
            Number::Rational(r) => r.to_u64(),
            Number::Float(f) => f.to_u64(),
        }
    }

    fn to_u128(&self) -> Option<u128> {
        match self {
            Number::Integer(i) => i.to_u128(),
            Number::Rational(r) => r.to_u128(),
            Number::Float(f) => f.to_u128(),
        }
    }

    fn to_f32(&self) -> Option<f32> {
        match self {
            Number::Integer(i) => i.to_f32(),
            Number::Rational(r) => r.to_f32(),
            Number::Float(f) => f.to_f32(),
        }
    }

    fn to_f64(&self) -> Option<f64> {
        match self {
            Number::Integer(i) => i.to_f64(),
            Number::Rational(r) => r.to_f64(),
            Number::Float(f) => Some(*f),
        }
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

    pub fn to_integer(&self) -> Option<Int> {
        match self {
            Number::Integer(i) => Some(i.clone()),
            Number::Rational(r) if r.is_integer() => Some(r.to_integer()),
            Number::Float(f) if *f == f.trunc() => Int::from_f64(*f),
            _ => None,
        }
    }

    pub fn to_float(&self) -> f64 {
        match self {
            Number::Integer(i) => i.to_f64().unwrap(),
            Number::Rational(r) => r.to_f64().unwrap(),
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
            Number::Rational(r) => Number::Integer(r.round().to_integer()),
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
                    Ok(Float(a.to_f64().unwrap() / b))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", b)).into())
                }
            }
            (Float(a), Integer(b)) => {
                if self.is_integer() {
                    Ok(Float(a / b.to_f64().unwrap()))
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
                    Ok(Float(a.to_f64().unwrap() % b))
                } else {
                    Err(ErrorKind::TypeError(format!("not an integer: {}", b)).into())
                }
            }
            (Float(a), Integer(b)) => {
                if self.is_integer() {
                    Ok(Float(a % b.to_f64().unwrap()))
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
            (Integer(a), Integer(b)) => Integer(rand_range(a, b)),
            (Float(a), Float(b)) => Float(thread_rng().gen_range(a, b)),
            (Integer(a), Float(_)) => Self::rand_range(&Float(a.to_f64().unwrap()), hi),
            (Float(_), Integer(b)) => Self::rand_range(lo, &Float(b.to_f64().unwrap())),
            (Rational(a), _) => Self::rand_range(&Float(a.to_f64().unwrap()), hi),
            (_, Rational(b)) => Self::rand_range(lo, &Float(b.to_f64().unwrap())),
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
            Number::Rational(r) => r.is_zero(),
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
            (Float(a), Float(b)) => Float(a + b),
            (Rational(a), Rational(b)) => Rational(a + b),

            (Integer(a), Rational(b)) => Rational(Ratio::from_integer(a) + b),
            (Integer(a), Float(b)) => Float(a.to_f64().unwrap() + b),

            (Float(a), Integer(b)) => Float(a + b.to_f64().unwrap()),
            (Float(a), Rational(b)) => Float(a + b.to_f64().unwrap()),

            (Rational(a), Integer(b)) => Rational(a + Ratio::from_integer(b)),
            (Rational(a), Float(b)) => Float(a.to_f64().unwrap() + b),
        }
    }
}

impl Mul for Number {
    type Output = Number;
    fn mul(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a * b),
            (Float(a), Float(b)) => Float(a * b),
            (Rational(a), Rational(b)) => Rational(a * b),

            (Integer(a), Rational(b)) => Rational(Ratio::from_integer(a) * b),
            (Integer(a), Float(b)) => Float(a.to_f64().unwrap() * b),

            (Float(a), Integer(b)) => Float(a * b.to_f64().unwrap()),
            (Float(a), Rational(b)) => Float(a * b.to_f64().unwrap()),

            (Rational(a), Integer(b)) => Rational(a * Ratio::from_integer(b)),
            (Rational(a), Float(b)) => Float(a.to_f64().unwrap() * b),
        }
    }
}

impl Sub for Number {
    type Output = Number;
    fn sub(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a - b),
            (Float(a), Float(b)) => Float(a - b),
            (Rational(a), Rational(b)) => Rational(a - b),

            (Integer(a), Rational(b)) => Rational(Ratio::from_integer(a) - b),
            (Integer(a), Float(b)) => Float(a.to_f64().unwrap() - b),

            (Float(a), Integer(b)) => Float(a - b.to_f64().unwrap()),
            (Float(a), Rational(b)) => Float(a - b.to_f64().unwrap()),

            (Rational(a), Integer(b)) => Rational(a - Ratio::from_integer(b)),
            (Rational(a), Float(b)) => Float(a.to_f64().unwrap() - b),
        }
    }
}

impl Div for Number {
    type Output = Number;
    fn div(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Rational(Ratio::from_integer(a) / Ratio::from_integer(b)),
            (Float(a), Float(b)) => Float(a / b),
            (Rational(a), Rational(b)) => Rational(a / b),

            (Integer(a), Rational(b)) => Rational(Ratio::from_integer(a) / b),
            (Integer(a), Float(b)) => Float(a.to_f64().unwrap() / b),

            (Float(a), Integer(b)) => Float(a / b.to_f64().unwrap()),
            (Float(a), Rational(b)) => Float(a / b.to_f64().unwrap()),

            (Rational(a), Integer(b)) => Rational(a / Ratio::from_integer(b)),
            (Rational(a), Float(b)) => Float(a.to_f64().unwrap() / b),
        }
    }
}

impl Rem for Number {
    type Output = Number;
    fn rem(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a % b),
            (Float(a), Float(b)) => Float(a % b),
            (Rational(a), Rational(b)) => Rational(a % b),

            (Integer(a), Rational(b)) => Rational(Ratio::from_integer(a) % b),
            (Integer(a), Float(b)) => Float(a.to_f64().unwrap() % b),

            (Float(a), Integer(b)) => Float(a % b.to_f64().unwrap()),
            (Float(a), Rational(b)) => Float(a % b.to_f64().unwrap()),

            (Rational(a), Integer(b)) => Rational(a % Ratio::from_integer(b)),
            (Rational(a), Float(b)) => Float(a.to_f64().unwrap() % b),
        }
    }
}

impl std::cmp::PartialOrd for Number {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        use Number::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a.partial_cmp(b),
            (Float(a), Float(b)) => a.partial_cmp(b),
            (Rational(a), Rational(b)) => a.partial_cmp(b),

            (Integer(a), Rational(b)) => Ratio::from_integer(a.clone()).partial_cmp(b),
            (Integer(a), Float(b)) => a.to_f64().unwrap().partial_cmp(b),

            (Float(a), Integer(b)) => a.partial_cmp(&b.to_f64().unwrap()),
            (Float(a), Rational(b)) => a.partial_cmp(&b.to_f64().unwrap()),

            (Rational(a), Integer(b)) => a.partial_cmp(&Ratio::from_integer(b.clone())),
            (Rational(a), Float(b)) => a.to_f64().unwrap().partial_cmp(&b),
        }
    }
}

impl std::cmp::PartialEq for Number {
    fn eq(&self, rhs: &Self) -> bool {
        use Number::*;
        match (self, rhs) {
            (Integer(a), Integer(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Rational(a), Rational(b)) => a == b,

            (Integer(a), Rational(b)) if b.is_integer() => a == &b.to_integer(),
            (Integer(_), Rational(_)) => false,
            (Integer(a), Float(b)) => a.to_f64().unwrap() == *b,

            (Float(a), Integer(b)) => *a == b.to_f64().unwrap(),
            (Float(a), Rational(b)) => a == &b.to_f64().unwrap(),

            (Rational(a), Integer(b)) if a.is_integer() => &a.to_integer() == b,
            (Rational(_), Integer(_)) => false,
            (Rational(a), Float(b)) => a.to_f64().unwrap() == *b,
        }
    }
}
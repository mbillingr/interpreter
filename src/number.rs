use crate::errors::{ErrorKind, Result};
use crate::integer::{rand_range, Int};
use num_complex::Complex64;
#[cfg(feature = "bigint")]
use num_rational::BigRational as Ratio;
#[cfg(not(feature = "bigint"))]
use num_rational::Rational64 as Ratio;
use num_traits::{FromPrimitive, One, Signed, ToPrimitive, Zero};
use rand::{thread_rng, Rng};
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::str::FromStr;

#[derive(Clone, Debug)]
pub enum Number {
    Integer(Int),
    Rational(Ratio),
    Float(f64),
    Complex(Complex64),
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Number::Integer(i) => write!(f, "{}", i),
            Number::Rational(r) => write!(f, "{}", r),
            Number::Float(x) => write!(f, "{:?}", x),
            Number::Complex(c) => write!(f, "{}", c),
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
            Number::Complex(c) => c.to_i64(),
        }
    }

    fn to_i128(&self) -> Option<i128> {
        match self {
            Number::Integer(i) => i.to_i128(),
            Number::Rational(r) => r.to_i128(),
            Number::Float(f) => f.to_i128(),
            Number::Complex(c) => c.to_i128(),
        }
    }

    fn to_u64(&self) -> Option<u64> {
        match self {
            Number::Integer(i) => i.to_u64(),
            Number::Rational(r) => r.to_u64(),
            Number::Float(f) => f.to_u64(),
            Number::Complex(c) => c.to_u64(),
        }
    }

    fn to_u128(&self) -> Option<u128> {
        match self {
            Number::Integer(i) => i.to_u128(),
            Number::Rational(r) => r.to_u128(),
            Number::Float(f) => f.to_u128(),
            Number::Complex(c) => c.to_u128(),
        }
    }

    fn to_f32(&self) -> Option<f32> {
        match self {
            Number::Integer(i) => i.to_f32(),
            Number::Rational(r) => r.to_f32(),
            Number::Float(f) => f.to_f32(),
            Number::Complex(c) => c.to_f32(),
        }
    }

    fn to_f64(&self) -> Option<f64> {
        match self {
            Number::Integer(i) => i.to_f64(),
            Number::Rational(r) => r.to_f64(),
            Number::Float(f) => Some(*f),
            Number::Complex(c) => c.to_f64(),
        }
    }
}

impl Number {
    pub fn complex(re: f64, im: f64) -> Self {
        Number::Complex(Complex64::new(re, im))
    }

    pub fn is_exact(&self) -> bool {
        match self {
            Number::Integer(_) => true,
            Number::Rational(_) => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Number::Integer(_) => true,
            Number::Rational(r) => r.is_integer(),
            Number::Float(f) if *f == f.trunc() => true,
            _ => false,
        }
    }

    pub fn is_rational(&self) -> bool {
        match self {
            Number::Integer(_) => true,
            Number::Rational(_) => true,
            Number::Float(f) if *f == f.trunc() => true, // is_rational() must return true if is_integer() would return true
            _ => false,
        }
    }

    pub fn is_real(&self) -> bool {
        match self {
            Number::Integer(_) => true,
            Number::Rational(_) => true,
            Number::Float(_) => true,
            Number::Complex(Complex64 { im, .. }) => *im == 0.0,
        }
    }

    pub fn is_complex(&self) -> bool {
        true
    }

    pub fn try_as_integer(&self) -> Option<&Int> {
        match self {
            Number::Integer(i) => Some(i),
            _ => None,
        }
    }

    pub fn try_into_integer(self) -> Option<Int> {
        match self {
            Number::Integer(i) => Some(i),
            _ => None,
        }
    }

    pub fn eqv(&self, other: &Self) -> bool {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) => a == b,
            (Rational(a), Rational(b)) => a == b,
            (Float(a), Float(b)) => *a == *b,
            (Complex(a), Complex(b)) => a == b,
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
            Number::Complex(Complex64 { re, im }) => {
                Number::Complex(Complex64::new(re.round(), im.round()))
            }
        }
    }

    pub fn to_complex(&self) -> Option<Self> {
        Some(Number::Complex(match self {
            Number::Complex(c) => c.clone(),
            Number::Float(f) => Complex64::from_f64(*f).unwrap(),
            Number::Rational(r) => r.to_f64().and_then(Complex64::from_f64).unwrap(),
            Number::Integer(i) => i.to_f64().and_then(Complex64::from_f64).unwrap(),
        }))
    }

    pub fn to_float(&self) -> Option<Self> {
        match self {
            Number::Float(f) => Some(*f),
            Number::Rational(r) => r.to_f64(),
            Number::Integer(i) => i.to_f64(),
            Number::Complex(c) => c.to_f64(),
        }
        .map(Number::Float)
    }

    pub fn to_rational(&self) -> Option<Self> {
        match self {
            Number::Rational(r) => Some(r.clone()),
            Number::Integer(i) => Some(Ratio::from_integer(i.clone())),
            Number::Float(f) if *f == f.trunc() => Ratio::from_f64(*f),
            Number::Float(_) => None,
            Number::Complex(c) => {
                return c
                    .to_f64()
                    .map(Number::Float)
                    .as_ref()
                    .and_then(Number::to_rational)
            }
        }
        .map(Number::Rational)
    }

    pub fn to_integer(&self) -> Option<Self> {
        match self {
            Number::Integer(i) => Some(i.clone()),
            Number::Rational(r) if r.is_integer() => Some(r.to_integer()),
            Number::Rational(_) => None,
            Number::Float(f) if *f == f.trunc() => Int::from_f64(*f),
            Number::Float(_) => None,
            Number::Complex(Complex64 { re, im }) if *im == 0.0 && *re == re.trunc() => {
                Int::from_f64(*re)
            }
            Number::Complex(_) => None,
        }
        .map(Number::Integer)
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
            (Complex(Complex64 { re: ar, im: ai }), Complex(Complex64 { re: br, im: bi })) => {
                let mut rng = thread_rng();
                let re = rng.gen_range(ar, br);
                let im = rng.gen_range(ai, bi);
                Complex(Complex64 { re, im })
            }

            (Float(a), Complex(_)) => {
                Self::rand_range(&Complex(Complex64::from_f64(*a).unwrap()), hi)
            }
            (Complex(_), Float(b)) => {
                Self::rand_range(lo, &Complex(Complex64::from_f64(*b).unwrap()))
            }

            (Rational(a), _) => Self::rand_range(&Float(a.to_f64().unwrap()), hi),
            (_, Rational(b)) => Self::rand_range(lo, &Float(b.to_f64().unwrap())),

            (Integer(a), _) => Self::rand_range(&Float(a.to_f64().unwrap()), hi),
            (_, Integer(b)) => Self::rand_range(lo, &Float(b.to_f64().unwrap())),
        }
    }

    pub fn abs(&self) -> Self {
        match self {
            Number::Integer(i) => Number::Integer(i.abs()),
            Number::Rational(r) => Number::Rational(r.abs()),
            Number::Float(f) => Number::Float(f.abs()),
            Number::Complex(c) => Number::Float(c.norm()),
        }
    }

    pub fn angle(&self) -> Self {
        match self {
            Number::Integer(_) | Number::Rational(_) | Number::Float(_) => Number::zero(),
            Number::Complex(c) => Number::Float(c.arg()),
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
            Number::Complex(c) => c.is_zero(),
        }
    }
}

impl One for Number {
    fn one() -> Self {
        Number::Integer(Int::one())
    }
}

macro_rules! impl_binop {
    ($trait:ident::$fn:ident, $op:tt) => {
        impl $trait for Number {
            type Output = Number;
            fn $fn(self, other: Self) -> Self::Output {
                use Number::*;
                match (self, other) {
                    (Integer(a), Integer(b)) => Integer(a $op b),
                    (Float(a), Float(b)) => Float(a $op b),
                    (Rational(a), Rational(b)) => Rational(a $op b),
                    (Complex(a), Complex(b)) => Complex(a $op b),

                    (a@Complex(_), b) => b.to_complex().map(|b| a $op b).unwrap(),
                    (a, b@Complex(_)) => a.to_complex().map(|a| a $op b).unwrap(),

                    (a@Float(_), b) => b.to_float().map(|b| a $op b).unwrap(),
                    (a, b@Float(_)) => a.to_float().map(|a| a $op b).unwrap(),

                    (a@Rational(_), b) => b.to_rational().map(|b| a $op b).unwrap(),
                    (a, b@Rational(_)) => a.to_rational().map(|a| a $op b).unwrap(),
                }
            }
        }
    }
}

impl_binop!(Add::add, +);
impl_binop!(Sub::sub, -);
impl_binop!(Mul::mul, *);
impl_binop!(Rem::rem, %);

impl Div for Number {
    type Output = Number;
    fn div(self, other: Self) -> Self::Output {
        use Number::*;
        match (self, other) {
            (Integer(a), Integer(b)) if b.is_zero() => {
                Float(a.to_f64().unwrap() / b.to_f64().unwrap())
            }
            (Integer(a), Integer(b)) => {
                let c = Ratio::from_integer(a) / Ratio::from_integer(b);
                if c.is_integer() {
                    Integer(c.to_integer())
                } else {
                    Rational(c)
                }
            }
            (Float(a), Float(b)) => Float(a / b),
            (Rational(a), Rational(b)) => Rational(a / b),
            (Complex(a), Complex(b)) => Complex(a / b),

            (a @ Complex(_), b) => b.to_complex().map(|b| a / b).unwrap(),
            (a, b @ Complex(_)) => a.to_complex().map(|a| a / b).unwrap(),

            (a @ Float(_), b) => b.to_float().map(|b| a / b).unwrap(),
            (a, b @ Float(_)) => a.to_float().map(|a| a / b).unwrap(),

            (a @ Rational(_), b) => b.to_rational().map(|b| a / b).unwrap(),
            (a, b @ Rational(_)) => a.to_rational().map(|a| a / b).unwrap(),
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

            // can't order complex numbers
            (Complex(_), _) | (_, Complex(_)) => None,

            (a @ Float(_), b) => b.to_float().and_then(|b| a.partial_cmp(&b)),
            (a, b @ Float(_)) => a.to_float().and_then(|a| a.partial_cmp(b)),

            (a @ Rational(_), b) => b.to_rational().and_then(|b| a.partial_cmp(&b)),
            (a, b @ Rational(_)) => a.to_rational().and_then(|a| a.partial_cmp(b)),
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
            (Complex(a), Complex(b)) => a == b,

            (a @ Complex(_), b) => b.to_complex().map(|b| a.eq(&b)).unwrap_or(false),
            (a, b @ Complex(_)) => a.to_complex().map(|a| a.eq(b)).unwrap_or(false),

            (a @ Float(_), b) => b.to_float().map(|b| a.eq(&b)).unwrap_or(false),
            (a, b @ Float(_)) => a.to_float().map(|a| a.eq(b)).unwrap_or(false),

            (a @ Rational(_), b) => b.to_rational().map(|b| a.eq(&b)).unwrap_or(false),
            (a, b @ Rational(_)) => a.to_rational().map(|a| a.eq(b)).unwrap_or(false),
        }
    }
}

impl FromStr for Number {
    type Err = ();
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        s.parse::<Int>()
            .map(Number::Integer)
            .or_else(|_| s.parse::<f64>().map(Number::Float))
            .or_else(|_| s.parse::<Ratio>().map(Number::Rational))
            .map_err(|_| ())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compare_float_to_int() {
        let x = Number::from(1.0);
        let y = Number::from(0);

        assert!(x > y)
    }
}

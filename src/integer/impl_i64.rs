use rand::thread_rng;
use rand::Rng;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Int(i64);

impl Int {
    pub fn zero() -> Self {
        Int(0)
    }

    pub fn one() -> Self {
        Int(1)
    }

    pub fn minus_one() -> Self {
        Int(-1)
    }

    pub fn from_u128(x: u128) -> Self {
        if x > i64::MAX as u128 {
            panic!("{} is too large for the built-in integer type", x)
        }
        Int(x as i64)
    }

    pub fn from_f64(x: f64) -> Option<Self> {
        if x > i64::MAX as f64 {
            None
        } else if x < i64::MIN as f64 {
            None
        } else {
            Some(Int(x as i64))
        }
    }

    pub fn add(&self, x: &Int) -> Self {
        Int(self.0.wrapping_add(x.0))
    }

    pub fn sub(&self, x: &Int) -> Self {
        Int(self.0.wrapping_sub(x.0))
    }

    pub fn mul(&self, x: &Int) -> Self {
        Int(self.0.wrapping_mul(x.0))
    }

    pub fn div(&self, x: &Int) -> Self {
        Int(self.0.wrapping_div(x.0))
    }

    pub fn rem(&self, x: &Int) -> Self {
        Int(self.0.wrapping_rem(x.0))
    }

    pub fn to_float(&self) -> f64 {
        self.0 as f64
    }

    pub fn to_usize(&self) -> Option<usize> {
        if self.0 < 0 {
            None
        } else {
            Some(self.0 as usize)
        }
    }

    pub fn rand_range(lo: &Int, hi: &Int) -> Self {
        Int(thread_rng().gen_range(&lo.0, &hi.0))
    }
}

impl<T> From<T> for Int
where
    T: Into<i64>,
{
    fn from(x: T) -> Self {
        Int(x.into())
    }
}

impl FromStr for Int {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Int(i64::from_str(s)?))
    }
}

impl std::fmt::Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Debug for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

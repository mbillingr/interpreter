use num_traits::{FromPrimitive, One, Zero};
use rand::thread_rng;
use rand::Rng;
use std::num::ParseIntError;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::str::FromStr;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Int(i64);

impl_numeric_traits!();

impl Int {
    pub fn new(n: i64) -> Self {
        Int(n)
    }

    pub fn as_inner(&self) -> &i64 {
        &self.0
    }

    pub fn into_inner(self) -> i64 {
        self.0
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

impl Zero for Int {
    fn zero() -> Self {
        Int(i64::zero())
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl One for Int {
    fn one() -> Self {
        Int(i64::one())
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

impl FromPrimitive for Int {
    fn from_i64(n: i64) -> Option<Self> {
        Some(Int(n))
    }

    fn from_u64(n: u64) -> Option<Self> {
        i64::from_u64(n).map(Int)
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

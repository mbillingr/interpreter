use num_bigint::{BigInt, ParseBigIntError, RandBigInt, Sign};
use num_traits::{FromPrimitive, One, Zero};
use rand::thread_rng;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::str::FromStr;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Int(BigInt);

impl_numeric_traits!();

impl Int {
    pub fn to_float(&self) -> f64 {
        let (sign, digits) = self.0.to_u32_digits();

        let mut f = 0.0;
        for d in digits.into_iter().rev() {
            f *= u32::MAX as f64;
            f += d as f64;
        }

        match sign {
            Sign::NoSign | Sign::Plus => f,
            Sign::Minus => -f,
        }
    }

    pub fn to_usize(&self) -> Option<usize> {
        let (sign, digits) = self.0.to_u32_digits();

        if let Sign::Minus = sign {
            return None;
        }

        if digits.len() > 2 {
            return None;
        }

        Some(digits[0] as usize + digits[1] as usize * u32::MAX as usize)
    }

    pub fn rand_range(lo: &Int, hi: &Int) -> Self {
        Int(thread_rng().gen_bigint_range(&lo.0, &hi.0))
    }
}

impl Zero for Int {
    fn zero() -> Self {
        Int(BigInt::zero())
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl One for Int {
    fn one() -> Self {
        Int(BigInt::one())
    }
}

impl FromPrimitive for Int {
    fn from_i64(n: i64) -> Option<Self> {
        BigInt::from_i64(n).map(Int)
    }

    fn from_i128(n: i128) -> Option<Self> {
        BigInt::from_i128(n).map(Int)
    }

    fn from_u64(n: u64) -> Option<Self> {
        BigInt::from_u64(n).map(Int)
    }

    fn from_u128(n: u128) -> Option<Self> {
        BigInt::from_u128(n).map(Int)
    }

    fn from_f32(n: f32) -> Option<Self> {
        BigInt::from_f32(n).map(Int)
    }

    fn from_f64(n: f64) -> Option<Self> {
        BigInt::from_f64(n).map(Int)
    }
}

impl<T> From<T> for Int
where
    T: Into<BigInt>,
{
    fn from(x: T) -> Self {
        Int(x.into())
    }
}

impl FromStr for Int {
    type Err = ParseBigIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Int(BigInt::from_str(s)?))
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

use num_bigint::{BigInt, ParseBigIntError, RandBigInt, Sign, ToBigInt};
use rand::thread_rng;
use std::str::FromStr;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Int(BigInt);

impl Int {
    pub fn zero() -> Self {
        Int(0.into())
    }

    pub fn one() -> Self {
        Int(1.into())
    }

    pub fn minus_one() -> Self {
        Int((-1).into())
    }

    pub fn from_u128(x: u128) -> Self {
        Int(x.into())
    }

    pub fn from_f64(x: f64) -> Option<Self> {
        x.to_bigint().map(Int)
    }

    pub fn add(&self, x: &Int) -> Self {
        Int(&self.0 + &x.0)
    }

    pub fn sub(&self, x: &Int) -> Self {
        Int(&self.0 - &x.0)
    }

    pub fn mul(&self, x: &Int) -> Self {
        Int(&self.0 * &x.0)
    }

    pub fn div(&self, x: &Int) -> Self {
        Int(&self.0 / &x.0)
    }

    pub fn rem(&self, x: &Int) -> Self {
        Int(&self.0 % &x.0)
    }

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

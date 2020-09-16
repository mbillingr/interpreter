macro_rules! impl_numeric_traits {
    () => {
        impl Add for Int {
            type Output = Int;

            fn add(self, rhs: Self) -> Self {
                Int(self.0 + rhs.0)
            }
        }

        impl Add<&Int> for &Int {
            type Output = Int;

            fn add(self, rhs: &Int) -> Int {
                Int(&self.0 + &rhs.0)
            }
        }

        impl Sub for Int {
            type Output = Int;

            fn sub(self, rhs: Self) -> Self {
                Int(self.0 - rhs.0)
            }
        }

        impl Sub<&Int> for &Int {
            type Output = Int;

            fn sub(self, rhs: &Int) -> Int {
                Int(&self.0 - &rhs.0)
            }
        }

        impl Mul for Int {
            type Output = Int;

            fn mul(self, rhs: Self) -> Self {
                Int(self.0 * rhs.0)
            }
        }

        impl Mul<&Int> for &Int {
            type Output = Int;

            fn mul(self, rhs: &Int) -> Int {
                Int(&self.0 * &rhs.0)
            }
        }

        impl Div for Int {
            type Output = Int;

            fn div(self, rhs: Self) -> Self {
                Int(self.0 / rhs.0)
            }
        }

        impl Div<&Int> for &Int {
            type Output = Int;

            fn div(self, rhs: &Int) -> Int {
                Int(&self.0 / &rhs.0)
            }
        }

        impl Rem for Int {
            type Output = Int;

            fn rem(self, rhs: Self) -> Self {
                Int(self.0 % rhs.0)
            }
        }

        impl Rem<&Int> for &Int {
            type Output = Int;

            fn rem(self, rhs: &Int) -> Int {
                Int(&self.0 % &rhs.0)
            }
        }
    };
}

#[cfg(feature = "bigint")]
mod impl_bigint;

#[cfg(not(feature = "bigint"))]
mod impl_i64;

#[cfg(feature = "bigint")]
pub use impl_bigint::Int;

#[cfg(not(feature = "bigint"))]
pub use impl_i64::Int;

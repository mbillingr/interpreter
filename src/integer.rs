#[cfg(feature = "bigint")]
mod impl_bigint;

#[cfg(not(feature = "bigint"))]
mod impl_i64;

#[cfg(feature = "bigint")]
pub use impl_bigint::*;

#[cfg(not(feature = "bigint"))]
pub use impl_i64::*;

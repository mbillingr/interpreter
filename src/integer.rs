#[cfg(feature = "bigint")]
mod impl_bigint;

#[cfg(not(feature = "bigint"))]
mod impl_i64;

#[cfg(feature = "bigint")]
pub use impl_bigint::Int;

#[cfg(not(feature = "bigint"))]
pub use impl_i64::Int;

// (define (fact n i) (if (= 0 n) i (fact (- n 1) (* n i))))
